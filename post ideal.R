# import functions, assumptions, setup configs
source("refined-code\\config.R")
source("refined-code\\functions.R")
cer_ann_due <- getCertainAnnuityDue()
source("refined-code\\Assumptions.R")
load("refined-code\\LE.RData")
r_spia <- read.csv("inputs\\SPIArate.csv")

FinaProd <- read.csv("Inputs\\Financial Products\\Financial Products.csv", header = TRUE)
MP <- read.csv("DataResults\\Post-Model Points 500 0521.csv", header = TRUE)

idealAssetV <- vector("numeric", nrow(MP))
for(j in 1 : 500){
    source("refined-code\\readData.R")
    
    phycon <- getPhycon(Gender,AgeRe,MaxAge,st)
    
    
    cashOutflow <- matrix(0, period, st)
    for(i in 1 : st)
        cashOutflow[min(which(phycon[, i] == 3)), i] <- Burial_cost
    
    cashOutflow[which(phycon == 1)] <- (a0 + a1 * Salary)+ (b0 + b1 * AgeRe) + (c0 + c1 * AgeRe + c2 * Salary)
    cashOutflow[which(phycon == 2)] <- ((a0 + a1 * Salary)+ (b0 + b1 * AgeRe) + (c0 + c1 * AgeRe + c2 * Salary)) * 2
    cashOutflow <- cashOutflow * as.vector(lev)
    cashOutflow[which(phycon == 1)] <- cashOutflow[which(phycon == 1)] + WL.premium
    cashOutflow[which(phycon == 1)] <- cashOutflow[which(phycon == 1)] + LTC.premium
    
    
    cashInflow <- matrix(0, period, st)
    cashInflow[which(phycon != 3)] <- ssb
    for(i in 1 : st)
        cashInflow[min(which(phycon[, i] == 3)), i] <- WL.benefit
    cashInflow[which(phycon == 2)] <- LTC.benefit
    
    need <- cashOutflow - cashInflow
    
    startingAsset <- vector("numeric", length = st)
    for(i in 1 : st){
        
    
        startingAsset[i] <- sum(need[, i]/cumprod(YCP_M[, i]))
        
    }
    rp <- 0 
    n <- 0
    idealAssetL <- 0
    idealAssetH <- max(startingAsset)
    idealAsset <- max(startingAsset) / 2
    
    netA <- vector(length=st)
    while(rp > 0.01 || rp < 0.008 && n<100){
        n <- n + 1
        
        for(i in 1 : st){
            deathYear <- max(which(need[, i] != 0))
            netA[i] <- (idealAsset - sum(need[, i] / cumprod(YCP_M[, i]))) * (cumprod(YCP_M[, i])[deathYear])
        }
        
        rp = abs(sum(netA[which(netA < 0)])) / sum(abs(netA))
        
        if(rp < 0.008){
            idealAssetH <- idealAsset
            idealAsset <- (idealAsset + idealAssetL) / 2
            
        } 
        
        if(rp > 0.01){
            idealAssetL <- idealAsset
            idealAsset <- (idealAsset + idealAssetH) / 2
        } 
        
    }
    idealAssetV[j] <- idealAsset
    print(paste(j, "----", idealAsset, "----", rp))
}
