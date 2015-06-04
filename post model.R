# import functions
source("refined-code\\config.R")
source("refined-code\\functions.R")
cer_ann_due <- getCertainAnnuityDue()
source("refined-code\\Assumptions.R")
load("refined-code\\LE.RData")
r_spia <- read.csv("inputs\\SPIArate.csv")
load("refined-code\\phycons.RData")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Model Points Test

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1st step: read the model points

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MP <- read.csv("DataResults\\Post MP 750 0603.csv", header = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2nd step: update inputs and do a loop
# test points format:
# columns
#    1            2            3              4            5            6                     
# point ID   Current Age   Current salary   Gender  Retirement Age   wholeLife insurance premium                                                      
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mp_optr_ideal <- matrix(0, 1, 9)
mp_optr_actual <- matrix(0, 1, 9)

for (j in 1 : 50){
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 1.2 Customer Information
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    AgePre <- MP[j, AgePreID]				# the current age				
    currentSalary <- MP[j,currentSalaryID]    
    Gender <- levels(factor(MP[j, GenderID]))
    AgeRe <- MP[j, AgeReID]    			# the retirement age
    life_ept <- get(paste("LE_",Gender,sep=""))[AgeRe-19]
    Salary <- currentSalary * (1 + r_wage) ^ (AgeRe - AgePre)
    ssb <- MP[j,ssbID]    				# social security benefit
    period <- MaxAge - AgeRe + 2
    
# annual social security benefits (retirement and disablement)
    
	
#	Age_Period<-c(AgePre:AgeRe)
    
    #Level_Sal:Annual Income Level & Salary: Current Salary & r_wage: wage inflation
#    Level_Sal<- currentSalary * ((1+r_wage)^(Age_Period - AgePre))
    
    #Average Annual Income 
#    a <- sum(Level_Sal)/((AgeRe-AgePre+1)*12)
    
    #PIA Accounts
#    ssb1 <- 0.09 * min(a, 816)
#    ssb2 <- 0.32 * max(0, min(a,4101) - 816)
#    ssb3 <- 0.15 * max(0, a - 4101)
#    ssb_t <- (ssb1 + ssb2 + ssb3) * 12
    
    #Adj after consider Retirement age 
#    if (AgeRe > 67){ssb <- ssb_t * (1+0.08* min(3,(AgeRe - 67)))}
#    if (AgeRe >= 64 & AgeRe < 67){ssb <- ssb_t * (1-0.0667 * (67 - AgeRe))}
#    if (AgeRe < 64){ssb <- ssb_t * (1-0.05 * min(5,(67 - AgeRe)))}
#    if (AgeRe == 67){ssb <- ssb_t}
    # evaluation period for post-retirement
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 1.3 Economic Assumptions
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # Stochastic Yield Curve for post-retirement
    mu_L <- Portfolio[1, 2]
    sigma_L <- Portfolio[2, 2] 
    YCP_L <- matrix(rlnorm((period * st), meanlog = mu_L, 
                           sdlog = sigma_L), period, st)
    YCP_L[1, ] <- 1
    
    mu_M <- Portfolio[1, 3]
    sigma_M <- Portfolio[2, 3]
    YCP_M <- matrix(rlnorm((period * st), meanlog = mu_M, 
                           sdlog = sigma_M), period, st)
    YCP_M[1, ] <- 1
    
    mu_A <- Portfolio[1, 4]
    sigma_A <- Portfolio[2, 4]
    YCP_A <- matrix(rlnorm((period * st), meanlog = mu_A, 
                           sdlog = sigma_A), period, st)
    YCP_A[1, ] <- 1
   
    # living expenses inflation vector
    
    lev <- matrix(1, MaxAge - AgeRe + 2, 1)
    for (i in 1 : (nrow(lev) - 1)) {
        lev[i + 1] <- (1 + r_live) ^ i  
    }
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 1.8 Financial Products
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    # whole life insurance premium rate w_rate
    
    if (Gender == "M"){
        w_rate <- FinaProd[AgePre + 1, 6]
    } else {
        w_rate <- FinaProd[AgePre + 1, 7]
    }
    
    #    w_bene <- Burial_cost + 0.5 * Salary
    

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    #phycon <- getPhycon(Gender,AgeRe,MaxAge,st)
    phycon <- get(paste("phycon.",Gender,AgeRe,sep=""))
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 6. Exhaustive method to obtain the optimal allocation for post-retirement
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # Pre-determined the LTC and the whole life insurance
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # LTC benefits (the whole benefits for 3 years)
    
    LTCb <- ((a0 + a1 * Salary)+ (c0 + c1 * AgeRe + c2 * Salary)) * 0.5
    
    # percentage for LTC
    # http://www.ownyourfuturetexas.org/long-term-care-insurance/sample-premium-rates/
    # http://www.tdi.texas.gov/pubs/consumer/lrgpolicy3.html
    
    # LTC benefit period
    LTC_period <- 5
    
    #ratio = annual benefits / annual premium
    ratio_LTC <- ((100 + 50) * 365) / 4354
    ratio_LTC <- 30                                                                     #### temporary setup, need further discussion
    # LTC annual premium
    pre_LTC <- LTCb / ratio_LTC
    # LTC single premium
    # use certain annuity to represent healthy life annuity
    sig_pre_LTC <- pre_LTC * cer_ann_due[round(length(which(phycon == 1)) / st)]      
    

        
    unit <- 0.1
    
    ####  Ideal case  ######
    FinSin <-  MP[j, idealAssetID] * (1 + APVstd[which(APVstd[,1]==AgeRe), ifelse(Gender=="M",2,3)] * 1.64)
    #**********************#
         
    #whole life benefit is to cover burial cost and 0.5* currentSalary
    w_prem <- MP[j,WLIdealID]            	# whole life insurance annual premium    
    w_bene <- w_prem / w_rate * 10000
    # Perm insurance
    sig_pre_perm <- w_prem * cer_ann_due[round(length(which(phycon == 1)) / st)]       

    per_LTC <- sig_pre_LTC / FinSin
    
    per_perm <- sig_pre_perm / FinSin
    
    per_LTCPerm <- round(per_LTC + per_perm, digits = 1) + unit
    
    ntest <- round(1 - per_LTCPerm, digits = 1) / unit
    
    # list all possible combinations of per_LTC and per_Ann
    possible_allocations <- matrix(seq(0.5, (1 - per_LTCPerm), by = unit), , 1)
    
    possible_results <- matrix(0, nrow(possible_allocations), 8)
    
    for (i in 1 : nrow(possible_allocations)) {
        possible_results[i, ] <- post_opt(FinSin, Salary, possible_allocations[i], life_ept)
    }
    
    # addjusted possible results (eliminate the cases that % portfolio < % annuity)
    if(length(which(possible_results[ , 3] < possible_results[ , 4])) == 0 ){
        add_pr <- possible_results
    } else {
        add_pr <- possible_results[-which(possible_results[ , 3] < possible_results[ , 4]), -c(8)]
    }
    
    
    # the optimal results for a single test
    # check if there is only one solution or no solution
    if(is.null(nrow(add_pr))){    
        test_optr <- add_pr
    }else{
        test_optr <- add_pr[which(add_pr[ , 2] == min(add_pr[ , 2])), ]
    }
    #if multi results have rp = 0, then choose greatest ANetA -- aggregated net asset at death
    if(!is.null(nrow(test_optr)) && nrow(test_optr)>1){
        test_optr <- test_optr[which(test_optr[,1] == max(test_optr[,1])),]
    }
    
    # the optimal results for all of the model points
    test_optr <- c(j, test_optr)
    mp_optr_ideal <- rbind(mp_optr_ideal, test_optr)
        if(FALSE){ 
            #####  Actual (affordable budget) case  #####
            FinSin <-  MP[j, actualAssetID]
            #*******************************************#
            
            # Whole life premium is driven by pre-model(affordable budget/actual asset)
            w_prem <- MP[j,WLIdealID]                # whole life insurance annual premium    
            w_bene <- w_prem / w_rate * 10000
            # Perm insurance
            sig_pre_perm <- w_prem * cer_ann_due[round(length(which(phycon == 1)) / st)]
            
            per_LTC <- sig_pre_LTC / FinSin
            
            per_perm <- sig_pre_perm / FinSin
            
            per_LTCPerm <- round(per_LTC + per_perm, digits = 1) + unit
            
            ntest <- round(1 - per_LTCPerm, digits = 1) / unit
            
            #if no possible_allocations available, skip to next data point
            if(per_LTCPerm > 0.5) next
            # list all possible combinations of per_LTC and per_Ann
            possible_allocations <- matrix(seq(0.5, (1 - per_LTCPerm), by = unit), , 1)
            
            possible_results <- matrix(0, nrow(possible_allocations), 8)
            
            for (i in 1 : nrow(possible_allocations)) {
                possible_results[i, ] <- post_opt(FinSin, Salary, possible_allocations[i], life_ept)
            }
            
        
            # addjusted possible results (eliminate the cases that % portfolio < % annuity)
            if(length(which(possible_results[ , 3] < possible_results[ , 4])) == 0 ){
                add_pr <- possible_results
            } else {
                add_pr <- possible_results[-which(possible_results[ , 3] < possible_results[ , 4]), -c(8)]
            }
        
            # the optimal results for a single test
            if(!is.null(nrow(add_pr))){    
                test_optr <- add_pr[which(add_pr[ , 2] == min(add_pr[ , 2])), ]
            }else{
                test_optr <- add_pr
            }
            
            #if multi results have rp = 0, then choose greatest ANetA
            if(!is.null(nrow(test_optr)) && nrow(test_optr)>1){
                test_optr <- test_optr[which(test_optr[,1] == max(test_optr[,1])),]
            }
            
            
            # the optimal results for all of the model points
            test_optr <- c(j, test_optr)
            mp_optr_actual <- rbind(mp_optr_actual, test_optr)
        }
    print(j)
}


mp_optr_ideal <- mp_optr_ideal[-1, ]
#mp_optr_actual <- mp_optr_actual[-1, ]


colnames(mp_optr_ideal) <- c("ANetA", "Ideal Ruin Prob", "% Investment", "% SPIA", "% DSPIA", "% LTC", "% WLI","shortfalls")
#colnames(mp_optr_actual) <- c("ANetA", "Actual Ruin Prob", "% Investment", "% SPIA", "% DSPIA", "% LTC", "% WLI","shortfalls")
write.csv(cbind(mp_optr_ideal,mp_optr_actual), file = "DataResults\\temp0604_1-50.csv")



