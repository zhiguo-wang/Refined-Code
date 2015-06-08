#' naive strategy
#' required variables:
#'      period,st, ssb, phycon, Salary, AgeRe, lev, Burial_cost, LCP_M

source("refined-code\\config.R")
source("refined-code\\functions.R")
source("refined-code\\Assumptions.R")
load("refined-code\\LE.RData")
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


mp_naive_actual <- matrix(0, 1, 4)

for (j in 1 : 750) {
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 1.2 Customer Information
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    AgePre <- MP[j, AgePreID]    			# the current age				
    currentSalary <- MP[j,currentSalaryID]    
    Gender <- levels(factor(MP[j, GenderID]))
    AgeRe <- MP[j, AgeReID]    			# the retirement age
    life_ept <- get(paste("LE_",Gender,sep=""))[AgeRe-19]
    Salary <- currentSalary * (1 + r_wage) ^ (AgeRe - AgePre)
    ssb <- MP[j,ssbID]    				# social security benefit
    period <- MaxAge - AgeRe + 2

    
    # 1.3 Economic Assumptions
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
    # Stochastic Yield Curve for post-retirement    
    mu_M <- Portfolio[1, 3]
    sigma_M <- Portfolio[2, 3]
    YCP_M <- matrix(rlnorm((period * st), meanlog = mu_M, 
                           sdlog = sigma_M), period, st)
    YCP_M[1, ] <- 1
    
    # living expenses inflation vector
    
    lev <- matrix(1, MaxAge - AgeRe + 2, 1)
    for (i in 1 : (nrow(lev) - 1)) {
        lev[i + 1] <- (1 + r_live) ^ i  
    }
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 1.8 Financial Products
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    FinaProd <- read.csv("Inputs\\Financial Products\\Financial Products.csv", header = TRUE)
       
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    phycon <- get(paste("phycon.",Gender,AgeRe,sep=""))
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
    
    #####  Actual (affordable budget) case  #####
    FinSin <-  MP[j, naiveAssetID]
    #*******************************************#
   
    naiveResult <- post_naive()
    
    naiveResult <- c(j, naiveResult)
    # the optimal results for all of the model points
    mp_naive_actual <- rbind(mp_naive_actual, naiveResult)
    print(j)
}
colnames(mp_naive_actual) <- c("id", "ANetA", "agg_ruin", "shortfalls")
write.csv(mp_naive_actual, file="dataresults\\Post naive 750 06050330.csv")
