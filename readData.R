## read in data from file

AgePre <- MP[j, AgePreID]        		                    # current age				
currentSalary <- MP[j,currentSalaryID]                      # current salary
Gender <- levels(factor(MP[j, GenderID]))                   # Gender
AgeRe <- MP[j, AgeReID]    			                        # retirement age
life_ept <- get(paste("LE_",Gender,sep=""))[AgeRe-19]       # calculate life expectancy (LE.RData is the workspace containing surviving probability)
Salary <- currentSalary * (1 + r_wage) ^ (AgeRe - AgePre)   # salary at retirement (r_wage is defined in Assumptions.R)
ssb <- MP[j,ssbID]    				                        # social security benefit (calculated from pre model)
period <- MaxAge - AgeRe + 2                                # MaxAge is defined in Assumption.R; +2 is because we artificially add 1 year to be the ultimate age
WL.premium <- MP[j,WLIdealID]                               # whole life insurance annual premium
WL.benefit <- Burial_cost + 0.5 * currentSalary             #whole life benefit covers burial cost and 0.5* currentSalary

# LTC benefits (the whole benefits for 3 years)
LTC.benefit <- ((a0 + a1 * Salary)+ (c0 + c1 * AgeRe + c2 * Salary)) * 0.5
LTC.premium <- LTC.benefit / 30

## investment return rates
mu_L <- Portfolio[1, 2]
sigma_L <- Portfolio[2, 2] 
YCP_L <- matrix(rlnorm((period * st), meanlog = mu_L, 
                       sdlog = sigma_L), period, st)
YCP_L[1, ] <- 1                                                   # Low 

mu_M <- Portfolio[1, 3]
sigma_M <- Portfolio[2, 3]
YCP_M <- matrix(rlnorm((period * st), meanlog = mu_M, 
                       sdlog = sigma_M), period, st)
YCP_M[1, ] <- 1                                                   # medium

mu_A <- Portfolio[1, 4]
sigma_A <- Portfolio[2, 4]
YCP_A <- matrix(rlnorm((period * st), meanlog = mu_A, 
                       sdlog = sigma_A), period, st)
YCP_A[1, ] <- 1                                                   # aggresive

## living expenses inflation vector
lev <- matrix(1, MaxAge - AgeRe + 2, 1)
for (i in 1 : (nrow(lev) - 1)) {
    lev[i + 1] <- (1 + r_live) ^ i  
}



# whole life insurance premium rate w_rate
w_rate <- FinaProd[AgePre + 1, ifelse ( Gender == "M", 6, 7)]


