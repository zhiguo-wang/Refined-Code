# read in assumption files
EcoAss <- read.csv("Inputs\\Economic Assumptions\\Economic Assumptions.csv", header = TRUE)
Portfolio <- read.csv("Inputs\\Economic Assumptions\\Investment Portfolio.csv", header = TRUE)
HeaToDea_M <- read.csv("Inputs\\Post Probability\\Healthy\\Annuity 2000 Basic Table - Male.csv", header = TRUE) # Health to Death for Male
FinaProd <- read.csv("Inputs\\Financial Products\\Financial Products.csv", header = TRUE)
# scenarios of Monte Carlo Simulation
st <- 2000
Burial_cost <- 25000
# Minimum Age: 20
MinAge <- 20
# Maximum Age: 115
MaxAge <- nrow(HeaToDea_M) + MinAge - 1

r_wage <- EcoAss[1, 2]

# living expense (basic + additional living expense) Inflation rate
r_live <- EcoAss[6, 2]


# Regression model for basic living expenses and healthcare expenses
# Annual basic living expenses (without mortgage & rent) = a0 + a1 * salary
# Annual healthcare expenses = b0 + b1 * age
# Annual additional living expenses = c0 + c1 * age + c2 * salary
# Personal insurance and pensions = d0 + d1 * age + d2 * salary
# Everage mortgage = m0 + m1 * Salary
# Everage rent = r0 + r1 * Age
# Coefficients from the Excel file: 
# E:\\UCONN Projects\\Mass Mutual\\Expenses\\MM expenses regression (latest).xlsm
a0 <- 5313.678641
a1 <- 0.179338079

b0 <- -780.2190476
b1 <- 80.20857143

c0 <- 1311.347392
c1 <- -120.3701846
c2 <- 0.288165909