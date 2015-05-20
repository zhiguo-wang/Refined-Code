# Use Markov Chain to calculate life expectancy for each age

# Load Markov Chain Matrix
load("Refined Code\\new MC Matrix.RData")

# Life expectancy function
# Parameter: Gender: "M" or "F"
# Return: vector ex: contains Life Expectancy from age 20-115
getLE <- function(Gender){
	ex <- vector(length=116-20)
	for ( x in 20 : 115){
	
		MC_x_k <- diag(3)
		period <- 115 - x
		kpx <- vector(length=period+1)
		for(k in 0: period){
			MC_x_k <- MC_x_k %*% get(paste("MC_",Gender,"_",x+k,"_",x+k,sep=""))
			kpx[k] <- 1 - MC_x_k[1,3]
		}

		ex[x-19] <- sum(kpx)
	}
	return(ex)
}

LE_M <- getLE("M")		#Male life expectancy
LE_F <- getLE("F")		#Female life expectancy

# Save data to file
write.csv(data.frame(CurrentAge = 20:115, MaleLE = LE_Male, FemaleLE = LE_Female),file="refined code\\LE.csv")

# Save data to workspace image
variables <- ls()				# read in all current variables
variables <- variables[-c(3,4)]	# variable list expect "LE_Male" & "LE_Female"
rm(list = variables)			# remove variables except "LE_Male" & "LE_Female"
rm(variables)				# remove variable "variables"
