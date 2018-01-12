# Load packages
library(tidyverse)
library(ggthemes)
library(WRS2)
library(extrafont) 
library(Bain)
loadfonts()

# Load functions
source("2. Research Report/R files/Functions/Estimation.R") 
source("2. Research Report/R files/Functions/BF calculation.R") 

# Set random seed
set.seed(2017)

# Determine group with outliers
round(runif(n = 1, min = 1, max = 3),0)

# ----------------------------------------------------------------------- #
# --------------------------- POPULATION 1 ------------------------------ #
# ----------------------------------------------------------------------- #

# Data H0
# ----------------------------------------------------------------------- #
source("2. Research Report/R files/Simulated data/Population_1_H0.R") 
# Each element of the list contains 1,000 datasets 
pop1 <- outdata    
rm(outdata)

# Estimation
# ----------------------------------------------------------------------- #
# Calculate OLS estimates for each sample (progress indicator runs till 11)
# Stored in a list, each element containing a matrix of which
# the rows are the samples, columns are the groups
OLS_estimates <- OLSestimation(pop1)

# Calculate 20% trimmed mean estimates for each sample
# Stored in a matrix, rows are the samples, columns are the groups
t_estimates <- tmean(pop1, tr = 0.2)

# Save the data
dput(OLS_estimates, "2. Research Report/R files/Results/OLS_estimates.txt")
dput(t_estimates, "2. Research Report/R files/Results/t_estimates.txt")

# Bayes factor calculation
# ----------------------------------------------------------------------- #
# Calculate BFs with OLS estimates 
BFresultsOLS <- BFcalc(estimates = OLS_estimates$OLSmatrix, 
											 se = OLS_estimates$SEmatrix)

# Calculate BFs with trimmed mean estimates 
BFresultstrimmed <- BFcalc(estimates = t_estimates$tmeanmatrix, 
													 se = t_estimates$trimSEmatrix)
# Save the data
dput(BFresultsOLS, "2. Research Report/R files/Results/BFresultsOLS.txt")
dput(BFresultstrimmed, "2. Research Report/R files/Results/BFresultstrimmed.txt")

# ----------------------------------------------------------------------- #
# --------------------------- Population 2  ------------------------------ #
# ----------------------------------------------------------------------- #

# Data P2
# ----------------------------------------------------------------------- #
source("2. Research Report/R files/Simulated data/Population_2_H1.R") 
# Each element of the list contains 1,000 datasets 
pop2 <- outdata    
rm(outdata)

# Estimation 
# ----------------------------------------------------------------------- #
# Calculate OLS estimates for each sample (progress indicator runs till 11)
# Stored in a list, each element containing a matrix of which
# the rows are the samples, columns are the groups
OLS_estimates_pop2 <- OLSestimation(pop2)

# Calculate 20% trimmed mean estimates for each sample
# Stored in a matrix, rows are the samples, columns are the groups
t_estimates_pop2 <- tmean(pop2, 0.2)

# Save the data
dput(OLS_estimates_pop2, "2. Research Report/R files/Results/OLS_estimates_pop2.txt")
dput(t_estimates_pop2, "2. Research Report/R files/Results/t_estimates_pop2.txt")

# Bayes factor calculation
# ----------------------------------------------------------------------- #
# Calculate BFs with OLS estimates (note : can take a while)
BFresultsOLS_pop2 <- BFcalc(estimates = OLS_estimates_pop2$OLSmatrix, 
														se = OLS_estimates_pop2$SEmatrix)


# Calculate BFs with trimmed mean estimates (note : can take a while)
BFresultstrimmed_pop2 <- BFcalc(estimates = t_estimates_pop2$tmeanmatrix, 
																se = t_estimates_pop2$trimSEmatrix)

# Save the data
dput(BFresultsOLS_pop2, "2. Research Report/R files/Results/BFresultsOLS_pop2.txt")
dput(BFresultstrimmed_pop2, "2. Research Report/R files/Results/BFresultstrimmed_pop2.txt")

