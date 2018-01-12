# Load functions
source("2. Research Report/R files/Functions/Performance evaluation.R") 

# Load data
# (Source file : 2. Research Report/R files/Estimation/Estimation.R)
OLS_estimates <- dget("2. Research Report/R files/Results/OLS_estimates.txt")
t_estimates <- dget("2. Research Report/R files/Results/t_estimates.txt")

# Absolute bias
# ----------------------------------------------------------------------- #
# Calculate the aboslute bias for each condition (number of outliers) for
# the OLS estimates
biasOLS <- abbias(OLS_estimates$OLSmatrix, c(0,0,0))

# Calculate the aboslute bias for each condition (number of outliers) for
# the 20% trimmed mean estimates
biastrimmed <- abbias(t_estimates$tmeanmatrix, c(0,0,0))

# Combine the data in a dataframe 
plotdata <- rbind(data.frame(bias = biasOLS[,3],
														 outliers = seq(from = 0, to = 13, by = 1),
														 estimate = "OLS"), 
									
									data.frame(bias = biastrimmed[,3],
														 outliers = seq(from = 0, to = 13, by = 1),
														 estimate = "20% trimmed mean"))

# Save the data 
dput(plotdata, "2. Research Report/R files/Results/biasdata.txt")

