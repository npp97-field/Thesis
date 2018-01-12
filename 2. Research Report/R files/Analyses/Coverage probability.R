# Load functions
source("2. Research Report/R files/Functions/Performance evaluation.R") 

# Load data
# (Source file : 2. Research Report/R files/Estimation/Estimation.R)
OLS_estimates <- dget("2. Research Report/R files/Results/OLS_estimates.txt")
t_estimates <- dget("2. Research Report/R files/Results/t_estimates.txt")

# Coverage probability
# ----------------------------------------------------------------------- #
# Calculate the coverage probability for each condition (number of outliers) 
# for the OLS estimates
covOLS <- covprob(OLS_estimates$OLSmatrix, OLS_estimates$SEmatrix, 
									c(0,0,0), type=1)

# Calculate the coverage probability for each condition (number of outliers) 
# for the 20% trimmed mean estimates
cov_t <- covprob(t_estimates$tmeanmatrix, t_estimates$trimSEmatrix, 
								 c(0,0,0), type=2)

# Combine the data in a dataframe 
plotdata <- rbind(
	data.frame(cov_prob = c(covOLS[,1], covOLS[,2], covOLS[,3]), 
						 group = rep(1:3, each=14),
						 outliers = rep(seq(from = 0, to = 13, by =1), 3),
						 estimate = "OLS"),
	
	data.frame(cov_prob = c(cov_t[,1], cov_t[,2], cov_t[,3]), 
						 group = rep(1:3, each=14),
						 outliers = rep(seq(from = 0, to = 13, by =1), 3),
						 estimate = "20% trimmed mean")
)

# Save the data
dput(plotdata, "2. Research Report/R files/Results/covprobdata.txt")
