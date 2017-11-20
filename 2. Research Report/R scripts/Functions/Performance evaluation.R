abbias <- function(estimates, mu_vector) {
	# Storage room for saving the absolute bias matrices
	biasmatrix <- matrix(NA, nrow=length(estimates), ncol=3)
	
	# For each condition (number of outliers) 
	for (p in 1:length(estimates)) {
		biasmatrix[p,] <- apply(estimates[[p]], 2, mean)-mu_vector
	}
	
	# Output
	colnames(biasmatrix) <- c(paste("bias group", seq(from = 1, to = 3, by = 1)))
	biasmatrix
}

covprob <- function(parestimates, SE, mu_vector, type) {
	# Create storage room for saving the coverage probability
 	covprob <- matrix(NA, nrow=length(parestimates), ncol=3)
	
	# Define critical value if type = 1 (OLS estimates)
	if(type==1) {t <- qt(.975, df=50-1)}
	
	# Define critical value if type = 2 (20% trimmed mean)
	if(type==2) {t <- qt(.975, df=50-(2*0.2*50)-1)} 
	
	# For each condition (number of outliers) 
	for (p in 1:length(parestimates)) {
		# Extract the matrix with parameters corresponding to the condition
		estimates <- parestimates[[p]]
		estSE <- SE[[p]]
		
		# Storage room for saving the lower and upper limit and a vector indicating
		# if the population mean is in the confidence interval yes/no. 
		lower <- matrix(NA, nrow=nrow(estimates), ncol=3)          						
		upper <- matrix(NA, nrow=nrow(estimates), ncol=3)      
		covered <- matrix(NA, nrow=nrow(estimates), ncol=3)      
		
		for(r in 1:nrow(estimates)){
			# Calculate lower confidence interval bound
			lower[r,] <- estimates[r,]-t*estSE[r,]
			
			# Calculate upper confidence interval bound
			upper[r,] <- estimates[r,]+t*estSE[r,]
			
			# Calculate if the CI coveres the population mean
			covered[r,] <- mu_vector >= lower[r,] & mu_vector <= upper[r,]
			}
		
		# Calculate coverage probability 
		#return(covered)
		covprob[p,] <- apply(covered, 2, mean)
		
	}
	
	# Output
	colnames(covprob) <- c(paste("coverage probability group", seq(from = 1, to = 3, by = 1)))
	covprob
}