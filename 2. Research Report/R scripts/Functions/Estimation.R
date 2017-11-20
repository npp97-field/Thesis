# OLS estimation
OLSestimation <- function(data) {
	OLSmatrix <- list(NA)
	OLSSEmatrix <- list(NA)
	
	# For each condition (number of outliers)
	for (p in 1:length(data)) {
		# Extract the datamatrices corresponding to the condition
		copy <- data[[p]]
		
		# create storage room to save fitted linear model
		fit <- list(NA)
		
		# For each datamatrix in copy
		# fit a linear model for each dataset
		for (r in 1:length(copy)) {
			fit[[r]] <- lm(y ~ group-1, data=copy[[r]])
		}
		
		# extract the coefficients of the fitted linear model for each dataset
		OLSestimates <- sapply(fit, coef)
		
		# extract the standard errors of the coefficients for each dataset
		OLSSEestimates <- sqrt(sapply(fit, vcov)[c(1,5,9),])
		
		# store the estimates in a matrix, rows corresponding to sample ID, 
		# columns corresponding to group
		OLSmatrix[[p]] <- t(OLSestimates)
		OLSSEmatrix[[p]] <- t(OLSSEestimates)
		
		# progress indicator
		cat(p, " \r"); flush.console()            				
	}
	
	# Name list elements
	names(OLSmatrix) <- paste("OLS estimates for datamatrices with", 
														seq(from = 0, to = 10), "outlier(s)")
	
	names(OLSSEmatrix) <- paste("OLS SE estimates for datamatrices with", 
														seq(from = 0, to = 10), "outlier(s)")
	# output 
	list(OLSmatrix=OLSmatrix, SEmatrix=OLSSEmatrix)
}

# 20% trimmed mean estimation
tmean <- function(data, tr) {
	tmeanmatrix <- list(NA)
	trimSEmatrix <- list(NA)
	
	# For each condition (number of outliers)
	for (p in 1:length(data)) {
		# Extract the datamatrices corresponding to the condition
		copy <- data[[p]]
		
		# create storage room for to store the trimmed means
		tmeans <- matrix(NA, nrow=length(copy), ncol=3)
		tSE <- matrix(NA, nrow=length(copy), ncol=3)
		colnames(tmeans) <- c("group1", "group2", "group3")
		
		for (r in 1:length(copy)) {
			
			# Transform the data from long format into wide format
			wide <- unstack(copy[[r]], form = y ~ group)
			
			# Calculate the tr% trimmed mean for each column (= each group)
			tmeans[r,] <- apply(wide, 2, mean, trim=tr)
			
			# Calculate the SE 
			tSE[r,] <- apply(wide, 2, trimse, tr=tr)
			
		}
		
		tmeanmatrix[[p]] <- tmeans
		trimSEmatrix[[p]] <- tSE
		
		# progress indicator
		cat(p, " \r"); flush.console()    
	}
	
	# Name list elements
	names(tmeanmatrix) <- paste(tr*100, 
															"% trimmed mean estimates for datamatrices with", 
															seq(from = 0, to = 10), "outlier(s)")
	
	names(trimSEmatrix) <- paste(tr*100, 
															"% trimmed mean SE estimates for datamatrices with", 
															seq(from = 0, to = 10), "outlier(s)")
	
	# Output
	list(tmeanmatrix=tmeanmatrix, trimSEmatrix=trimSEmatrix)
}
