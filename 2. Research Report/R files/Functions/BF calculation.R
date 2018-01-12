BFcalc <- function(estimates, se) {
	# General arguments for Bain (remain equal across all data sets)
	sampsize <- c(65, 65, 65)
	
	ERr1 <- matrix(c(1, -1, 0, 0,
									 0, 1, -1, 0), nrow=2, ncol=4, byrow=TRUE)
	
	IRr1 <- matrix(0, nrow=0, ncol=0)
	
	ERr2 <- matrix(0, nrow=0, ncol=0)
	
	IRr2 <- matrix(c(-1, 1, 0, 0,
									 0, -1, 1, 0), nrow=2, ncol=4, byrow=TRUE)
	
	# Storage room
	all.BF <- list(NA)
	all.PMPb <- list(NA)
	
	# For each condition 
	for (p in 1:length(estimates)) {
		# Obtain estimates
		cond.est <- estimates[[p]]
		
		# Obtain SE
		cond.se <- se[[p]]
		
		# Progress indicator
		cat(p, " \r"); flush.console()    # Progress indicator
		
		# For each dataset
		cond.BF <- matrix(NA, nrow = nrow(cond.est), ncol = 2) # create storage room
		cond.PMPb <- matrix(NA, nrow = nrow(cond.est), ncol = 2)
		
		for (r in 1:nrow(cond.est)) {
			# Obtain estimates 
			dat.est <- cond.est[r,] 
			
			# Obtain  list of covariance matrices
			dat.cov <- cond.se[r,]**2
			dat.covlist <- list(as.matrix(dat.cov[1]), 
													as.matrix(dat.cov[2]), 
													as.matrix(dat.cov[3]))
			
			# Run Bain
			res <- Bain(estimate = dat.est, covariance = dat.covlist, nspec = 1, njoint = 0,
									samp = sampsize, ERr1, IRr1, ERr2, IRr2, print=FALSE)
			
			# Obtain BFs
			cond.BF[r,] <- res$testResult[,3]
			
			
			# Obtain PMPs
			cond.PMPb[r,] <- res$testResult[,5]
			
			cat(r, " \r"); flush.console()    # Progress indicator
		}
		
		all.BF[[p]] <- cond.BF
		all.PMPb[[p]] <- cond.PMPb
		
	}
 
	# Output
	list(all.BF = all.BF, all.PMPb = all.PMPb)
	
}


