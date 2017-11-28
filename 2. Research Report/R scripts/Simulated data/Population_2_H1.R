# create groups
group <- rep(1:3, each = 65)

# sample 1,000 times from population
data <- list(NA)

for (r in 1:1000) {
	
	y <- NA
	
	for (i in 1:length(group)) {
		if (group[i] == 1) {y[i] <- rnorm(n = 1, mean = 0, sd = 1)}
		if (group[i] == 2) {y[i] <- rnorm(n = 1, mean = 0.5, sd = 1)}
		if (group[i] == 3) {y[i] <- rnorm(n = 1, mean = 1, sd = 1)}
	}
	
	data[[r]] <- data.frame(y=as.numeric(y), group=factor(group))
}

# add outliers to the third group 
outdata <- list(data)                              # storage room

for (p in 1:13) {
	copy <- data 
	
	for (r in 1:length(data)) {
		index <- sample(131:195, size = p)                # random case
		
		med3 <- median(data[[r]]$y[data[[r]]$group==3])   # median group 3
		madn3 <- mad(data[[r]]$y[data[[r]]$group==3])     # MADN group 3
		
		# Replace random case by outlier 
		copy[[r]]$y[index] <- med3 - runif(n = p, min = 2.5, max = 5)*madn3
	}
	
	outdata[[p+1]] <- copy
}

# Name list elements
names(outdata) <- paste("datamatrices with", seq(from = 0, to = 13), "outlier(s)")

rm(copy, data, group, i, index, madn3, med3, p, r, y)




