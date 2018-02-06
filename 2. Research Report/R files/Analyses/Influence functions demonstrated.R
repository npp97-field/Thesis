# Load packages
library(tidyverse)        # Data manipulating package
library(ggplot2)					# Plotting package
library(ggthemes)         # Plotting themes 
library(extrafont)   			# Font styles
loadfonts()

# Plotting examples of influence functions 
# -----------------------------------------
# Influence of adding one single value 

# Example sample
set.seed(36)
largesample <- rnorm(65, 0, 1)

# Additional values
y <- seq(-10, 10, 1)

# Empericial influences: 
mv <- rep(NA, length(y))             # storage room mean
tmv <- rep(NA, length(y))            # storage room 20% trimmed mean

for(i in 1:length(y)) {
	
	# Adding one value
	newsample <- largesample
	newsample[66] <- y[i]
	
	# Calculate the mean 
	mv[i] <- mean(newsample)
	tmv[i] <- mean(newsample, tr = 0.2)
}

# Make a dataframe
IF <- data.frame("Y" = y, 
					"mean" = mv,
					"trimmed" = tmv)

IF <- IF %>% gather(key = "Estimator", value = "Value", 2:3)

# Save data
dput(IF, "2. Research Report/R files/Results/IF.txt")






