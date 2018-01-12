# Load used packages
library(tidyverse)

# Load data
# (Source file : 2. Research Report/R files/Estimation/Estimation.R)
BFresultsOLS <- dget("2. Research Report/R files/Results/BFresultsOLS.txt")
BFresultstrimmed <- dget("2. Research Report/R files/Results/BFresultstrimmed.txt")

# Substract results to make a plot 
all.BFOLS <- BFresultsOLS$all.BF
all.BFtrimmed <- BFresultstrimmed$all.BF

# Transform result into plottable data
BFOLS <- all.BFOLS %>% 
	sapply(colMeans) %>%
	t() %>%
	as_tibble() %>%
	rename(BF1u = V1) %>%
	rename(BF2u = V2) %>%
	mutate(BF12 = BF1u/BF2u) %>%
	gather() %>%
	cbind(Estimate = "OLS", Outliers = seq(0, 13, 1)) %>%
	as_tibble() %>%
	rename(BF = key)

BFtrimmed <- all.BFtrimmed %>% 
	sapply(colMeans) %>%
	t() %>%
	as_tibble() %>%
	rename(BF1u = V1) %>%
	rename(BF2u = V2) %>%
	mutate(BF12 = BF1u/BF2u) %>%
	gather() %>%
	cbind(Estimate = "20% trimmed mean", Outliers = seq(0, 13, 1)) %>%
	as_tibble() %>%
	rename(BF = key)

BFplotdata <- rbind(BFOLS, BFtrimmed)

BFplotdata$BF <- factor(BFplotdata$BF, levels = c("BF1u", "BF2u", "BF12"))

# Save data
dput(BFplotdata, "2. Research Report/R files/Results/BFplotdata.txt")
