# Load used packages
library(tidyverse)

# Load data
# (Source file : 2. Research Report/R files/Estimation/Estimation.R)
BFresultsOLS_pop2 <- dget("2. Research Report/R files/Results/BFresultsOLS_pop2.txt")
BFresultstrimmed_pop2 <- dget("2. Research Report/R files/Results/BFresultstrimmed_pop2.txt")

# Substract results to make a plot 
all.BFOLS_pop2 <- BFresultsOLS_pop2$all.BF
all.BFtrimmed_pop2 <- BFresultstrimmed_pop2$all.BF

# Transform result into plottable data
BFOLS <- all.BFOLS_pop2 %>% 
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

BFtrimmed <- all.BFtrimmed_pop2 %>% 
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

BFplotdata_pop2 <- rbind(BFOLS, BFtrimmed)

BFplotdata_pop2$BF <- factor(BFplotdata_pop2$BF, levels = c("BF1u", "BF2u", "BF12"))

# Save data
dput(BFplotdata_pop2, "2. Research Report/R files/Results/BFplotdata_pop2.txt")
