rm(list=ls())

# Load packages
library(tidyverse)
library(ggthemes)
library(WRS2)
library(extrafont) 
library(Bain)
loadfonts()

# Load functions
source("2. Research Report/R scripts/Functions/Estimation.R") 
source("2. Research Report/R scripts/Functions/Performance evaluation.R") 
source("2. Research Report/R scripts/Functions/BF calculation.R") 

# Set random seed
set.seed(2017)

# Determine group with outliers
round(runif(n = 1, min = 1, max = 3),0)

# ----------------------------------------------------------------------- #
# ------------------------------ PART 1 --------------------------------- #
# ----------------------------------------------------------------------- #

# Data H0
# ----------------------------------------------------------------------- #
source("2. Research Report/R scripts/Simulated data/Population_1_H0.R") 
# Each element of the list contains 1,000 datasets 
pop1 <- outdata    
rm(outdata)

# Estimation
# ----------------------------------------------------------------------- #
# Calculate OLS estimates for each sample (progress indicator runs till 11)
# Stored in a list, each element containing a matrix of which
# the rows are the samples, columns are the groups
OLS_estimates <- OLSestimation(pop1)
dput(OLS_estimates, "2. Research Report/R scripts/Saved results/OLS_estimates.txt")

# Calculate 20% trimmed mean estimates for each sample
# Stored in a matrix, rows are the samples, columns are the groups
t_estimates <- tmean(pop1, tr = 0.2)
dput(t_estimates, "2. Research Report/R scripts/Saved results/t_estimates.txt")

# Absolute bias
# ----------------------------------------------------------------------- #
# Calculate the aboslute bias for each condition (number of outliers) for
# the OLS estimates
biasOLS <- abbias(OLS_estimates$OLSmatrix, c(0,0,0))
dput(biasOLS, "2. Research Report/R scripts/Saved results/biasOLS.txt")
biasOLS <- dget("2. Research Report/R scripts/Saved results/biasOLS.txt")


# Calculate the aboslute bias for each condition (number of outliers) for
# the 20% trimmed mean estimates
biastrimmed <- abbias(t_estimates$tmeanmatrix, c(0,0,0))
dput(biastrimmed, "2. Research Report/R scripts/Saved results/biastrimmed.txt")
biastrimmed <- dget("2. Research Report/R scripts/Saved results/biastrimmed.txt")

# Bias plot
# Visualizes the bias for the different estimates for group 3, the group
# with outliers
plotdata <- rbind(data.frame(bias = biasOLS[,3],
														 outliers = seq(from = 0, to = 13, by = 1),
														 estimate = "OLS"), 
												
									data.frame(bias = biastrimmed[,3],
														 outliers = seq(from = 0, to = 13, by = 1),
														 estimate = "20% trimmed mean"))

pdf("2. Research report/Latex files/partI_biasplot_pop1.pdf", family="CM Roman", width=6, height=4)

ggplot(plotdata, aes(x=outliers, y=bias)) +
	geom_line(aes(linetype=estimate)) +
	scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11, 13)) +
	scale_linetype_discrete(name="Mean estimator") +
	labs(x = "Number of outliers", y = "Size of absolute bias") +
	theme_few() +
	theme(text=element_text(family="Georgia"))

dev.off()

# Coverage probability
# ----------------------------------------------------------------------- #
# Calculate the coverage probability for each condition (number of outliers) 
# for the OLS estimates
covOLS <- covprob(OLS_estimates$OLSmatrix, OLS_estimates$SEmatrix, 
									c(0,0,0), type=1)
dput(covOLS, "2. Research Report/R scripts/Saved results/covOLS.txt")
covOLS <- dget("2. Research Report/R scripts/Saved results/covOLS.txt")

# Calculate the coverage probability for each condition (number of outliers) 
# for the 20% trimmed mean estimates
cov_t <- covprob(t_estimates$tmeanmatrix, t_estimates$trimSEmatrix, 
									c(0,0,0), type=2)
dput(cov_t, "2. Research Report/R scripts/Saved results/cov_t.txt")
cov_t <- dget("2. Research Report/R scripts/Saved results/cov_t.txt")

# Coverage probability plot
# Visualizes the coverage probability for the different estimates for the
# three groups
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

pdf("2. Research report/Latex files/partI_covplot_pop1.pdf", family="CM Roman", width=6, height=4)

ggplot(plotdata, aes(x=outliers, y=cov_prob))+
	geom_hline(yintercept = 0.95, color = "gray", size = 2, alpha = 1/2) + 
	annotate("text", max(plotdata$outliers)-0.5, 0.95, vjust = 1.5, label = "95%")+
	geom_line(aes(linetype=factor(group)))+
	scale_x_continuous(breaks=c(1,3,5,7,9,11,13))+
	scale_linetype_discrete(name="Group")+
	labs(x = "Number of outliers", y = "95% CI coverage probability")+
	facet_wrap(~estimate, ncol = 2)+
	theme_few()+
	theme(text=element_text(family="Georgia"))

dev.off()

# ----------------------------------------------------------------------- #
# ------------------------------ PART 2 --------------------------------- #
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# --------------------------- Population 1 ------------------------------ #
# ----------------------------------------------------------------------- #

# Calculate the Bayes factors for the hypothesis with the OLS estimates as 
# input for Bain. 

# Calculate BFs with OLS estimates (note : can take a while)
BFresultsOLS <- BFcalc(estimates = OLS_estimates$OLSmatrix, 
											 se = OLS_estimates$SEmatrix)
dput(BFresultsOLS, "2. Research Report/R scripts/Saved results/BFresultsOLS.txt")
BFresultsOLS <- dget("2. Research Report/R scripts/Saved results/BFresultsOLS.txt")

# Calculate BFs with trimmed mean estimates (note : can take a while)
BFresultstrimmed <- BFcalc(estimates = t_estimates$tmeanmatrix, 
													 se = t_estimates$trimSEmatrix)
dput(BFresultstrimmed, "2. Research Report/R scripts/Saved results/BFresultstrimmed.txt")
BFresultstrimmed <- dget("2. Research Report/R scripts/Saved results/BFresultstrimmed.txt")

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

# Plot results
pdf("2. Research report/Latex files/partII_BF_pop1.pdf", family="CM Roman", width=6, height=4)

ggplot(data = BFplotdata, aes(x = Outliers, y = value, linetype = BF)) +
	geom_line() +
	scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11, 13)) +
	facet_grid(.~Estimate) +
	scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
	labs(x = "Number of outliers", y = "Mean size Bayes factor") +
	theme_few() +
  coord_cartesian(ylim = c(0, 50))+
	theme(text=element_text(family="Georgia"))

dev.off()

# ----------------------------------------------------------------------- #
# --------------------------- Population 2  ------------------------------ #
# ----------------------------------------------------------------------- #

# Data P2
# ----------------------------------------------------------------------- #
source("2. Research Report/R scripts/Simulated data/Population_2_H1.R") 
# Each element of the list contains 1,000 datasets 
pop2 <- outdata    
rm(outdata)

# Estimation P2
# ----------------------------------------------------------------------- #
OLS_estimates_pop2 <- OLSestimation(pop2)
dput(OLS_estimates_pop2, "2. Research Report/R scripts/Saved results/OLS_estimates_pop2.txt")

t_estimates_pop2 <- tmean(pop2, 0.2)
dput(t_estimates_pop2, "2. Research Report/R scripts/Saved results/t_estimates_pop2.txt")

# Calculate the Bayes factors for the hypothesis with the OLS estimates as 
# input for Bain. 

# Calculate BFs with OLS estimates (note : can take a while)
BFresultsOLS_pop2 <- BFcalc(estimates = OLS_estimates_pop2$OLSmatrix, 
											 se = OLS_estimates_pop2$SEmatrix)
dput(BFresultsOLS_pop2, "2. Research Report/R scripts/Saved results/BFresultsOLS_pop2.txt")
BFresultsOLS_pop2 <- dget("2. Research Report/R scripts/Saved results/BFresultsOLS_pop2.txt")


# Calculate BFs with trimmed mean estimates (note : can take a while)
BFresultstrimmed_pop2 <- BFcalc(estimates = t_estimates_pop2$tmeanmatrix, 
													 se = t_estimates_pop2$trimSEmatrix)
dput(BFresultstrimmed_pop2, "2. Research Report/R scripts/Saved results/BFresultstrimmed_pop2.txt")
BFresultstrimmed_pop2 <- dget("2. Research Report/R scripts/Saved results/BFresultstrimmed_pop2.txt")


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

BFplotdata <- rbind(BFOLS, BFtrimmed)

BFplotdata$BF <- factor(BFplotdata$BF, levels = c("BF1u", "BF2u", "BF12"))

# Plot results
pdf("2. Research report/Latex files/partII_BF_pop2.pdf", family="CM Roman", width=6, height=4)

ggplot(data = BFplotdata, aes(x = Outliers, y = value, linetype = BF)) +
	geom_line() +
	scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11, 13)) +
	facet_grid(.~Estimate) +
	scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
	labs(x = "Number of outliers", y = "Mean size Bayes factor") +
	theme_few() +
	coord_cartesian(ylim = c(0, 50))+
	theme(text=element_text(family="Georgia"))

dev.off()
