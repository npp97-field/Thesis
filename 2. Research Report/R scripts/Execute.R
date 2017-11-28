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

# ----------------------------------------------------------------------- #
# --------------------------- Population 1 ------------------------------ #
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

# Calculate 20% trimmed mean estimates for each sample
# Stored in a matrix, rows are the samples, columns are the groups
t_estimates <- tmean(pop1, tr = 0.2)

# Absolute bias
# ----------------------------------------------------------------------- #
# Calculate the aboslute bias for each condition (number of outliers) for
# the OLS estimates
biasOLS <- abbias(OLS_estimates$OLSmatrix, c(0,0,0))

# Calculate the aboslute bias for each condition (number of outliers) for
# the 20% trimmed mean estimates
biastrimmed <- abbias(t_estimates$tmeanmatrix, c(0,0,0))

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

ggplot(plotdata, aes(x=outliers, y=bias))+
	geom_line(aes(linetype=estimate))+
	scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11, 13))+
	scale_linetype_discrete(name="Mean estimator")+
	labs(x = "Number of outliers", y = "Size of absolute bias")+
	theme_few()

dev.off()

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
	theme_few()

dev.off()

# ----------------------------------------------------------------------- #
# --------------------------- Population 2 ------------------------------ #
# ----------------------------------------------------------------------- #


# ----------------------------------------------------------------------- #
# ------------------------------ PART 2 --------------------------------- #
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# --------------------------- Population 1 ------------------------------ #
# ----------------------------------------------------------------------- #

# Calculate BFs with OLS estimates
all.est <- OLS_estimates$OLSmatrix
all.se <- OLS_estimates$SEmatrix

system.time(BFresultsOLS <- BFcalc(estimates = all.est, se = all.se))   # Takes about 20 min.

# Obtain results
all.BFOLS <- BFresultsOLS$all.BF
all.PMPbOLS <- BFresultsOLS$all.PMPb

resBF <- t(sapply(all.BFOLS, colMeans))
resPMPb <- t(sapply(all.PMPbOLS, colMeans))

resBF <- tbl_df(resBF)
resPMPb <- tbl_df(resPMPb)

information <- data.frame(BF = "BF1u", 
													Estimate = "OLS", 
													Outliers = seq(from = 0, to = 13, by = 1)) 

BF1uOLS <- resBF %>% 
	rename(Value = V1) %>%
	select(Value) %>%
	cbind(information)

information$BF <- "BF2u"

BF2uOLS <- resBF %>% 
	rename(Value = V2) %>%
	select(Value) %>%
	cbind(information)

information$BF <- "BF12"

BF12OLS <- resBF %>% 
	mutate(Value = V1/V2) %>%
	select(Value) %>%
	cbind(information)
	
BFOLS <- rbind(BF1uOLS, BF2uOLS, BF12OLS)

information <- information[,-1]
information$PMPB <- "PMP_H1"

PMP.H1.OLS <- resPMPb %>%
	rename(Value = V1) %>%
	select(Value) %>%
	cbind(information)

information$PMPB <- "PMP_H2"

PMP.H2.OLS <- resPMPb %>%
	rename(Value = V2) %>%
	select(Value) %>%
	cbind(information)

PMPbOLS <- rbind(PMP.H1.OLS, PMP.H2.OLS)

# Calculate BFs with trimmed mean estimates
all.est.t <- t_estimates$tmeanmatrix
all.se.t <- t_estimates$trimSEmatrix

system.time(BFresultstrimmed <- BFcalc(estimates = all.est.t, se = all.se.t))   # Takes about 25 min. 

# Obtain results
all.BFOLS <- BFresultstrimmed$all.BF
all.PMPbOLS <- BFresultstrimmed$all.PMPb

resBF <- t(sapply(all.BFOLS, colMeans))
resPMPb <- t(sapply(all.PMPbOLS, colMeans))

resBF <- tbl_df(resBF)
resPMPb <- tbl_df(resPMPb)

information <- data.frame(BF = "BF1u", 
													Estimate = "20% trimmed mean", 
													Outliers = seq(from = 0, to = 13, by = 1)) 

BF1utrimmed <- resBF %>% 
	rename(Value = V1) %>%
	select(Value) %>%
	cbind(information)

information$BF <- "BF2u"

BF2utrimmed <- resBF %>% 
	rename(Value = V2) %>%
	select(Value) %>%
	cbind(information)

information$BF <- "BF12"

BF12trimmed <- resBF %>% 
	mutate(Value = V1/V2) %>%
	select(Value) %>%
	cbind(information)

BFtrimmed <- rbind(BF1utrimmed, BF2utrimmed, BF12trimmed)

information <- information[,-1]
information$PMPB <- "PMP_H1"

PMP.H1.t <- resPMPb %>%
	rename(Value = V1) %>%
	select(Value) %>%
	cbind(information)

information$PMPB <- "PMP_H2"

PMP.H2.t <- resPMPb %>%
	rename(Value = V2) %>%
	select(Value) %>%
	cbind(information)

PMPbtrimmed <- rbind(PMP.H1.t, PMP.H2.t)

BFplotdata <- rbind(BFOLS, BFtrimmed)
PMPBplotdata <- rbind(PMPbOLS, PMPbtrimmed)

# Plot results
pdf("2. Research report/Latex files/partII_BF_pop1.pdf", family="CM Roman", width=6, height=4)

ggplot(data = BFplotdata, aes(x = Outliers, y = Value, linetype = BF)) +
	geom_line() +
	scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11, 13)) +
	facet_grid(.~Estimate) +
	labs(x = "Number of outliers", y = "Size Bayes factor") +
	theme_few() 

dev.off()

pdf("2. Research report/Latex files/partII_PMP_pop1.pdf", family="CM Roman", width=6, height=4)

ggplot(data = PMPBplotdata, aes(x = Outliers, y = Value, linetype = PMPB)) +
	geom_line() +
	scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11, 13)) +
	facet_grid(.~Estimate) +
	labs(x = "Number of outliers", y = "Size Posterior Model Probability (b)") +
	theme_few() 

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
OLS_estimates <- OLSestimation(pop2)
t_estimates <- tmean(pop2, 0.2)

# BF calculation 
# ----------------------------------------------------------------------- #
# Calculate BFs with OLS estimates
all.est <- OLS_estimates$OLSmatrix
all.se <- OLS_estimates$SEmatrix

system.time(BFresultsOLS <- BFcalc(estimates = all.est, se = all.se))   # Takes about 20 min.

# Obtain results
all.BFOLS <- BFresultsOLS$all.BF
all.PMPbOLS <- BFresultsOLS$all.PMPb

resBF <- t(sapply(all.BFOLS, colMeans))
resPMPb <- t(sapply(all.PMPbOLS, colMeans))

resBF <- tbl_df(resBF)
resPMPb <- tbl_df(resPMPb)

information <- data.frame(BF = "BF1u", 
													Estimate = "OLS", 
													Outliers = seq(from = 0, to = 13, by = 1)) 

BF1uOLS <- resBF %>% 
	rename(Value = V1) %>%
	select(Value) %>%
	cbind(information)

information$BF <- "BF2u"

BF2uOLS <- resBF %>% 
	rename(Value = V2) %>%
	select(Value) %>%
	cbind(information)

information$BF <- "BF12"

BF12OLS <- resBF %>% 
	mutate(Value = V1/V2) %>%
	select(Value) %>%
	cbind(information)

BFOLS <- rbind(BF1uOLS, BF2uOLS, BF12OLS)

information <- information[,-1]
information$PMPB <- "PMP_H1"

PMP.H1.OLS <- resPMPb %>%
	rename(Value = V1) %>%
	select(Value) %>%
	cbind(information)

information$PMPB <- "PMP_H2"

PMP.H2.OLS <- resPMPb %>%
	rename(Value = V2) %>%
	select(Value) %>%
	cbind(information)

PMPbOLS <- rbind(PMP.H1.OLS, PMP.H2.OLS)

# Calculate BFs with trimmed mean estimates
all.est.t <- t_estimates$tmeanmatrix
all.se.t <- t_estimates$trimSEmatrix

system.time(BFresultstrimmed <- BFcalc(estimates = all.est.t, se = all.se.t))   # Takes about 25 min. 

# Obtain results
all.BFOLS <- BFresultstrimmed$all.BF
all.PMPbOLS <- BFresultstrimmed$all.PMPb

resBF <- t(sapply(all.BFOLS, colMeans))
resPMPb <- t(sapply(all.PMPbOLS, colMeans))

resBF <- tbl_df(resBF)
resPMPb <- tbl_df(resPMPb)

information <- data.frame(BF = "BF1u", 
													Estimate = "20% trimmed mean", 
													Outliers = seq(from = 0, to = 13, by = 1)) 

BF1utrimmed <- resBF %>% 
	rename(Value = V1) %>%
	select(Value) %>%
	cbind(information)

information$BF <- "BF2u"

BF2utrimmed <- resBF %>% 
	rename(Value = V2) %>%
	select(Value) %>%
	cbind(information)

information$BF <- "BF12"

BF12trimmed <- resBF %>% 
	mutate(Value = V1/V2) %>%
	select(Value) %>%
	cbind(information)

BFtrimmed <- rbind(BF1utrimmed, BF2utrimmed, BF12trimmed)

information <- information[,-1]
information$PMPB <- "PMP_H1"

PMP.H1.t <- resPMPb %>%
	rename(Value = V1) %>%
	select(Value) %>%
	cbind(information)

information$PMPB <- "PMP_H2"

PMP.H2.t <- resPMPb %>%
	rename(Value = V2) %>%
	select(Value) %>%
	cbind(information)

PMPbtrimmed <- rbind(PMP.H1.t, PMP.H2.t)

BFplotdata <- rbind(BFOLS, BFtrimmed)
PMPBplotdata <- rbind(PMPbOLS, PMPbtrimmed)

# Plot results
pdf("2. Research report/Latex files/partII_BF_pop2.pdf", family="CM Roman", width=6, height=4)

ggplot(data = BFplotdata, aes(x = Outliers, y = Value, linetype = BF)) +
	geom_line() +
	scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11, 13)) +
	facet_grid(.~Estimate) +
	labs(x = "Number of outliers", y = "Size Bayes factor")+
	theme_few() 

dev.off()

pdf("2. Research report/Latex files/partII_PMP_pop2.pdf", family="CM Roman", width=6, height=4)

ggplot(data = PMPBplotdata, aes(x = Outliers, y = Value, linetype = PMPB)) +
	geom_line() +
	scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11, 13)) +
	facet_grid(.~Estimate) +
	labs(x = "Number of outliers", y = "Size Posterior Model Probability (b)") +
	theme_few() 

dev.off()


