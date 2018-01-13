# ---------------------------------------------------------------------------- 
# Load used packages
library(ggplot2)
library(ggthemes)
library(extrafont)
loadfonts()
# ---------------------------------------------------------------------------- 

# Figure 1 : Emperical influence function
# -----------------------------------------
# Source file : 
# 2. Research Report/R files/Analyses/Influence functions demonstrated.R

# Load saved data 
IFcombined <- dget("2. Research Report/R files/Results/IFcombined.txt")

# Plot
# pdf("2. Research Report/Latex Files/IFillustration.pdf", family="CM Roman", width=6, height=4)

ggplot(IFcombined,aes(x=x))+
	geom_line(aes(y=IFmean, linetype="mean"))+
	geom_line(aes(y=IFtrimmed, linetype="20% trimmed mean"))+
	labs(y="IF(y)", x = "y")+
	scale_linetype_discrete(name="Estimator", 
													breaks=c("mean", "20% trimmed mean"))+
	theme(text=element_text(family="CM Roman", size=10))+
	theme_few()

# dev.off()

# Figure 2 : Bias figure
# -----------------------------------------
# Source file : 
# 2. Research Report/R files/Analyses/Bias.R

# Load saved data
biasdata <- dget("2. Research Report/R files/Results/biasdata.txt")

# Plot
# pdf("2. Research report/Latex files/partI_biasplot_pop1.pdf", family="CM Roman", width=6, height=4)

ggplot(biasdata, aes(x=outliers, y=bias)) +
	geom_line(aes(linetype=estimate)) +
	scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11, 13)) +
	scale_linetype_discrete(name="Mean estimator") +
	labs(x = "Number of outliers", y = "Size of absolute bias") +
	theme_few()

# dev.off()

# Figure 3 : Coverage probability figure
# -----------------------------------------
# Source file : 
# 2. Research Report/R files/Analyses/Coverage probability.R

# Load saved data 
covprobdata <- dget("2. Research Report/R files/Results/covprobdata.txt")

# Plot
# pdf("2. Research report/Latex files/partI_covplot_pop1.pdf", family="CM Roman", width=6, height=4)

ggplot(covprobdata, aes(x=outliers, y=cov_prob))+
	geom_hline(yintercept = 0.95, color = "gray", size = 2, alpha = 1/2) + 
	annotate("text", max(covprobdata$outliers)-0.5, 0.95, vjust = 1.5, label = "95%")+
	geom_line(aes(linetype=factor(group)))+
	scale_x_continuous(breaks=c(1,3,5,7,9,11,13))+
	scale_linetype_discrete(name="Group")+
	labs(x = "Number of outliers", y = "95% CI coverage probability")+
	facet_wrap(~estimate, ncol = 2)+
	theme_few()

# dev.off()

# Figure 4 : Bayes factors population 1 
# -----------------------------------------
# Source file : 
# 2. Resesarch Report/R files/Analyses/BFsPop1 - Transform into plottable data.R

# Load saved data
BFplotdata <- dget("2. Research Report/R files/Results/BFplotdata.txt")

# Plot
# pdf("2. Research report/Latex files/partII_BF_pop1.pdf", family="CM Roman", width=6, height=4)

ggplot(data = BFplotdata, aes(x = Outliers, y = value, linetype = BF)) +
	geom_line() +
	scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11, 13)) +
	facet_grid(.~Estimate) +
	scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
	labs(x = "Number of outliers", y = "Mean size Bayes factor") +
	theme_few() +
	coord_cartesian(ylim = c(0, 50))

# dev.off()

# Figure 5 : Bayes factors population 2
# -----------------------------------------
# Source file : 
# 2. Resesarch Report/R files/Analyses/BFsPop2 - Transform into plottable data.R

# Load saved data
BFplotdata_pop2 <- dget("2. Research Report/R files/Results/BFplotdata_pop2.txt")

# Plot
# pdf("2. Research report/Latex files/partII_BF_pop1.pdf", family="CM Roman", width=6, height=4)

ggplot(data = BFplotdata_pop2, aes(x = Outliers, y = value, linetype = BF)) +
	geom_line() +
	scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11, 13)) +
	facet_grid(.~Estimate) +
	scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
	labs(x = "Number of outliers", y = "Mean size Bayes factor") +
	theme_few() +
	coord_cartesian(ylim = c(0, 50))

# dev.off()






