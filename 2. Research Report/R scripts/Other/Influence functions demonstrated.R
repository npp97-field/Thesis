# Load packages
library(WRS2)
library(ggplot2)					# Plotting package
library(ggthemes)         # Plotting themes 
library(extrafont)   			# Font styles
loadfonts()

# Plotting examples of influence functions 
# -----------------------------------------
# Influence of changing one single value 

set.seed(1010)
largesample <- rnorm(65, 0, 1)

# Influence function of the mean: IF(x) = x - /mu 
IFmean <- data.frame(
	x = seq(-10, 10, 1), 
	IF = seq(-10, 10, 1)-mean(largesample)
)

# Influence function of the 20% trimmed mean: 

x = seq(-10, 10, 1)

IFt <- NA
for (i in 1:length(x)) {
	if (x[i] <= quantile(largesample, 0.2)) {IFt[i] <- (1/(1-(2*0.2)))*(quantile(largesample, 0.2)-winmean(largesample))}
	if (x[i] >= quantile(largesample, 0.8)) {IFt[i] <- (1/(1-(2*0.2)))*(quantile(largesample, 0.8)-winmean(largesample))}
	if (x[i] >= quantile(largesample, 0.2) & x[i] <= quantile(largesample, 0.8)) 
	{IFt[i] <- (1/(1-(2*0.2)))*(x[i]-winmean(largesample))}
	
}

IFtrimmed <- data.frame(
	x = x,
	IF = IFt 
)

# Combined
IFcombined <- IFmean
IFcombined$trimmed <- IFt
colnames(IFcombined) <- c("x", "IFmean", "IFtrimmed")

pdf("2. Research Report/Latex Files/IFillustration.pdf", family="CM Roman", width=6, height=4)

ggplot(IFcombined,aes(x=x))+
	geom_line(aes(y=IFmean, linetype="mean"))+
	geom_line(aes(y=IFtrimmed, linetype="20% trimmed mean"))+
	labs(y="IF(y)", x = "y")+
	scale_linetype_discrete(name="Estimator", 
													breaks=c("mean", "20% trimmed mean"))+
	theme(text=element_text(family="CM Roman", size=10))+
	theme_few()

dev.off()

largesample[66] <- 10
mean(largesample)
mean(largesample[1:65], 0.2)

