# Load packages
library(tidyverse)
library(ggthemes)
library(extrafont)
loadfonts()

# Plotting example of influence functions
# Influence of adding a single value

# Sample an example sample
set.seed(3) 
ex.sample <- rnorm(65, 0, 1)

mean(ex.sample)

# Additional values
y <- seq(-10, 10, 1)

mean.vector <- rep(NA, length(y))
tmean.vector <- rep(NA, length(y))

# Calculating the mean and 20% trimmed mean for each sample with an additional y-value
for(i in 1:length(y)){
	ex.sample[66] <- y[i]
	
	mean.vector[i] <- mean(ex.sample)
	tmean.vector[i] <- mean(ex.sample, tr = 0.2)
}

# Prepare for plotting
plotdat <- cbind(y, mean.vector, tmean.vector) %>% as_tibble()

plotdat.t <- plotdat %>% 
	rename("mean" = mean.vector,
				 "20% trimmed mean" = tmean.vector) %>%
	gather(key, value, 2:3)

# Plot
pdf("2. Research Report/Latex Files/IFillustration.pdf", family="CM Roman", width=6, height=4)

ggplot(plotdat.t, aes(x = y, y = value)) +
	geom_line(aes(linetype = key)) + 
	labs(y="Value parameter estimate", x = "Additional y value")+
	scale_linetype_discrete(name="Estimator", 
													breaks=c("mean", "20% trimmed mean"))+
	theme_few()+
	theme(text=element_text(family="Georgia"))

dev.off()

