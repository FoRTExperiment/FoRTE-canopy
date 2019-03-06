#03_pcl
require(plyr)
require(dplyr)
require(ggplot2)
require(tidyverse)

pcl <- read.csv("./data/forte_pcl_2018.csv")

head(pcl)

#
pcl$group <- as.factor(substring(pcl$plot, 1, 1))
pcl$subplot <- as.factor(paste(pcl$plot, pcl$plot_side, sep = ""))

#
pcl %>%
  group_by(subplot) %>%
  summarize(rugosity = mean(rugosity), rugosity_sd = sd(rugosity), vai  = mean(mean.vai), vai_sd = sd(mean.vai)) -> df

#
df <- data.frame(df)


#boxplot rugosity
# A basic box with the conditions colored
x11()
ggplot(pcl, aes(x = group, y = rugosity, fill = group))+ 
  geom_boxplot()+
  theme_classic()+
  xlab("")+
  ylab("Rugosity")

x11()
ggplot(pcl, aes(x = group, y = mean.vai, fill = group))+ 
  geom_boxplot()+
  theme_classic()+
  xlab("")+
  ylab("VAI")

x11()
ggplot(pcl, aes(x = group, y = sky.fraction, fill = group))+ 
  geom_boxplot()+
  theme_classic()+
  xlab("")+
  ylab("VAI")




