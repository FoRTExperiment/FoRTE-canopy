#03_pcl
require(plyr)
require(dplyr)
require(ggplot2)
require(tidyverse)

##############2018

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


write.csv(df, "rugosity_shorty.csv")

#boxplot rugosity
# A basic box with the conditions colored
x11(width = 3, height = 3)
ggplot(pcl, aes(x = group, y = rugosity, fill = group))+ 
  geom_boxplot()+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("")+
  ylab("Rugosity")

x11(width = 3, height = 3)
ggplot(pcl, aes(x = group, y = mean.vai, fill = group))+ 
  geom_boxplot()+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("")+
  ylab("VAI")

x11(width = 3, height = 3)
ggplot(pcl, aes(x = group, y = sky.fraction, fill = group))+ 
  geom_boxplot()+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("")+
  ylab("Gap Fraction")



###### 2019

require(forestr)


# pcl work
require(forestr)


# lines check
#this script, thanks in part to the indomitable Ben Bond-Lamberty, error checks which files have missing lines.
og <- getwd()
setwd("./data/pcl/pcl2019")

files <- list.files(pattern = "*.CSV", full.names = TRUE)

for(i in seq_along(files)) {
  filedata <- readLines(files[i])
  lines_to_skip <- min(which(filedata != "")) - 1
  cat(i, files[i], lines_to_skip, "\n")
  x <- read.csv(files[i], skip = lines_to_skip)
}

setwd(og)


# run them bois
data_dir <- "./data/pcl/pcl2019"
process_multi_pcl(data_dir, marker.spacing = 10, user_height = 1.1)


## set your output directory
output_directory <- "./output/output/"

#
library(dplyr)
library(readr)
df <- list.files(path = output_directory, full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows

df <- data.frame(df)
write.csv(df, "hmc_pcl.CSV")


