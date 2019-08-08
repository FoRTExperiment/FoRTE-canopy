#
require(tidyverse)
require(ggplot2)
require(stringr)

#big.boi <- read.csv("./data/ndvi/ndvi_forte_master.csv", skip = 7, header = FALSE)
big.boi <- read.csv("./data/ndvi/20190807_forte_paris_reu_set.csv")
#
big.boi %>% select(-1) -> df

str(df)

#rename em
names(df)[1] <- "date"
names(df)[2] <- "plotlong"
names(df)[3] <- "project"
names(df)[4] <- "ndvi"
names(df)[5] <- "gf"
names(df)[6] <- "open"
#names(df)[7] <- "ci"

# creating new factors
df$plot <- as.factor(substr(df$plotlong, 0, 3))
df$plot.side <- as.factor(substr(df$plotlong, 4, 4))
df$SubplotID <- as.factor(substr(df$plotlong, 0, 4))
df$group <- as.factor(substr(df$plotlong, 0, 1))

#
df %>% filter(project == "forte") %>% filter(group != "e") -> df2

#adding subplot
# getting rid of NAME pictures
df %>% filter(!str_detect(df$plotlong, 'NAME')) -> df

df$nps <- as.factor(substr(df$plotlong, 5, 5))
#
df %>% filter(project == "forte") %>% filter(group != "e") -> df2


#adding subplot
source("./code/addNewData.r")
allowedVars <- c("treatment")

df2 <- addNewData("./data/inventory_lookup_table_treatment.csv", df2, allowedVars)

allowedVars <- c("severity")
df2 <- addNewData("./data/inventory_lookup_table_severity.csv", df2, allowedVars)

# factorize
df2$severity <- as.factor(df2$severity)
df2$treatment <- as.factor(df2$treatment)

#make dates
df2$date <- as.Date(df2$date)

#lai
df2$lai <- rowMeans(df2[, 7:10])

#ratio
df2$ratio <- df2$ndvi/df2$lai


#boxplot rugosity
# A basic box with the conditions colored
x11()
ggplot(df2, aes(x = date, y = ndvi, fill = treatment))+ 
  geom_boxplot()+
  theme_classic()+
  # guides(fill=FALSE)+
  xlab("")+
  ylab("NDVI")+
  facet_grid(rows = vars(group))

x11()
ggplot(df2, aes(x = date, y = ndvi, fill = severity))+ 
  geom_boxplot()+
  theme_classic()+
  # guides(fill=FALSE)+
  xlab("")+
  ylab("NDVI")

x11()
ggplot(df2, aes(x = date, y = lai, fill = subplot))+ 
  geom_boxplot()+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("")+
  ylab("LAI")+
  facet_grid(rows = vars(group))
# math




x11(width = 3, height = 3)
ggplot(df2, aes(x = lai, y = ndvi, color = group))+ 
  geom_point()+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("LAI")+
  ylab("NDVI")

x11(width = 3, height = 3)
ggplot(df2, aes(x = lai, y = ndvi, color = treatment))+ 
  geom_point()+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("LAI")+
  ylab("NDVI")

x11(width = 3, height = 3)
ggplot(df2, aes(x = lai, y = ndvi, color = severity))+ 
  geom_point()+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("LAI")+
  ylab("NDVI")

x11(width = 3, height = 3)
ggplot(df2, aes(x = date, y = ratio))+ 
  geom_point(size = 2, alpha = 0.2)+
  theme_classic()+
  xlab("DATE")+
  ylab("NDVI/LAI Ratio")+
  geom_smooth(method = lm, se = TRUE)

x11(width = 3, height = 3)
ggplot(df2, aes(x = date, y = ndvi))+ 
  geom_point(size = 2, alpha = 0.2)+
  theme_classic()+
  xlab("DATE")+
  ylab("NDVI")+
  geom_smooth(method = loess, se = TRUE)

x11(width = 3, height = 3)
ggplot(df2, aes(x = date, y = lai))+ 
  geom_point(size = 2, alpha = 0.2)+
  theme_classic()+
  xlab("DATE")+
  ylab("LAI")+
  geom_smooth(method = loess, se = TRUE)

x11(width = 3, height = 3)
ggplot(df2, aes(x = date, y = ratio))+ 
  geom_point(size = 2, alpha = 0.2)+
  theme_classic()+
  xlab("DATE")+
  ylab("NDVI/LAI Ratio")+
  geom_smooth(method = lm, se = TRUE)

# STATS
lai.ratio.model <- lm(ratio ~ date, data = df2)


x11()
ggplot(df2, aes(x = date, y = lai, color = group))+ 
  geom_point()+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("date")+
  ylab("lai")+
  facet_grid(rows = vars(group))

# STATS
lai.model <- aov(lai ~ group * date, data = df2)


df2 %>%
  select(date, group,  ratio) %>%
  group_by(date, group) %>%
  summarise(ratio.mean = mean(df2$ratio, na.rm = TRUE), ratio.sd = sd(df2$ratio, na.rm = TRUE))



x11(width = 3, height = 3)
ggplot(df2, aes(x = gf, y = ndvi, color = group))+ 
  geom_point()+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("gap fraction")+
  ylab("NDVI")

x11(width = 3, height = 3)
ggplot(df, aes(x = group, y = gf, fill = group))+ 
  geom_boxplot()+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("")+
  ylab("Gap Fraction")

x11(width = 3, height = 3)
ggplot(df, aes(x = lai, y = ndvi, color = group))+ 
  geom_point(size = 3)+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("LAI")+
  ylab("NDVI")


### bringing in PCL now
pcl <- read.csv("./data/forte_pcl_2018.csv")

head(pcl)

#
pcl$group <- as.factor(substring(pcl$plot, 1, 1))
pcl$subplot <- as.factor(paste(pcl$plot, pcl$plot_side, sep = ""))


pcl$subplot <- toupper(pcl$subplot)

pcl %>%
  group_by(subplot) %>%
  summarize(rugosity = mean(rugosity), rugosity_sd = sd(rugosity), vai  = mean(mean.vai), vai_sd = sd(mean.vai), 
            ci_pcl = mean(clumping.index), ci_pcl_sd = sd(clumping.index), gf_pcl = mean(sky.fraction), gf_pcl_sd = sd(sky.fraction)) -> df.pcl

df %>%
  group_by(subplot) %>%
  summarize(ndvi = mean(ndvi), lai = mean(lai), gf = mean(gf), ci = mean(ci)) -> df.ndvi

# bring together
x <- merge(df.ndvi, df.pcl)
x$group <- substr(x$subplot, 0, 1)

x11(width = 3, height = 3)
ggplot(x, aes(x = rugosity, y = ndvi, color = group))+ 
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("RUGOSITY")+
  ylab("NDVI")

x11(width = 3, height = 3)
ggplot(x, aes(x = vai, y = ndvi, color = group))+ 
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("VAI")+
  ylab("NDVI")

######
x11(width = 3, height = 3)
ggplot(x, aes(x = vai, y = lai))+
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("VAI")+
  ylab("LAI")+
  xlim(0, 8)+
  ylim(0, 8)+
  geom_abline(slope = 1)

x11(width = 3, height = 3)
ggplot(x, aes(x = ci_pcl, y = ci))+
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("CI (PCL)")+
  ylab("CI (HEMI)")+
  xlim(0, 1)+
  ylim(0, 1)+
  geom_abline(slope = 1)

x11(width = 3, height = 3)
ggplot(x, aes(x = gf_pcl, y = gf))+
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("gap fraction (PCL)")+
  ylab("gap fraction (HEMI)")+
  xlim(0, 30)+
  ylim(0, 30)+
  geom_abline(slope = 1)


head(x)





###########################
# loading the required packages
library(ggplot2)
library(ggmap)
require(plyr)
require(dplyr)
require(tidyverse)
require(ggforce)
require(splitstackshape)
require(data.table)
library(forcats)
require(ggridges)

# The palette with black:
cbbPalette <-c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6aC02", "#a6761d", "#666666")


#bring in inventory data

#set data directory
data_dir <- "./data/inventory/"

#merge a bunch of .csvs
multmerge = function(path){
  filenames=list.files(path=path, full.names=TRUE)
  rbindlist(lapply(filenames, fread))
}

#importing all the data
inventory <- multmerge(data_dir)

#convert to data frame
inventory <- as(inventory, "data.frame")
inventory$Tag <- as.factor(inventory$Tag)

inventory <- na.omit(inventory, cols = "Subplot")
#

#adding subplot
source("./code/addNewData.r")
allowedVars <- c("SubplotID")

#add subplot
df <- addNewData("./data/inventory_lookup_table.csv", inventory, allowedVars)

#remove empty lines from haglof

inventory <- na.omit(inventory, cols = "Tag")
#
df$group <- as.factor(substr(df$SubplotID, 0, 1))

#
df$plotID <- as.factor(substr(df$SubplotID, 1, 3))

# cleaning up df
names(df)[names(df) == "DBH_cm"] <- "dbh"
df$dbh <- as.numeric(df$dbh)
df$SubplotID <- as.factor(df$SubplotID)
df$Species <- as.factor(df$Species)


# merge with spatial data
# using plot A01W
data_dir <- "./data/haglof/"

#import all the .csv files
spatial.data <- multmerge(data_dir)

# make a data frame
spatial.data <- as(spatial.data, "data.frame")

#rename columns
names(spatial.data)[1] <- "Subplot"
names(spatial.data)[2] <- "Plot_Radius"
names(spatial.data)[3] <- "Tag"
names(spatial.data)[4] <- "Tree_Spc"
names(spatial.data)[5] <- "Tree_Dia"
names(spatial.data)[6] <- "Tree_Hgt"
names(spatial.data)[7] <- "Tree_PosTex1"
names(spatial.data)[8] <- "Tree_PosTex2"
names(spatial.data)[9] <- "Tree_PosTex3"
names(spatial.data)[10] <- "Tree_Local_x"
names(spatial.data)[11] <- "Tree_Local_y"
names(spatial.data)[12] <- "Tree_Local_Dist"
names(spatial.data)[13] <- "Tree_Local_Angle"
names(spatial.data)[14] <- "Tree_Angle_ToPlotCenter"
names(spatial.data)[15] <- "Latitude"
names(spatial.data)[16] <- "Longitude"
names(spatial.data)[17] <- "Tree_Nr"


spatial.data %>%
  select("Subplot", "Tag", "Latitude", "Longitude") -> jim

#merging
stem <-  merge(df, jim, all.x = TRUE)

# cleaning up missing data
stem$Species[stem$Species == "FAGR#"] <- "FAGR"
stem$Species[stem$Species == "POGR ?"] <- "POGR"
stem$Species[stem$Species == "TSCA ?"] <- "TSCA"
stem$Species[stem$Species == "?"] <- "unknown"
stem$Species[stem$Species == "UNKNOWN"] <- "unknown"
stem$Species[stem$Species == "QUR"] <- "QURU"
stem$Species[stem$Species == "ADRU"] <- "ACRU"
#
stem <- subset(stem, Species != "snag")
#
#bring in conversion to leaf area
allo.df <- read.csv("./data/dbh_to_leaf_area_conversions.csv")

allo.df %>%
  filter(component == "FL") -> allo.fl
stem <- merge(stem, allo.fl)

stem$leaf.mass <- stem$a * (stem$dbh^stem$b)

stem <- droplevels(stem)


attach(stem)
stem$genus[stem$Species == "ACPE"] <- "Acer"
stem$genus[stem$Species == "ACRU"] <- "Acer"
stem$genus[stem$Species == "ACSA"] <- "Acer"
stem$genus[stem$Species == "BEPA"] <- "Betula"
stem$genus[stem$Species == "PIRE"] <- "Pinus"
stem$genus[stem$Species == "PIST"] <- "Pinus"
stem$genus[stem$Species == "QURU"] <- "Quercus"
stem$genus[stem$Species == "AMEL"] <- "Other"
stem$genus[stem$Species == "TSCA"] <- "Tsuga"
stem$genus[stem$Species == "FAGR"] <- "Fagus"
stem$genus[stem$Species == "POGR"] <- "Populus"
stem$genus[stem$Species == "POTR"] <- "Populus"
stem$genus[stem$Species == "unknown"] <- "Other"

stem$genus <- as.factor(stem$genus)

attach(stem)
stem$sla[stem$genus == "Acer"] <- 19
stem$sla[stem$genus == "Betula"] <- 20.82
stem$sla[stem$Species == "PIRE"] <- 5.39 #penner and deblonde ref.
stem$sla[stem$Species == "PIST"] <- 12.5 #abrams & kubiske, 1990
stem$sla[stem$genus == "Quercus"] <- 14.2
stem$sla[stem$genus == "Other"] <- 19
stem$sla[stem$genus == "Tsuga"] <- 5.84
stem$sla[stem$genus == "Fagus"] <- 35
stem$sla[stem$genus == "Populus"] <- 15.89

stem$leaf.area <- stem$leaf.mass * stem$sla

stem %>%
  filter(dbh >= 8) -> stem

stem %>%
  group_by(SubplotID) %>%
  summarise(plot.leaf.area = sum(leaf.area)) -> plot.stem

plot.stem <- data.frame(plot.stem)

# this adjusts total leaf are by plot size (0.1 ha)
plot.stem$plot.lai <- plot.stem$plot.leaf.area / 1000

names(plot.stem)[1] <- "subplot"


z <- merge(x, plot.stem, by = "subplot")

#########################

x11(width = 3, height = 3)
ggplot(z, aes(x = vai, y = plot.lai))+
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("VAI (PCL)")+
  ylab("LAI (Allometry)")+
  xlim(0, 8)+
  ylim(0, 8)+
  geom_abline(slope = 1)

x11(width = 3, height = 3)
ggplot(z, aes(x = lai, y = plot.lai))+
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("LAI (HEMI)")+
  ylab("LAI (Allometry)")+
  xlim(0, 8)+
  ylim(0, 8)+
  geom_abline(slope = 1)

