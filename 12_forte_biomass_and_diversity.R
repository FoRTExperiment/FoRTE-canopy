#
require(tidyverse)
require(stringr)
require(vegan)
require(ggplot2)

################ 
################ #SANDBOX for baseline inventory data
df <- read.csv("./data/forte_inventory.csv")
y <- read.csv("./data/forte_inventory_lai.csv")

# add leading zeros to create unique id
df$Tag2 <- str_pad(df$Tag, width = 4, pad = "0")
y$Tag2 <- str_pad(y$Tag, width = 4, pad = "0")

#creating unique tree ids
df$TreeID <- paste(df$SubplotID, df$Tag2, sep = ".")
#creating unique tree ids
y$TreeID <- paste(y$SubplotID, y$Tag2, sep = ".")

# check for duplicates
summary(duplicated(y$TreeID))

# sorting down
y %>%
  filter(Health_status != "D") %>%
  data.frame() -> z


df %>%
  filter(DBH_cm >= 8) %>%
  filter(Health_status != "D") %>%
  data.frame() -> x

table(x$SubplotID)
table(z$SubplotID)


setdiff(x$TreeID, z$TreeID)

#### for some reason treeid "A03E.2517" exists in the forte_inventory data set data.frame(x) and not in there other. No idea. 


#### 

# bringing in disturbance assignment data
a <- read.csv("./summary/group_a_lai_disturbance_treatment_output.csv")
b <- read.csv("./summary/group_b_lai_disturbance_treatment_output.csv")
c <- read.csv("./summary/group_c_lai_disturbance_treatment_output.csv")
d <- read.csv("./summary/group_d_lai_disturbance_treatment_output.csv")

# editing to combine
df <- do.call("rbind", list(a, b, c, d))

# filter out dead (did we kill these???????)
df %>%
  filter(Health_status != "D") %>%
  data.frame() -> df
  

# biomass

# Table  . Parameters a and b and diameter at breast height (DBH, cm) range for the aboveground biomass 
# (sans leaves) equation Mass = a * DBH^b for tree species occurring in the UMBS-Flux footprint.  
# 
# Studies used to augment Cooper’s raw data for a species are denoted by superscripts.
### BIOMASS BACKGROUND INFO
# Species	a	b	DBH range (cm)
# Acer rubrum a	0.0312	2.7780	1 – 40
# Acer saccharum b	0.1693	2.3436	3 – 66
# Amelanchier spp.c	0.1630	2.4940	0.3-2.7h
# Betula papyrifera d	0.0301	2.8387	2 – 49
# Fagus grandifolia b	0.1892	2.3097	3 – 66
# Pinus resinosa e	0.0526	2.5258	2 – 32
# Pinus strobus e	0.0408	2.5735	1 – 32
# Populus grandidentata f	0.1387	2.3498	1 – 37
# Populus tremuloides b	0.0589	2.6235	3 – 51
# Quercus rubra g	0.0398	2.7734	1 – 44
# a Crow and Erdmann, 1983, Perala and Alban, 1994, Young et al., 1980.
# b Young et al., 1980 equations used exclusively, DBH range estimated by Ker-Mikaelian and Korzukhin, 1997.
# c Perala and Alban, 1994 equations used exclusively.
# d Perala and Alban, 1994, Schmitt and Grigal, 1981, Young et al., 1980.
# e Ker, 1980, Perala and Alban, 1994, Young et al., 1980.
# f Koerper, 1977  raw data collected near UMBS-Flux tower.
# g Hocker and Early, 1983, Perala and Alban, 1994, Wiant et al., 1977.
# h Diameter at 15 cm height.

#
#bring in conversion to leaf area
allo.df <- read.csv("./data/biomass_allometry_table.csv") #this has the same equations AmeriFlux uses

#changing column names
names(allo.df)[1]<-paste("Species")

stem <- merge(df, allo.df)

#calculates biomass in units of kg
stem$biomass <- stem$a.biomass * stem$dbh^stem$b.biomass

####
x11()
ggplot(stem, aes(x = dbh, y = biomass, color = Species))+
  geom_point()


# making basal area
stem$ba <-  pi * ((stem$dbh / 2)^2) 




#####

# BA by species by plot
stem %>%
  group_by(SubplotID, Species) %>%
  summarize(ba.sum = sum(ba, na.rm = TRUE)) %>%
  data.frame() -> df.species


# species by plot
stem %>%
  group_by(SubplotID, Species) %>%
  count(Species) %>%
  data.frame() -> df.count

# Species richness
stem %>%
  group_by(SubplotID) %>%
  count(species.rich = n_distinct(Species)) %>%
  data.frame() -> df.richness


# begin plot diversity and biomass estimates
plot.diversity <- df.richness

df.species %>%
  group_by(SubplotID) %>%
  summarize(basal.area = sum(ba.sum, na.rm = TRUE)) -> df.species.by.plot

stem %>%
  group_by(SubplotID) %>%
  summarize(biomass = sum(biomass, na.rm = TRUE)) %>% data.frame() -> df.biomass.plot

#merging them together
plot.diversity <- merge(plot.diversity, df.species.by.plot)
plot.diversity <- merge(plot.diversity, df.biomass.plot)



#################
# OK now we need make wide for vegan
df.species %>%
  spread(Species, ba.sum) -> df.ba.wide
df.ba.wide <- data.frame(df.ba.wide)

#check the dimensions
dim(df.ba.wide)

df.ba.wide[is.na(df.ba.wide)] <- 0

df.ba <- df.ba.wide[-1,]



#vegan time
plot.diversity$simpsons.species <- diversity(df.ba.wide[,3:13], index = "simpson")
plot.diversity$shannon.species <- diversity(df.ba.wide[,3:13], index = "shannon")

plot.diversity$group <- substr(plot.diversity$SubplotID, 0, 1)

x11()
ggplot(plot.diversity, aes(x = simpsons.species, y = ((biomass * 0.001) * 10), label = as.factor(SubplotID), color = group))+
  geom_point(size = 4)+
  geom_text(hjust = 0, nudge_x = 0.01)+
  theme_minimal()+
  labs(y = "Biomass (Mg per ha)", x = "Shannon's Diversity", caption = "Look at D04, that boy is crazy!")+
  theme(legend.position="none")
  

# bringing in struchture
pcl <- read.csv("./data/forte_pcl_2018.csv")

head(pcl)

#
pcl$group <- as.factor(substring(pcl$plot, 1, 1))
pcl$subplot <- toupper(as.factor(paste(pcl$plot, pcl$plot_side, sep = "")))
pcl$plotID <- toupper(pcl$plot)
#
pcl %>%
  group_by(plotID) %>%
  summarize(rugosity = mean(rugosity, na.rm = TRUE), rugosity_sd = sd(rugosity), vai  = mean(mean.vai), vai_sd = sd(mean.vai)) %>%
  data.frame() -> pcl2

pcl2 %>%
  select(plotID, rugosity, vai) %>%
  data.frame() -> small.boi

plot.diversity2 <- merge(plot.diversity, small.boi, by = "plotID", keep.all = TRUE) 


write.csv(plot.diversity, "./summary/plot_diversity_and_biomass_forte_2018.csv")
#
x11()
ggplot(plot.diversity2, aes(x = rugosity, y = (biomass * 0.001) * 10, label = as.factor(plotID), color = group))+
  geom_point(size = 4)+
  geom_text(hjust = 0, nudge_x = 0.02)+
  theme_minimal()+
  labs(y = "Biomass (Mg)", x = "Rugosity", caption = "R^2 = 0.81")+
  geom_smooth(method = "lm")+
  theme(legend.position="none")

summary(lm(biomass ~ rugosity, data = plot.diversity))

x11()
ggplot(plot.diversity2, aes(x = vai, y = (biomass * 0.001), label = as.factor(plotID), color = group))+
  geom_point(size = 4)+
  geom_text(hjust = 0, nudge_x = 0.02)+
  theme_minimal()+
  labs(y = "Biomass (Mg)", x = "VAI", caption = "Look at D04, that boy is crazy!")+
  theme(legend.position="none")





