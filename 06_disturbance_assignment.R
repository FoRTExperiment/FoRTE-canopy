# 06_disturbance_assignment
# this file assigns disturbances

#####
# loading the required packages
library(ggplot2)
library(ggmap)
require(plyr)
require(dplyr)
require(tidyverse)
require(ggforce)
require(splitstackshape)
require(data.table)


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

#remove empty lines from haglof
inventory <- na.omit(inventory, cols = "Tag")

#adding subplot
source("./code/addNewData.r")
allowedVars <- c("SubplotID")

#add subplot
df <- addNewData("./data/inventory_lookup_table.csv", inventory, allowedVars)

#
df$group <- as.factor(substr(df$SubplotID, 0, 1))

#
df$plotID <- as.factor(substr(df$SubplotID, 1, 3))

# cleaning up df
names(df)[names(df) == "DBH_cm"] <- "dbh"
df$dbh <- as.numeric(df$dbh)
df$SubplotID <- as.factor(df$SubplotID)


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
stem2 <- merge(stem, allo.fl)

stem2$leaf.mass <- stem2$a * (stem2$dbh^stem2$b)
#

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#stem$label <- ifelse( stem$fate == "kill", emoji('smile'), emoji('skull_and_crossbones'))
#add emoji
hist(stem$dbh)



# stem map
x11(height = 5, width = 6)
ggplot(data = stem, aes(x = Longitude, y = Latitude, size = (dbh/10))) +
  geom_point(alpha = 1)+
  #scale_colour_manual(values=cbbPalette)+
  #scale_shape_manual(values=c(1, 19)) +                  
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  theme_classic()


######
# Assuming `trees` is your data.frame


rtrees <- stem[sample(nrow(stem)), ]
rtrees$cum_dbh <- cumsum(rtrees$dbh)
samp_trees <- subset(rtrees, cum_dbh < 0.85 * sum(dbh))

#####
group plot disturbance
A    1          85
A    2          45
A    3          65
A    4           0
Group A Treatment Assignments
plot plot.side treatment
A1    bottom         E
A1       top         W
A2       top         E
A2    bottom         W
A3    bottom         E
A3       top         W
A4       top         W
A4    bottom         E

########
stem %>%
  filter(SubplotID == "A01W") %>%
  arrange(-dbh) -> df.big

sum.dbh <- sum(df.big$dbh)



# looping in
x <- 0

for (i in 1:nrow(df.big)) {
  x <- x + df.big$dbh[i]
  
  if(x < (0.85 * sum.dbh)){
    df.big$fate[i] <- "kill"}
  else {
    df.big$fate[i] <- "live"
  }
  
}

#loook at output
table(df.big$fate)

x11()
ggplot(data = df.big, aes(x = Longitude, y = Latitude, size = (dbh/10), color = Species, shape = fate)) +
  geom_point(alpha = 1)+
  scale_colour_manual(values=cbbPalette)+
  scale_shape_manual(values=c(1, 19))+
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  ggtitle("A01W - 85% - Top-Down")
  theme_classic()



  
  
  ###
  library(RColorBrewer)
  n <- 14
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  pie(rep(1,n), col=sample(col_vector, n))

  
  require(RColorBrewer)
  require(viridis)
  palette <- brewer.pal(3, "Set1")
  
  palette <- viridis_pal(option = "D")(8)  # n 
  #palette <- c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975")
  pie(rep(1, 8), col=palette)

####
