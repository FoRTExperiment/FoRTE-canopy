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
# will make this a forte specific one eventually
source("./code/addNewData.r")



# using plot A01W
jim <- read.csv("./data/haglof/111.CSV")
colnames(jim)[colnames(jim)=="Nr"] <- "Tag"
colnames(jim)[colnames(jim)=="Plot"] <- "Subplot"

jim %>%
  select("Subplot", "Tag", "Tree_Local_x", "Tree_Local_y", "Tree_Local_Dist",
         "Tree_Local_Angle", "Latitude", "Longitude") -> jim

#bring in inventory data
#inventory <- read.csv("./data/inventory/Inventory_A01W.csv")

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

df <- addNewData("./data/inventory_lookup_table.csv", inventory, allowedVars)

stem <- merge(inventory, jim, all.x = TRUE)

#rename column
colnames(stem)[colnames(stem)=="DBH_cm"] <- "dbh"


#make dbh in cm
#stem$dbh <- stem$Tree_Dia * 10

#### Now to make DBH classes
breaks <- seq(8, 63 , by = 5)

# loop through em all
stem$dbh.class <- cut(stem$dbh, breaks, right = FALSE)

# change them to formatted levels
levels(stem$dbh.class) <- c("8-13", "13-18", "18-23", "23-28", "28-33", "33-38", "38-43", "43-48", "48-53", "53-58", "58-63")



# BO1E
# stem %>%
#   filter(SubplotID == "A01E") %>%
#   filter(!Health_status == "D") %>%
#   select(Site, SubplotID, Tag, Species, DBH_cm, Health_status, Canopy_status, dbh.class) -> A01E
# 
# sp.list <- unique(A01E$Species)
# sp.list <- list(sp.list)

# stratifying to kill
set.seed = 666
df <- stratified(stem, group = "dbh.class", size = 0.45)

df$fate <- "kill"

#matching to old df
table1$val2 <- table2$val2[match(table1$pid, table2$pid)]
stem$fate <- df$fate[match(stem$Tag, df$Tag)]

stem$fate[is.na(stem$fate)] <- "live"
stem$fate <- as.factor(stem$fate)

#clean the NA
stem %>% drop_na(Tag) -> stem

#

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#stem$label <- ifelse( stem$fate == "kill", emoji('smile'), emoji('skull_and_crossbones'))
#add emoji
hist(stem$dbh)

# stem map
x11(height = 5, width = 6)
ggplot(data = stem, aes(x = Longitude, y = Latitude, size = (dbh/10), color = Species, shape= fate)) +
  geom_point(alpha = 1)+
  scale_colour_manual(values=cbbPalette)+
  scale_shape_manual(values=c(1, 19)) +                  
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  theme_classic()


######
# Assuming `trees` is your data.frame
rtrees <- df[sample(nrow(df)), ]
rtrees$cum_dbh <- cumsum(rtrees$dbh)
samp_trees <- subset(rtrees, cum_dbh < 0.85 * sum(dbh))


########
stem %>%
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
    ggtitle("A01W - 85% - Top-Down")+
  theme_classic()





