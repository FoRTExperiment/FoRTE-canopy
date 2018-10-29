#####
# loading the required packages
library(ggplot2)
library(ggmap)
require(plyr)
require(dplyr)
require(tidyverse)
require(ggforce)
require(splitstackshape)
require(emojifont)

# using plot A01W
stem <- read.csv("./data/haglof/111.CSV")

#make dbh in cm
stem$dbh <- stem$Tree_Dia * 10

#### Now to make DBH classes
breaks <- seq(8, 63 , by = 5)

# loop through em all
stem$dbh.class <- cut(stem$dbh, breaks, right = FALSE)

# change them to formatted levels
levels(stem$dbh.class) <- c("8-13", "13-18", "18-23", "23-28", "28-33", "33-38", "38-43", "43-48", "48-53", "53-58", "58-63")

# change order

# stratfied
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
df <- stratified(stem, group = "dbh.class",size = 0.45)

df$fate <- "kill"

#matching to old df
table1$val2 <- table2$val2[match(table1$pid, table2$pid)]
stem$fate <- df$fate[match(stem$Nr, df$Nr)]

stem$fate[is.na(stem$fate)] <- "live"
stem$fate <- as.factor(stem$fate)

stem$label <- ifelse( stem$fate == "kill", emoji('smile'), emoji('skull_and_crossbones'))
#add emoji
hist(stem$Tree_Dia)
# stem map
x11()
ggplot() +
  geom_point(data = stem, aes(x = Longitude, y = Latitude, size = Tree_Dia, color = fate, alpha = 0.8)) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  theme_classic()

x11()
ggplot(data = stem, aes(x = Longitude, y = Latitude, size = Tree_Dia, color = fate, label = label)) +
  geom_text(family="EmojiOne", size=6)+
  guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  theme_classic()

ggplot(d, aes(x, y, color=type, label=label)) +
  geom_text(family="EmojiOne", size=6)

ggplot(dat) +
  aes(x = x, y = y, size = pi * (dbh/2)^2) +
  geom_point(pch = 1) +
  scale_size_identity() +
  ggforce::geom_circle(
    aes(x0 = mean(dat$x), y0 = mean(dat$y), r = 3),
    color = "red",
    inherit.aes = FALSE
  ) +
  coord_equal() # Force x and y to be equal



#failed circle bullshit
# # plotting the map with some points on it
# circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
#   r = diameter / 2
#   tt <- seq(0,2*pi,length.out = npoints)
#   xx <- center[1] + r * cos(tt)
#   yy <- center[2] + r * sin(tt)
#   return(data.frame(x = xx, y = yy))
# }
# 
# dat <- circleFun(c(1,-1),2.3,npoints = 100)
# 
# #geom_path will do open circles, geom_polygon will do filled circles
# ggplot(dat,aes(x,y)) + geom_path()