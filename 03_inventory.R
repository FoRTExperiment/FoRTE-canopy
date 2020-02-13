####
require(data.table)
require(plyr)
require(dplyr)
require(tidyverse)
#merging inventories
file_list <- list.files (path = "./data/inventory", 
                                   pattern    = "*.csv", 
                                   full.names = TRUE)


df <- as_tibble(rbindlist(lapply(file_list, fread)))


# lookupVariable	lookupValue	newVariable	newValue
# Subplot	111	SubplotID	A01W

#clear out no subplot rows
df <- df[!is.na(df$Subplot),]
source("./code/addNewData.r")
allowedVars <- c("SubplotID")

df <- addNewData("./data/inventory_lookup_table.csv", df, allowedVars)

df <- data.frame(df)

#
df$PlotID <- substr(df$SubplotID, 0, 1)

# cleaning up missing data
df$Species[df$Species == "FAGR#"] <- "FAGR"
df$Species[df$Species == "POGR ?"] <- "POGR"
df$Species[df$Species == "TSCA ?"] <- "TSCA"
df$Species[df$Species == "?"] <- "unknown"
df$Species[df$Species == "UNKNOWN"] <- "unknown"
df$Species[df$Species == "QUR"] <- "QURU"
df$Species[df$Species == "ADRU"] <- "ACRU"
#
df <- subset(df, Species != "snag")
#
df$DBH_cm <- as.numeric(df$DBH_cm)
#
df %>%
  filter(df$DBH_cm > 8) -> df


#####
# pulls in foliar allometry values for the upper great lakes, michigan or occasionally Maine 
# from  Ter-Mikaelian & Korzukhin, 1997 "Biomass equations for sixty-five North American tree species"
#
# using the equation M = aD^b
#
# AMEL set to POTR, ACPA set to ACSA, BEPA set to BEAL
lai.allo <- read.csv("./data/lai_allometry_table.csv")

# calculate LAI biomass for each tree
df$a <- lai.allo[match(df$Species, lai.allo$Species), "a"]
df$b <- lai.allo[match(df$Species, lai.allo$Species), "b"]

df$leaf_mass <- df$a * (df$DBH_cm^df$b)

#new_DF <- df[is.na(df$a),]
#####
table(df$SubplotID)
df.table <- table(df$Species, df$PlotID)
df.table <- data.frame(df.table)


# 
require(waffle)

library(waffle)
df.a <- subset(df.table, df.table$Var2 == "A")

#"QUR

A <- c("ACPE" = 26, "ACRU" = 60, "ACSA" = 246,  "BEPA" = 3, "FAGR" = 241,"PIRE" = 0, "PIST" = 0, "POGR" = 216, "QURU" = 8)  

B <- c("ACPE" = 11, "ACRU" = 394, "ACSA" = 6,  "BEPA" = 22, "FAGR" = 101,"PIRE" = 0, "PIST" = 1, "POGR" = 190, "QURU" = 61)  

C <- c("ACPE" = 7, "ACRU" = 233, "ACSA" = 0,  "BEPA" = 32, "FAGR" = 64,"PIRE" = 13, "PIST" = 76, "POGR" = 203, "QURU" = 191)  

D <- c("ACPE" = 0, "ACRU" = 166, "ACSA" = 0,  "BEPA" = 5, "FAGR" = 25,"PIRE" = 17, "PIST" = 162, "POGR" = 119, "QURU" = 199)  

x11()
waffle(A/10, rows=5, size=0.5, 
       colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#333333"), 
       title="Plot A", 
       xlab="1 square == 10 Individuals")
x11()
waffle(B/10, rows=5, size=0.5, 
       colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#333333"), 
       title="Plot B", 
       xlab="1 square == 10 Individuals")
x11()
waffle(C/10, rows=5, size=0.5, 
       colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#333333"), 
       title="Plot C", 
       xlab="1 square == 10 Individuals")
x11()
waffle(D/10, rows=5, size=0.5, 
       colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#333333"), 
       title="Plot D", 
       xlab="1 square == 10 Individuals")

<<<<<<< HEAD

waffle(savings/392, rows=7, size=0.5, 
       colors=c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf"), 
       title="Average Household Savings Each Year", 
       xlab="1 square == $392")
=======
# 
# savings <- c(`Mortgage ($84,911)`=84911, `Auto andntuition loans ($14,414)`=14414, 
#              `Home equity loans ($10,062)`=10062, `Credit Cards ($8,565)`=8565)
# waffle(savings/392, rows=7, size=0.5, 
#        colors=c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf"), 
#        title="Average Household Savings Each Year", 
#        xlab="1 square == $392")
>>>>>>> 39fc1e8799c41d45114a7ff5390ac1cbade2be6f


#####
######
# Assuming `trees` is your data.frame
rtrees <- df[sample(nrow(df)), ]
rtrees$cum_dbh <- cumsum(rtrees$dbh)
samp_trees <- subset(rtrees, cum_dbh < 0.85 * sum(dbh))


########
df.a1e <- subset(df, df$SubplotID == "A01E")

#bring in inventory data
# using plot A01W

a <- read.csv("./data/haglof/112.CSV")
b <- read.csv("./data/haglof/113.CSV")
c <- read.csv("./data/haglof/114.CSV")
d <- read.csv("./data/haglof/115.CSV")

jim <- rbind(a, b, c, d)
colnames(jim)[colnames(jim)=="Nr"] <- "Tag"
colnames(jim)[colnames(jim)=="Plot"] <- "Subplot"

jim %>%
  select("Subplot", "Tag", "Tree_Local_x", "Tree_Local_y", "Tree_Local_Dist",
         "Tree_Local_Angle", "Latitude", "Longitude") -> jim

#merge data
stem <- merge(df.a1e, jim, all.x = TRUE)

#deal with the empty row issues
stem <- subset(stem, !is.na(leaf_mass))

#big
stem %>%
  arrange(-leaf_mass) -> df.big

sum.leaf.mass <- sum(df.big$leaf_mass)

####space


# looping in
x <- 0

for (i in 1:nrow(df.big)) {
  x <- x + df.big$leaf_mass[i]
  
  if(x < (0.45 * sum.leaf.mass)){
    df.big$fate[i] <- "kill"}
  else {
    df.big$fate[i] <- "live"
  }
  
}

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



x11()
ggplot(data = df.big, aes(x = Longitude, y = Latitude, size = (DBH_cm/10), color = Species, shape = fate)) +
  geom_point(alpha = 1)+
  scale_colour_manual(values=cbbPalette)+
  scale_shape_manual(values=c(1, 19))+
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  theme_classic()


#start at the bottom
stem %>%
  arrange(leaf_mass) -> df.bottom

sum.leaf.mass <- sum(df.bottom$leaf_mass)

####space


# looping in
x <- 0

for (i in 1:nrow(df.big)) {
  x <- x + df.bottom$leaf_mass[i]
  
  if(x < (0.45 * sum.leaf.mass)){
    df.bottom$fate[i] <- "kill"}
  else {
    df.bottom$fate[i] <- "live"
  }
  
}

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#loook at output
table(df.bottom$fate)

x11()
ggplot(data = df.bottom, aes(x = Longitude, y = Latitude, size = (DBH_cm/10), color = Species, shape = fate)) +
  geom_point(alpha = 1)+
  scale_colour_manual(values=cbbPalette)+
  scale_shape_manual(values=c(1, 19))+
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  theme_classic()





