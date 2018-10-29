# This script concatenates the survey data to one master data frame and then also
# creates the stratified dendroband output

require(plyr)
library(dplyr)
library(readr)
require(tidyverse)
require(ggplot2)
require(ggridges)
require(wesanderson)
require(splitstackshape)
# sets source file
# will make this a forte specific one eventually
source("./code/addNewData.r")

# sets directory import
data_dir <- "./data/inventory"
file.names <- dir(data_dir, pattern =".csv")


#for loop that moves through files in directory
for(i in 1:length(file.names)){
  g <- file.names[i]
  f <- file.path(data_dir, g)
 
  #df <- read.csv("./data/inventory/Inventory_D03W.csv", header = TRUE)

  ######
    df <- read.csv(f, header = TRUE)
  
  #the look up table function struggles with na's
  #df <- na.omit(df)
  df[is.na(df)] <- -99999

    # from lookup table
  allowedVars <- c("SubplotID")
  df <- addNewData("inventory_lookup_table.csv", df, allowedVars) 
  
  # reorder data frame
  #df <- df[,c(1, 2, 11, 3...)]
  

  if(is.character(f) == TRUE) {
    
    # Cuts off the directory info to give just the filename.
    filename <- sub(".*/", "", f)
    
  } else if(is.data.frame(f) == TRUE){
    filename <- deparse(substitute(f))
  }
  

 # name for output file
  outputname = substr(filename,1,nchar(filename)-4)

  #outputname <- paste(outputname, "output", sep = "_")
  dir.create("output", showWarnings = FALSE)
  output_directory <- "./output/"
  print(outputname)
  print(output_directory)
  
  filename2 <- paste(outputname, "_clean.csv", sep="")
  utils::write.csv(df, file.path(output_directory, filename2))
  
}


# set your output directory
output_directory <- "./output/"



# listing
list.files(path = "./output", full.names = TRUE) %>%
  lapply(read_csv) %>%
  rbind.fill() -> survey.data
  
# reorganizing
df <- survey.data[,c(1, 2, 3, 12, 4, 5, 6, 7, 8, 9, 10, 11 )]

# filter out bad DBH_cm data
df %>% filter(!DBH_cm == "REDACT") -> df.2

# redo as numeric
df.2$DBH_cm <- as.numeric(df.2$DBH_cm)

df.2 %>% filter(DBH_cm >= 8) -> df.3

# adding group
df.3$group <- substr(df.3$SubplotID, 1, 1)
df.3$group <- as.factor(df.3$group)
#historgram
x11()
hist(df.3$DBH_cm, breaks = seq(8, 63, by = 5))

ggplot(df.3, aes(x = DBH_cm))+
  geom_histogram(binwidth = 5, color = "black")+
  facet_grid(. ~ SubplotID)

colors <- c(
            "dodgerblue",
            "firebrick",
            "forestgreen",
            "gold")
# ridge example
ggplot(df.3, aes(x = DBH_cm, y = SubplotID, fill = group))+
  geom_density_ridges(scale = 3)+
  #scale_fill_manual(values = colors)+
  theme_classic()+
  xlab("DBH (cm)")+
  ylab("")



#### Now to make DBH classes
breaks <- seq(8, 63 , by = 5)

# loop through em all
df.3$dbh.class <- cut(df.3$DBH_cm, breaks, right = FALSE)

# change them to formatted levels
levels(df.3$dbh.class) <- c("8-13", "13-18", "18-23", "23-28", "28-33", "33-38", "38-43", "43-48", "48-53", "53-58", "58-63")

# change order
# df.3$dbh.class <- ordered(df.3$dbh.class, levels = c("8-13", "13-18", "18-23", "23-28", "28-33", "33-38", "38-43", "43-48", "48-53", "53-58", "58-63"))

# how many of each by subplotID
dbh.table <- table(df.3$SubplotID, df.3$dbh.class)
write.table(dbh.table, file = "dbh_table.csv", sep = ",")

#write.csv(df.3, "forte_survey_sorted.csv")
####
# BO1E
df.3 %>%
  filter(SubplotID == "A01E") %>%
  filter(!Health_status == "D") %>%
  select(Site, SubplotID, Tag, Species, DBH_cm, Health_status, Canopy_status, dbh.class) -> A01E
  
sp.list <- unique(A01E$Species)
sp.list <- list(sp.list)

set.seed = 420
A01e.dendro <- stratified(A01E, group = "dbh.class",size = 0.25)

#write.csv(d01e.dendro, "D01E_dendrobandlist.csv")

####
# CO1W
df.3 %>%
  filter(SubplotID == "A01W") %>%
  filter(!Health_status == "D") %>%
  select(Site, SubplotID, Tag, Species, DBH_cm, Health_status, Canopy_status, dbh.class) -> A01W

sp.list <- unique(A01W$Species)
sp.list <- list(sp.list)

set.seed = 420
A01w.dendro <- stratified(A01W, group = "dbh.class", size = 0.25)

#write.csv(d01e.dendro, "D01W_dendrobandlist.csv")

# ####
# A02E
df.3 %>%
  filter(SubplotID == "A02E") %>%
  filter(!Health_status == "D") %>%
  select(Site, SubplotID, Tag, Species, DBH_cm, Health_status, Canopy_status, dbh.class) -> A02E

sp.list <- unique(A02E$Species)
sp.list <- list(sp.list)

set.seed = 420
A02e.dendro <- stratified(A02E, group = "dbh.class", size = 0.25)

#write.csv(d02e.dendro, "D02E_dendrobandlist.csv")

####
##### A02W
df.3 %>%
  filter(SubplotID == "A02W") %>%
  filter(!Health_status == "D") %>%
  select(Site, SubplotID, Tag, Species, DBH_cm, Health_status, Canopy_status, dbh.class) ->A02W

sp.list <- unique(A02W$Species)
sp.list <- list(sp.list)

set.seed = 420
A02w.dendro <- stratified(A02W, group = "dbh.class", size = 0.25)

####
df.3 %>%
  filter(SubplotID == "A03E") %>%  
  filter(!Health_status == "D") %>%
  select(Site, SubplotID, Tag, Species, DBH_cm, Health_status, Canopy_status, dbh.class) -> A03E

sp.list <- unique(A03E$Species)
sp.list <- list(sp.list)

set.seed = 420
A03e.dendro <- stratified(A03E, group = "dbh.class", size = 0.25)

####
df.3 %>%
  filter(SubplotID == "A03W") %>%
  filter(!Health_status == "D") %>%
  select(Site, SubplotID, Tag, Species, DBH_cm, Health_status, Canopy_status, dbh.class) -> A03W

sp.list <- unique(A03W$Species)
sp.list <- list(sp.list)

set.seed = 420
A03w.dendro <- stratified(A03W, group = "dbh.class", size = 0.25)

####
# D04W
df.3 %>%
  filter(SubplotID == "A04W") %>%
  filter(!Health_status == "D") %>%
  select(Site, SubplotID, Tag, Species, DBH_cm, Health_status, Canopy_status, dbh.class) -> A04W

sp.list <- unique(A04W$Species)
sp.list <- list(sp.list)

set.seed = 420
A04w.dendro <- stratified(A04W, group = "dbh.class", size = 0.25)



####
# D04E
df.3 %>%
  filter(SubplotID == "A04E") %>%
  filter(!Health_status == "D") %>%
  select(Site, SubplotID, Tag, Species, DBH_cm, Health_status, Canopy_status, dbh.class) -> A04E

sp.list <- unique(A04E$Species)
sp.list <- list(sp.list)

set.seed = 420
A04e.dendro <- stratified(A04E, group = "dbh.class", size = 0.25)


A.band.list <- rbind(A01w.dendro, A01e.dendro, A02e.dendro, A02w.dendro, A03e.dendro, A03w.dendro, A04w.dendro, A04e.dendro)

write.csv(A.band.list, "A_bandlist.csv")

