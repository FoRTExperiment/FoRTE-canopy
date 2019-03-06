####
require(data.table)
require(plyr)
require(dplyr)
require(tidyverse)
#merging inventories
file_list <- list.files (path       = "./data/inventory", 
                                   pattern    = "*.csv", 
                                   full.names = TRUE,
                                   fill = TRUE)


df <- as_tibble(rbindlist(lapply(file_list, fread)))


# lookupVariable	lookupValue	newVariable	newValue
# Subplot	111	SubplotID	A01W

#clear out no subplot rows
df <- df[!is.na(df$Subplot),]
source("addNewData.r")
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
#####

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


savings <- c(`Mortgage ($84,911)`=84911, `Auto andntuition loans ($14,414)`=14414, 
             `Home equity loans ($10,062)`=10062, `Credit Cards ($8,565)`=8565)
waffle(savings/392, rows=7, size=0.5, 
       colors=c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf"), 
       title="Average Household Savings Each Year", 
       xlab="1 square == $392")