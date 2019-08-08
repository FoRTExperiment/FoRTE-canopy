
##### pulling the data
#
require(tidyverse)
require(ggplot2)
require(stringr)



################################
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
########################################

# Assuming you've got a balanced design, the repeated measures ANOVA model using aov is

#aov(Measured.Value ~ Time + Error(Subject/Time), data = my.data)
ndvi.rpt.anova <- aov(ndvi ~ date + Error(SubplotID/date), data = df2)


The term Error(Subject/Time) models the subject-specific variance; or in the language of mixed-effect models: Subject is your random and Time your fixed effect, and Time is nested (i.e. repeatedly measured) within Subject.

Two more comments:

Keep in mind that aov only allows for random effects if the design is balanced (which is the case for the sample data you provide). If you've got an unbalanced design (e.g. due to missing observations), you will need to use a mixed-effect model.

It's a bit odd to use ANOVA to model a time dependence. I am assuming here that Time is a categorical variable.