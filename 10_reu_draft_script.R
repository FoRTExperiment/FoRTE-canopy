
##### pulling the data
#
require(tidyverse)
require(ggplot2)
require(stringr)


################################
#big.boi <- read.csv("./data/ndvi/ndvi_forte_master.csv", skip = 7, header = FALSE)
big.boi <- read.csv("./data/ndvi/20190807_forte_paris_reu_set.csv")

#write.csv(big.boi, "2019_forte_paris_reu_set.csv")
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

write.csv(df2, "ndvi_reu_forte_set.csv")

# Assuming you've got a balanced design, the repeated measures ANOVA model using aov is

#aov(Measured.Value ~ Time + Error(Subject/Time), data = my.data)
ndvi.rpt.anova <- aov(ndvi ~ date + Error(SubplotID/date), data = df2)
summary(ndvi.rpt.anova)

lai.rpt.anova <- aov(lai ~ date + Error(SubplotID/date), data = df2)
summary(lai.rpt.anova)
# 
# The term Error(Subject/Time) models the subject-specific variance; or in the language of mixed-effect models: Subject is your random and Time your fixed effect, and Time is nested (i.e. repeatedly measured) within Subject.
# 
# Two more comments:
# 
# Keep in mind that aov only allows for random effects if the design is balanced (which is the case for the sample data you provide). If you've got an unbalanced design (e.g. due to missing observations), you will need to use a mixed-effect model.
# 
# It's a bit odd to use ANOVA to model a time dependence. I am assuming here that Time is a categorical variable.


# subplot by treatment
df2 %>%
  select(date, SubplotID, ratio, treatment, severity) %>%
  group_by(date, SubplotID, treatment) %>%
  summarise(ratio.mean = mean(ratio), ratio.sd = sd(ratio)) -> df.ratio

df.ratio <- data.frame(df.ratio)

df.ratio$cv <- (df.ratio$ratio.sd / df.ratio$ratio.mean) * 100

x11()
ggplot(df.ratio, aes(x = date, y = cv, color = treatment))+ 
  geom_point(size = 2, alpha = 0.8)+
  theme_bw()+
  xlab("DATE")+
  ylab("CV of NDVI/LAI Ratio")+
  geom_smooth(method = lm, se = FALSE)+
  facet_grid(rows = vars(treatment))
  
#### 
df2 %>%
  select(date, group, ratio, treatment, severity) %>%
  group_by(date, group, treatment) %>%
  summarise(ratio.mean = mean(ratio), ratio.sd = sd(ratio)) -> df.group.ratio

df.group.ratio <- data.frame(df.group.ratio)

df.group.ratio$cv <- (df.group.ratio$ratio.sd / df.group.ratio$ratio.mean) * 100

x11()
ggplot(df.group.ratio, aes(x = date, y = cv, color = treatment))+ 
  geom_point(size = 2, alpha = 0.8)+
  theme_bw()+
  xlab("DATE")+
  ylab("CV of NDVI/LAI Ratio")+
  geom_smooth(method = lm, se = FALSE)+
  scale_color_discrete(name="Treatment",
                                                           breaks=c("C", "B", "T"),
                                                           labels=c("Control", "Bottom-Up", "Top-Down"))+
  theme(legend.justification=c(0, 1), legend.position=c(0.02,0.98))+
  facet_grid(rows = vars(treatment))

# # subplot by severity
df2 %>%
  select(date, SubplotID, ratio,  severity) %>%
  group_by(date, SubplotID, severity) %>%
  summarise(ratio.mean = mean(ratio), ratio.sd = sd(ratio)) -> df.s.ratio

df.s.ratio <- data.frame(df.s.ratio)

df.s.ratio$cv <- (df.s.ratio$ratio.sd / df.s.ratio$ratio.mean) * 100

x11()
ggplot(df.s.ratio, aes(x = date, y = cv, color = severity))+ 
  geom_point(size = 2, alpha = 0.8)+
  theme_bw()+
  xlab("DATE")+
  ylab("CV of NDVI/LAI Ratio")+
  geom_smooth(method = lm, se = FALSE)+
  # scale_color_discrete(name="Treatment",
  #                      breaks=c("C", "B", "T"),
  #                      labels=c("Control", "Bottom-Up", "Top-Down"))+
  theme(legend.justification=c(0, 1), legend.position=c(0.02,0.98))+
  facet_grid(rows = vars(severity))

##### regression models

# by treatment
df.ratio %>% 
  filter(treatment == "B") -> b.df

df.ratio %>% 
  filter(treatment == "T") -> t.df

df.ratio %>% 
  filter(treatment == "C") -> c.df

# linear models by treatment
b.lm <- lm(cv ~ date, data = b.df)
t.lm <- lm(cv ~ date, data = t.df)  
c.lm <- lm(cv ~ date, data = c.df)

x11()
ggplot(b.df, aes(x = date, y = cv))+ 
  geom_point(size = 2, alpha = 0.8)+
  theme_bw()+
  xlab("DATE")+
  ylab("CV of NDVI/LAI Ratio")+
  geom_smooth(method = lm, se = FALSE)
x11()
ggplot(t.df, aes(x = date, y = cv))+ 
  geom_point(size = 2, alpha = 0.8)+
  theme_bw()+
  xlab("DATE")+
  ylab("CV of NDVI/LAI Ratio")+
  geom_smooth(method = lm, se = FALSE)
x11()
ggplot(c.df, aes(x = date, y = cv))+ 
  geom_point(size = 2, alpha = 0.8)+
  theme_bw()+
  xlab("DATE")+
  ylab("CV of NDVI/LAI Ratio")+
  geom_smooth(method = lm, se = FALSE)

# stats
df.ratio %>%
  filter(treatment == "B") %>%
  lm(ratio ~ treatment)


x11(width = 3, height = 3)
ggplot(df2, aes(x = date, y = ndvi))+ 
  geom_point(size = 2, alpha = 0.2)+
  theme_classic()+
  xlab("DATE")+
  ylab("NDVI")+
  geom_smooth(method = lm, se = TRUE)
  
x11(width = 3, height = 3)
ggplot(df2, aes(x = date, y = lai))+ 
  geom_point(size = 2, alpha = 0.2)+
  theme_classic()+
  xlab("DATE")+
  ylab("LAI")+
  geom_smooth(method = lm, se = TRUE)
