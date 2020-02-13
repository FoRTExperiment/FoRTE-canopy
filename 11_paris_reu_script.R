#
##### pulling the data
#

# these are require packages to install/load
require(tidyverse)
require(ggplot2)
require(stringr)

# lets import the data
df <- read.csv("./data/ndvi/ndvi_reu_forte_set.csv")


# let's make summary stats
df %>%
  select(SubplotID, treatment, severity, date, lai, ndvi, gf) %>%
  group_by(SubplotID, date, treatment, severity) %>%
  summarise(mean.lai = mean(lai, na.rm = TRUE), sd.lai = sd(lai, na.rm = TRUE),
            mean.ndvi = mean(ndvi, na.rm = TRUE), sd.ndvi = sd(ndvi, na.rm = TRUE),
            mean.gf = mean(gf, na.rm = TRUE), sd.gf = sd(gf, na.rm = TRUE),n = n()) -> df.sums

#make into a data frame
df.sums <- data.frame(df.sums)


##### PLOTS

# BY TREATMENT
x11(width = 4, height = 4)
ggplot(df.sums, aes(x = as.Date(date), y = mean.lai, color = treatment))+ 
  geom_point(size = 4, alpha = 0.4)+
  theme_bw()+
  xlab("DATE")+
  ylab("LAI")+
  geom_smooth(method = lm, se = FALSE)+
  scale_color_discrete(name="Treatment",
                       breaks=c("C", "B", "T"),
                       labels=c("Control", "Bottom-Up", "Top-Down"))+
  theme(legend.position = "Bottom")+
  facet_grid(rows = vars(treatment))

x11(width = 4, height = 4)
ggplot(df.sums, aes(x = as.Date(date), y = mean.ndvi, color = treatment))+ 
  geom_point(size = 4, alpha = 0.4)+
  theme_bw()+
  xlab("DATE")+
  ylab("NDVI")+
  geom_smooth(method = lm, se = FALSE)+
  scale_color_discrete(name="Treatment",
                       breaks=c("C", "B", "T"),
                       labels=c("Control", "Bottom-Up", "Top-Down"))+
  theme(legend.position = "Bottom")+
  facet_grid(rows = vars(treatment))

x11(width = 4, height = 4)
ggplot(df.sums, aes(x = as.Date(date), y = mean.gf, color = treatment))+ 
  geom_point(size = 4, alpha = 0.4)+
  theme_bw()+
  xlab("DATE")+
  ylab("GF")+
  geom_smooth(method = lm, se = FALSE)+
  scale_color_discrete(name="Treatment",
                       breaks=c("C", "B", "T"),
                       labels=c("Control", "Bottom-Up", "Top-Down"))+
  theme(legend.position = "Bottom")+
  facet_grid(rows = vars(treatment))

# boxplots
x11()
ggplot(df, aes(x = date, y = ndvi, fill = SubplotID))+ 
  geom_boxplot()+
  theme_classic()+
  guides(fill=FALSE)+
  xlab("")+
  ylab("NDVI")+
  facet_grid(rows = vars(group))

# an analysis of variance

aov.lai.trt <- aov(lai ~ date * treatment * group, data = df)
summary(aov.lai.trt)