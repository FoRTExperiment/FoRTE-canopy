# working with Ben, alexey and stephanie

install.packages(c("dplyr", "tidyr", "ggplot2", "lubridate", "markdown", "babynames", "gapminder"))


#
require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lubridate)
require(markdown)
require(babynames)
require(gapminder)


#####
####
gapminder %>%
  filter(year == 1977) %>%
  ggplot( aes(gdpPercap, lifeExp, size = pop, color = continent))+
  geom_point()+
  labs(x = "Per Capita GDP", y = "Life Expectancy (Years)")+
  scale_x_log10()

gapminder %>%
  filter(country == "Egypt")%>%
  select(-country, -continent) -> Egypt
  
Egypt %>% 
  gather(variable, value, lifeExp, pop, gdpPercap)

Egypt %>%
  gather(variable, value, -year) %>% spread(year, value)

##
gapminder %>% unite(coco, country, continent)

gapminder %>%
  unite(coco, country, continent) %>%
  seperate(coco,
           into = c("country", "continent"),)


#ben's split-apply-combine combination
# 
# filter() - subset rows, likes base::subset()
# arrange() reorder rows like order()
# select() select or drop columns
# mutate() add new columns
# summarise() like base::aggregate

gapminder %>%
  group_by(continent) %>%
  summarize(meanpop = mean(pop))

# this will take the maximum value of any given column
gapminder %>%
  select(-continent, -year) %>%
  group_by(country) %>%
  summarize_all(list(max, min))

#3
gapminder %>%
  select(-year) %>%
  group_by(country) %>%
  summarise_if(is.numeric, funs(min, max, mean)) %>% 
  gather(variable, value, -country) %>%
  separate(variable, into = c("variable", "stat")) %>%
  spread(stat, value)
  ##################
require(babynames)
babynames %>% 
  filter(name == "Jeffrey", sex == "F") %>%
  ggplot( aes(year, prop))+
  geom_point()+
  geom_vline(xintercept = 1991)+
  theme_classic()+
  xlab("Year")+
  ylab("Proportion of People Named Jeff")
  
babynames %>%
  group_by(year, sex) %>%
  summarise(prop = max(prop),
            name = name[which.max(prop)]) %>%
  ggplot( aes(y = prop, x = year, color = name))+
  geom_point()+
  facet_grid(sex ~ .)
  
####
babynames %>%
  group_by
  

  
  

