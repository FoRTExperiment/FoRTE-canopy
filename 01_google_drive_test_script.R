#package dependencies

#this is jeff checking to see if this works.
library(plyr)
library(dplyr)
library(readr)
require(tidyverse)
require(googledrive)
require(ggplot2)

#check drives
drive_find(n_max = 50)
# https://drive.google.com/drive/folders/14XimVVLbjSfjpMk4X6ezcphWwBZvvizn?usp=sharing

#Direct Google Drive link to "FoRTE/data"
x <- as_id("https://drive.google.com/drive/folders/1YULT4fx50b1MXZNOywgEeosW0GkrUe9c?usp=sharing")
x <- as_id("https://drive.google.com/drive/folders/14XimVVLbjSfjpMk4X6ezcphWwBZvvizn?usp=sharing")

# Uses x to "get" drive
drive_get(as_id(x))

# lists what is in drive
drive_ls(x)


# id of drive with inventory
survey.files <- as_id("https://drive.google.com/open?id=1uD8EP-C902qB5wYYzgRl_7JHPBzGZ-YQ")
drive_ls(survey.files)
files <- drive_ls(survey.files)


dir.create("data/inventory", showWarnings = FALSE)

for(f in files$name) {
  cat("Downloading", f, "...\n")
  drive_download(f, overwrite = TRUE)
  print(f)
}




# 
# # Downloads file, but only one.
# drive_download("dummy_data", type = "csv", path = "./data/dummy_data.csv", overwrite = TRUE)


# y <- drive_find(q = "name contains 'dummy'")
# 
# for (i in 1:length(y)){
#   file <- y[1][i]
# drive_download(file, path = "./data/", overwrite = TRUE)
# }



# merging data
output_directory <- "./output/"

list.files(path = "./data/", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows -> df

# bring in data
df <- read.csv("./data/dummy_data.CSV")

# plot
ggplot(df, aes(x = leafN, y = leafC, color = as.factor(disturbance)))+
  geom_point(size = 4)+
  theme_classic()+
  xlab("Nitrogen (%)")+
  ylab("Carbon (%)")+
  ggtitle("Leaf Chemistry by Disturbance Level")
