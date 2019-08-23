# processing forte PCL lidar collected by JAtkins and BAlvershare 2018
# 
# 
# 
# 



### checking for empty lines

setwd("./data/pcl/2019")

files <- list.files(pattern = "*.CSV", full.names = TRUE)

for(i in seq_along(files)) {
  filedata <- readLines(files[i])
  lines_to_skip <- min(which(filedata != "")) - 1
  cat(i, files[i], lines_to_skip, "\n")
  x <- read.csv(files[i], skip = lines_to_skip)
}

setwd("C:/github/FoRTExperiment/FoRTE-canopy")

######

# processing for CSC
require(forestr)

data_dir <- ("./data/pcl/2019")


# process_pcl("./data/pcl/forteB03E.CSV", pavd = TRUE)
process_multi_pcl(data_dir, user_height = 1.05, marker.spacing = 10, max.vai = 8)

# collating output
# 
# set your output directory
output_directory <- "./output/output/"

#
library(dplyr)
library(readr)
df <- list.files(path = output_directory, full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows

df <- data.frame(df)
write.csv(df, "forte_pcl_20190206.csv")