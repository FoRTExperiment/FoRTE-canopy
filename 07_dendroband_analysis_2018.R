# ties all the dendroband files together

require(plyr)
require(dplyr)
require(tidyverse)
require(data.table)


#####

# tie all the dendroband files together
#set data directory
data_dir <- "./data/dendroband/"

#merge a bunch of .csvs
multmerge = function(path){
  filenames=list.files(path=path, full.names=TRUE)
  rbindlist(lapply(filenames, fread))
}

#importing all the data
dendro.list <- multmerge(data_dir)


# find disturbance files
files <- list.files(path = "./summary", pattern="*disturbance_treatment_output.csv")


disturbed <- files %>% 
  map(function(x) {
    read.csv(paste0("./summary/", x))
  }) %>%
  reduce(rbind)

#make unique id
dendro.list$uniqueID <- paste(dendro.list$SubplotID, dendro.list$Tag, sep = ".")
disturbed$uniqueID <- paste(disturbed$SubplotID, disturbed$Tag, sep = ".")

#create fate column
dendro.list$fate <- disturbed$fate[match(dendro.list$uniqueID, disturbed$uniqueID)]


table(dendro.list$fate, dendro.list$SubplotID)

summary(dendro.list$fate)