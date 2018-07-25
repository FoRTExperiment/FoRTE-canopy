# This script concatenates the survey data to one master data frame and then also
# creates the stratified dendroband output

require(plyr)
library(dplyr)
library(readr)
require(tidyverse)

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
 
  df <- read.csv(f, header = TRUE)

  ######
    df <- read.csv(f, header = TRUE)
  
  #the look up table function struggles with na's
  df <- na.omit(df)

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







