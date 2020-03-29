# setting the working directory and cleaning memory.
#Clearing variables
rm(list=ls())

# loading required packages
cat('Installing and loading required packages.')
source("code/load_packages.R")
cat('Packages loaded.')
#Clearing variables
rm(list=ls())

# source script where functions to download and prepare data are locate
source('code/functions.R')

# Download  sample files and place them in the data folder
Data(1999, 2018, sample=TRUE)

