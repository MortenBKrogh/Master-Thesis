# install needed packages which is not installed
list.of.packages <- c("keras", "tensorflow", "tidyverse", "caret", "readr", "plyr", "data.table", "recipes", 'tidyquant', 'ddplyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages) 

# load required packages
lapply(list.of.packages, library, character.only = TRUE)

# install_keras()
# install_tensorflow()

