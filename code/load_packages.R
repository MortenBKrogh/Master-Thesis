# install needed packages which is not installed
list.of.packages <- c("keras", "tensorflow", "tidyverse", "caret", "readr", "plyr", "data.table", "recipes", 'tidyquant','corrplot', 'DMwR',
                      'lime', 'readxl', 'rsample', 'yardstick', 'corr', 'DT', 'ggplot2', 'patchwork', 'openintro', 'scales', 'tidygraph',
                      'ggraph', 'stargazer', 'ggthemes', 'maps','lubridate', 'factoextra','survival', 'survminer', 'RColorBrewer')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages) 

# load required packages
lapply(list.of.packages, library, character.only = TRUE)
