# This is the main file of the thesis
list.of.packages <- c('here')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages('here') 
library(here)
here()
#Setting working directory to where the Rscrip is located
cat('Setting working directory to file location.')
setwd("~/Google Drive/Uni/Aarhus Universitet/10. semester/Speciale/Code")
# on the server the working directory should be something else.
# setwd('something something')
cat('Working Directory: ',getwd())

# setting the working directory and cleaning memory.
#Clearing variables
rm(list=ls())

# loading required packages
cat('Installing and loading required packages.')
source("load_packages.R")
cat('Packages loaded.')
#Clearing variables
rm(list=ls())



# firstly we will download the data, 
# extract it and place into the right
# folders. This is done in the Rscript
# I am sorcing here.

source('Freddie_Mac_Data_Download.R')

# Download  sample files and place them in the data folder
get_data_from_url(1999, 2018, sample=TRUE)

# Then we need to load and prepare the data for 
# the analysis, i.e. feature engineering and 
# normalizing for the Neural Network.

source("load_and_prepare_data.R")
var_names <- prepare_FMdata(1999, 2018, sample=TRUE)

# load the processed data
var_names <- c("id_loan", "dt_first_pi", "fico","flag_fthb","dt_matr","cd_msa","mi_pct",
               "cnt_units","occpy_sts","cltv","dti","orig_upb","ltv","int_rt",
               "channel","ppmt_pnlty","prod_type","st","prop_type","zipcode","loan_purpose",
               "orig_loan_term","cnt_borr","seller_name","servicer_name","flag_sc","delic_mean","delic_binary",
               "delic_date","surv_binary","surv","max_unpaid","recovered","first_complete_stop")

FM_data_files <- list.files(path=paste0(getwd(), "/data"), pattern="^Final_.*\\.txt", full.names=TRUE)
FM_data <- ldply(FM_data_files, data.table::fread, sep = " ", header = FALSE, col.names = var_names) #readr::read_delim("data/Final_1999.txt", delim = " ", col_names = FALSE)
