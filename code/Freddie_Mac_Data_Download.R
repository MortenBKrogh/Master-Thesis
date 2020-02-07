#Clearing variables
# rm(list=ls())

#Setting working directory to where the Rscrip is located
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Main_dir = getwd()

# create directories
dir.create(file.path(Main_dir, 'data'), showWarnings = FALSE)#, DIR_NAME))


# find all origination and performance sample files
# origination_sample_files <- list.files(path=paste0(getwd(), "/data/",UNZIPPED_DIR_NAME), pattern="^[sample_orig].*\\.txt", full.names=TRUE)
# performance_sample_files <- list.files(path=paste0(getwd(), "/data/",UNZIPPED_DIR_NAME), pattern="^[sample_svcg].*\\.txt", full.names=TRUE)

# function : Download Data 
get_data_from_url <- function(START_YEAR, END_YEAR, sample=TRUE) {
  
  for (i in START_YEAR:END_YEAR) {
    
    if(sample == FALSE){
    for (j in 1:4) {
      
      year = paste0('Q', j, i)

      GET_Response <- httr::GET(paste0('https://freddiemac.embs.com/FLoan/Data/sample_', as.character(year), '.zip'),
                                httr::write_disk(paste0(Main_dir, '/', 'data', '/', as.character(year), '.zip'), overwrite = TRUE))
      
      if (GET_Response$status_code != 200) {
        unlink(paste0(Main_dir, '/', 'data', '/', as.character(year), '.zip'))
        print("Error: Undefined data period. Data should be in the period Q11999 to Q42017.")
      }
      
      unzip(paste0(Main_dir, '/', 'data', '/', as.character(year), '.zip'),
            exdir = paste0(Main_dir, '/', 'data', '/'))
      
      unlink(paste0(Main_dir, '/data/', as.character(year), '.zip'))
      
    }} else if (sample == TRUE) {
      
      GET_Response <- httr::GET(paste0('https://freddiemac.embs.com/FLoan/Data/sample_', as.character(i), '.zip'),
                                httr::write_disk(paste0(Main_dir, '/data','/', as.character(i), '.zip'), overwrite = TRUE))
      
      
      if (GET_Response$status_code != 200) {
        unlink(paste0(Main_dir, '/data', as.character(year), '.zip'))
        print("Error: Undefined data period. Data should be in the period Q11999 to Q42017.")
      }

      unzip(paste0(Main_dir, '/data', '/', as.character(i), '.zip'),
            exdir = paste0(Main_dir, '/data/'))
      
      unlink(paste0(Main_dir, '/data/', as.character(i), '.zip'))
      
    } 
  }
}


