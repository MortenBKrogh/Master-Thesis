# function : Download Data 
Data <- function(START_YEAR, END_YEAR, sample=TRUE) {
  Main_dir = getwd()
  # create directories
  dir.create(file.path(Main_dir, 'data'), showWarnings = FALSE)  
  
  for (i in START_YEAR:END_YEAR) {
    
    if(sample == FALSE){
      for (j in 1:4) {
        
        year = paste0('Q', j, i)
        
        GET_Response <- httr::GET(paste0('https://freddiemac.embs.com/FLoan/Data/historical_data1_', as.character(year), '.zip'),
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


# prepare the data
prepare_FMdata <- function(START_YEAR, END_YEAR, sample=TRUE) {
  
  origination_names <- c('fico','dt_first_pi','flag_fthb','dt_matr','cd_msa', 'mi_pct','cnt_units','occpy_sts',
                         'cltv','dti','orig_upb','ltv','int_rt','channel','ppmt_pnlty','prod_type','st', 'prop_type',
                         'zipcode','id_loan','loan_purpose', 'orig_loan_term','cnt_borr','seller_name','servicer_name','flag_sc')
  
  performance_names <- c('id_loan','svcg_cycle','current_upb','delq_sts','loan_age', 
                         'mths_remng', 'repch_flag','flag_mod','cd_zero_bal','dt_zero_bal', 
                         'current_int_rt','non_int_brng_upb','dt_lst_pi','mi_recoveries', 
                         'net_sale_proceeds','non_mi_recoveries','expenses','legal_costs', 
                         'maint_pres_costs','taxes_ins_costs','misc_costs','actual_loss', 
                         'modcost','stepmod_ind','dpm_ind','eltv')
  if (sample==TRUE) {
    origination_files <- list.files(path=paste0(getwd(), "/data"), pattern="^sample_orig.*\\.txt", full.names=TRUE)
    performance_files <- list.files(path=paste0(getwd(), "/data"), pattern="^sample_svcg.*\\.txt", full.names=TRUE)
    
    
    
    for (i in (START_YEAR-(START_YEAR-1)):(END_YEAR-START_YEAR+1)) {
      # loading the origination datset
      # i <- 9
      origination <- read_delim(origination_files[i], 
                                "|", escape_double = FALSE, col_names = origination_names, 
                                trim_ws = TRUE) %>% 
        select(id_loan, dt_first_pi, everything()) %>% # drop_na(fico, orig_upb, int_rt) %>%
        mutate(dt_first_pi     = as.Date(paste0(as.character(dt_first_pi),"01"), format = "%Y%m%d"),
               dt_matr = as.Date(paste0(as.character(dt_matr),"01"), format = "%Y%m%d"), 
               fico = replace(fico, fico == 9999, NA),
               flag_fthb = replace(flag_fthb, flag_fthb == 9, 'Y'),
               occpy_sts = replace(occpy_sts, occpy_sts == 9, NA),
               cltv = replace(cltv, cltv == 999, NA),
               dti = replace(dti, dti == 999, NA),
               ltv = replace(ltv, ltv == 999, NA),
               channel = replace(channel, channel == 9, NA),
               prop_type = replace(prop_type, prop_type == 99, NA),
               loan_purpose = replace(loan_purpose, loan_purpose == 9, NA),
               cnt_borr = replace(cnt_borr, cnt_borr == 99, NA))
      
      # Loading the performance dataset
      performance <- read_delim(paste0(performance_files[i]), 
                                "|", escape_double = FALSE, col_names = performance_names, 
                                trim_ws = TRUE) %>% 
        mutate(delq_sts = replace(delq_sts, delq_sts == '   ', NA),
               repch_flag = replace(repch_flag, repch_flag == ' ', NA),
               svcg_cycle = as.Date(paste0(as.character(svcg_cycle),"01"), format = "%Y%m%d"))
      
    
      # clean performance data Maybe wait with dropping NA's til the data is combined in the final dataset
      # performance_v1 <- performance %>% # drop_na(current_upb, delq_sts, current_int_rt) %>% 
      #   mutate(svcg_cycle = as.Date(paste0(as.character(svcg_cycle),"01"), format = "%Y%m%d")) %>%
      #   dplyr::group_by(id_loan) %>% filter(row_number() >= (n())) %>%
      #   replace_na(list())
      
      performance$temp   = if_else(performance$delq_sts >= 3, performance$svcg_cycle, as.Date(NA))
      performance$temp_2 = if_else(performance$delq_sts == 1, performance$svcg_cycle, as.Date(NA))
      performance$temp_3 = if_else(performance$delq_sts == 1, performance$loan_age, as.numeric(NA))
      performance$temp_4 = if_else(performance$delq_sts >= 3, performance$loan_age, as.numeric(NA))
      performance$temp_5 = if_else(performance$delq_sts == 1, performance$mths_remng, as.numeric(NA))
      performance$temp_6 = if_else(performance$delq_sts >= 3, performance$mths_remng, as.numeric(NA))
      
      # Select and calculate performance features 
      performance_features <- performance %>%
        dplyr::group_by(id_loan) %>%
        dplyr::mutate('#current' = sum(delq_sts == 0, na.rm = TRUE),
                      '#30_dl' = sum(delq_sts == 1, na.rm = TRUE),
                      '#60_dl' = sum(delq_sts == 2, na.rm = TRUE),
                      '#90+_dl' = sum(delq_sts >= 3, na.rm = TRUE),
                      'current' = if_else(n() == sum(delq_sts == 0, na.rm=TRUE), TRUE, FALSE),
                      'default' = if_else(sum(delq_sts >= 3, na.rm = TRUE) > 0, TRUE, FALSE),
                      'dt_default'   = dplyr::first(na.omit(temp)),
                      'dt_delq' =dplyr::first(na.omit(temp_2)),
                      'delq_age' =dplyr::first(na.omit(temp_3)),
                      'default_age' =dplyr::first(na.omit(temp_4)),
                      'delq_remng' =dplyr::first(na.omit(temp_5)),
                      'default_remng' =dplyr::first(na.omit(temp_6)),
                      'pct_change_bal' = ((current_upb - lag(current_upb, 1)) / lag(current_upb, 1) )* 100) %>%
        filter(dplyr::row_number() >= (n() - 11)) %>%  
        dplyr::mutate('#current_l12' = sum(delq_sts == 0, na.rm = TRUE),
                      '#30_dl_l12' = sum(delq_sts == 1, na.rm = TRUE),
                      '#60_dl_l12' = sum(delq_sts == 2, na.rm = TRUE),
                      '#90+_dl_l12' = sum(delq_sts >= 3, na.rm = TRUE)) %>%
        filter(row_number() >= (n())) %>% 
        select(id_loan, default, dt_default, dt_delq, delq_age, default_age, delq_remng, default_remng, 
               current, delq_sts, current_upb, current_int_rt, loan_age, mths_remng, cd_zero_bal, 
               `#current`, `#30_dl`, `#60_dl`, `#90+_dl`,
               `#current_l12`, `#30_dl_l12`, `#60_dl_l12`, `#90+_dl_l12`)

      
      # Merge origination- and performance feature data
      Final <- left_join(origination, performance_features, by = "id_loan") #%>% 
        # drop_na(current_upb, delq_sts, current_int_rt, current_upb, cd_zero_bal, default)
      
      variable_names <- colnames(Final)
      readr::write_delim(Final, path = paste0(getwd(), "/data/Final_", i + START_YEAR - 1, '.txt'), col_names = FALSE, delim = "|")
    }}
  return(variable_names)
  }



# performance function for evaluation the models
perform_class = function (pre_class,y,classlabels) {
  confus=table(pre_class,y)
  TN=sum(pre_class==y & y==classlabels[1])
  TP=sum(pre_class==y & y==classlabels[2])
  FN=sum(pre_class!=y & y==classlabels[2])
  FP=sum(pre_class!=y & y==classlabels[1])
  specificity=TN/(TN+FP)
  sensitivity=TP/(TP+FN)
  accuracy=(TN+TP)/(TN+TP+FN+FP)
  return(list(confus=confus,specificity=specificity,sensitivity=sensitivity,accuracy=accuracy))
}
