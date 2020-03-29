# function : Download Data 
Data <- function(START_YEAR, END_YEAR, sample=TRUE) {
  Main_dir = getwd()
  # create directories
  dir.create(file.path(Main_dir, 'data'), showWarnings = FALSE)  
  
  for (i in START_YEAR:END_YEAR) {
    
    if(sample == FALSE){
      for (j in 1:4) {
        
        year = paste0('Q', j, i)
        # https://freddiemac.embs.com/FLoan/Data/download.php?f=historical_data1_1999&s=584672842
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
  origination_classes <- c('numeric','numeric','character', 'numeric', 'character', 'numeric', 'numeric',
                           'character','numeric','numeric','numeric','numeric','numeric','character','character', 
                           'character','character', 'character','character','character','character', 
                           'numeric', 'numeric','character','character','character')
  
  performance_names <- c('id_loan','svcg_cycle','current_upb','delq_sts','loan_age', 
                         'mths_remng', 'repch_flag','flag_mod','cd_zero_bal','dt_zero_bal', 
                         'current_int_rt','non_int_brng_upb','dt_lst_pi','mi_recoveries', 
                         'net_sale_proceeds','non_mi_recoveries','expenses','legal_costs', 
                         'maint_pres_costs','taxes_ins_costs','misc_costs','actual_loss', 
                         'modcost','stepmod_ind','dpm_ind','eltv')
  
  performance_classes <- c('character','integer','real','character', 'integer','integer','character',
                           'character','character','integer','real','real','integer', 'integer', 
                           'character','integer','integer','integer','integer','integer','integer', 
                           'real','real', 'character','character','real')
  if (sample==TRUE) {
    origination_files <- list.files(path=paste0(getwd(), "/data"), pattern="^sample_orig.*\\.txt", full.names=TRUE)
    performance_files <- list.files(path=paste0(getwd(), "/data"), pattern="^sample_svcg.*\\.txt", full.names=TRUE)
    
    
    
    for (i in (START_YEAR-(START_YEAR-1)):(END_YEAR-START_YEAR+1)) {
      # loading the origination datset
     
      origination = as_tibble(fread(origination_files[i], sep="|", header=FALSE, stringsAsFactors = FALSE, colClasses=origination_classes))
      names(origination) <- origination_names
      origination %<>% select(id_loan, dt_first_pi, everything())  %>% 
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
      performance = as_tibble(fread(performance_files[i] , sep="|", header=FALSE, stringsAsFactors = FALSE, colClasses=performance_classes))
      names(performance) <- performance_names
      performance %<>%
        mutate(delq_sts = replace(delq_sts, delq_sts == '   ', NA),
               repch_flag = replace(repch_flag, repch_flag == ' ', NA),
               svcg_cycle = as.Date(paste0(as.character(svcg_cycle),"01"), format = "%Y%m%d"))

      performance_features_v2 <- performance %>% 
                                  dplyr::group_by(id_loan) %>%
                                  dplyr::mutate('repch_flag'      = dplyr::last(repch_flag),
                                                'cd_zero_bal'     = dplyr::last(cd_zero_bal),
                                                'dt_zero_bal'     = dplyr::last(dt_zero_bal),
                                                'dt_lst_pi'       = if_else(is.na(dplyr::last(dt_lst_pi)), dplyr::last(dt_zero_bal), dplyr::last(dt_lst_pi))) %>%
                                  dplyr::filter(dplyr::row_number() >= (n() - 12) & dplyr::row_number() <= (n() - 1)) %>%
                                  dplyr::mutate('lst_upb'         = dplyr::last(current_upb),
                                                'loan_age'        = dplyr::last(loan_age),
                                                'mths_remng'      = dplyr::last(mths_remng),
                                                'lst_int_rt'      = dplyr::last(current_int_rt),
                                                '#current_l12'    = sum(delq_sts == 0, na.rm = TRUE),
                                                '#30_dl_l12'      = sum(delq_sts == 1, na.rm = TRUE),
                                                '#60_dl_l12'      = sum(delq_sts == 2, na.rm = TRUE),
                                                '#90_dl_l12'      = sum(delq_sts >= 3, na.rm = TRUE),
                                                'default'         = if_else(cd_zero_bal == '02' |cd_zero_bal == '03' | cd_zero_bal == '06' | cd_zero_bal == '09', TRUE, FALSE),
                                                'prepaid'         = if_else(cd_zero_bal == '01', TRUE, FALSE)) %>%
                                  filter(row_number() >= (n()))   %>%
                                  dplyr::select(id_loan, repch_flag, cd_zero_bal, dt_zero_bal, dt_lst_pi,
                                                lst_upb, loan_age, mths_remng, lst_int_rt, `#current_l12`,
                                                `#30_dl_l12`, `#60_dl_l12`, `#90_dl_l12`, `default`, `prepaid`
                                                )
      Final <- left_join(origination, performance_features_v2, by = "id_loan") %>% drop_na(default, fico, orig_upb, cd_zero_bal)
                                  
      variable_names <- colnames(Final)
      readr::write_delim(Final, path = paste0(getwd(), "/data/Final_Sample_", i + START_YEAR - 1, '.txt'), col_names = FALSE, delim = "|")
    }}else if (sample == FALSE) {
    
      origination_files <- list.files(path=paste0(getwd(), "/data"), pattern="^historical_data1_Q.*\\.txt", full.names=TRUE)
      performance_files <- list.files(path=paste0(getwd(), "/data"), pattern="^historical_data1_time_.*\\.txt", full.names=TRUE)
      
      for (i in START_YEAR:length(origination_files)) {
        # loading the origination datset
        # i <- 9
        # origination <- read_delim(origination_files[i], 
        #                           "|", escape_double = FALSE, col_names = origination_names, 
        #                           trim_ws = TRUE) %>% 
        #   select(id_loan, dt_first_pi, everything()) %>% # drop_na(fico, orig_upb, int_rt) %>%
        #   mutate(dt_first_pi     = as.Date(paste0(as.character(dt_first_pi),"01"), format = "%Y%m%d"),
        #          dt_matr = as.Date(paste0(as.character(dt_matr),"01"), format = "%Y%m%d"), 
        #          fico = replace(fico, fico == 9999, NA),
        #          flag_fthb = replace(flag_fthb, flag_fthb == 9, 'Y'),
        #          occpy_sts = replace(occpy_sts, occpy_sts == 9, NA),
        #          cltv = replace(cltv, cltv == 999, NA),
        #          dti = replace(dti, dti == 999, NA),
        #          ltv = replace(ltv, ltv == 999, NA),
        #          channel = replace(channel, channel == 9, NA),
        #          prop_type = replace(prop_type, prop_type == 99, NA),
        #          loan_purpose = replace(loan_purpose, loan_purpose == 9, NA),
        #          cnt_borr = replace(cnt_borr, cnt_borr == 99, NA))
        # 
        # # Loading the performance dataset
        # performance <- read_delim(paste0(performance_files[i]), 
        #                           "|", escape_double = FALSE, col_names = performance_names, 
        #                           trim_ws = TRUE) %>% 
        #   mutate(delq_sts = replace(delq_sts, delq_sts == '   ', NA),
        #          repch_flag = replace(repch_flag, repch_flag == ' ', NA),
        #          svcg_cycle = as.Date(paste0(as.character(svcg_cycle),"01"), format = "%Y%m%d"))# %>% sample_n(1000)
        # 
        # 
        # # clean performance data Maybe wait with dropping NA's til the data is combined in the final dataset
        # # performance_v1 <- performance %>% # drop_na(current_upb, delq_sts, current_int_rt) %>% 
        # #   mutate(svcg_cycle = as.Date(paste0(as.character(svcg_cycle),"01"), format = "%Y%m%d")) %>%
        # #   dplyr::group_by(id_loan) %>% filter(row_number() >= (n())) %>%
        # #   replace_na(list())
        #                     
        # performance$temp   = if_else(performance$delq_sts >= 3, performance$svcg_cycle, as.Date(NA))
        # performance$temp_2 = if_else(performance$delq_sts == 1, performance$svcg_cycle, as.Date(NA))
        # performance$temp_3 = if_else(performance$delq_sts == 1, performance$loan_age, as.numeric(NA))
        # performance$temp_4 = if_else(performance$delq_sts >= 3, performance$loan_age, as.numeric(NA))
        # 
        # # Select and calculate performance features 
        # # performance_features <- performance %>%
        # #   dplyr::group_by(id_loan) %>%
        # #   dplyr::mutate('#current' = sum(delq_sts == 0, na.rm = TRUE),
        # #                 '#30_dl' = sum(delq_sts == 1, na.rm = TRUE),
        # #                 '#60_dl' = sum(delq_sts == 2, na.rm = TRUE),
        # #                 '#90+_dl' = sum(delq_sts >= 3, na.rm = TRUE),
        # #                 'current' = if_else(n() == sum(delq_sts == 0, na.rm=TRUE), TRUE, FALSE),
        # #                 'default' = if_else(sum(delq_sts >= 3, na.rm = TRUE) > 0, TRUE, FALSE),
        # #                 'dt_default'   = dplyr::first(na.omit(temp)),
        # #                 'dt_delq' =dplyr::first(na.omit(temp_2)),
        # #                 'delq_age' =dplyr::first(na.omit(temp_3)),
        # #                 'default_age' =dplyr::first(na.omit(temp_4)),
        # #                 'delq_remng' =dplyr::first(na.omit(temp_5)),
        # #                 'default_remng' =dplyr::first(na.omit(temp_6)),
        # #                 'pct_change_bal' = ((current_upb - lag(current_upb, 1)) / lag(current_upb, 1) )* 100) %>%
        # #   filter(dplyr::row_number() >= (n() - 11)) %>%  
        # #   dplyr::mutate('#current_l12' = sum(delq_sts == 0, na.rm = TRUE),
        # #                 '#30_dl_l12' = sum(delq_sts == 1, na.rm = TRUE),
        # #                 '#60_dl_l12' = sum(delq_sts == 2, na.rm = TRUE),
        # #                 '#90+_dl_l12' = sum(delq_sts >= 3, na.rm = TRUE)) %>%
        # #   filter(row_number() >= (n())) %>% 
        # #   select(id_loan, default, dt_default, dt_delq, delq_age, default_age, delq_remng, default_remng, 
        # #          current, delq_sts, current_upb, current_int_rt, loan_age, mths_remng, cd_zero_bal, 
        # #          `#current`, `#30_dl`, `#60_dl`, `#90+_dl`,
        # #          `#current_l12`, `#30_dl_l12`, `#60_dl_l12`, `#90+_dl_l12`)
        # 
        # performance_features <- performance %>%
        #   dplyr::group_by(id_loan) %>%
        #   dplyr::mutate(prev = lag(delq_sts, order_by=id_loan),
        #                 survived = ifelse(prev != 0 & delq_sts == 0, 1, 0)) %>%
        #   dplyr::mutate('#30_dl'     = sum(delq_sts == 1, na.rm = TRUE),
        #                 '#60_dl'        = sum(delq_sts == 2, na.rm = TRUE),
        #                 '#90+_dl' = sum(delq_sts >= 3, na.rm = TRUE),
        #                 'first_dt_delq'       = dplyr::first(na.omit(temp_2)),
        #                 'first_delq_age'      = dplyr::first(na.omit(temp_3)),
        #                 'first_delq_mths_remng' = dplyr::first(na.omit(temp_4)),
        #                 'first_eltv'     = dplyr::first(na.omit(dplyr::first(eltv))),
        #                 'last_eltv'      = dplyr::last(na.omit(dplyr::last(eltv))),  
        #                 'surv_binary'    = mean(survived, na.rm=T) > 0,
        #                 '#surv'          = sum(survived, na.rm=T),
        #                 'recovered'      = dplyr::last(delq_sts) == 0,
        #                 'rt_change'      = dplyr::first(current_int_rt) - dplyr::last(current_int_rt),
        #                 'default_v0'     = if_else(sum(delq_sts >= 3, na.rm = TRUE) > 0, TRUE, FALSE),
        #                 'dt_default_v0'  = dplyr::first(na.omit(temp)),
        #                 'default_v1'     = if_else(dplyr::last(delq_sts) != 0 & sum(delq_sts >= 3, na.rm = TRUE), TRUE, FALSE),
        #                 'dt_default_v1'  = dplyr::first(na.omit(if_else(dplyr::last(delq_sts) != 0 & sum(delq_sts >= 3, na.rm = TRUE) , svcg_cycle, as.Date(NA))))) %>% 
        #   filter(dplyr::row_number() >= (n() - 11)) %>%  
        #   dplyr::mutate('#current_l12' = sum(delq_sts == 0, na.rm = TRUE),
        #                 '#30_dl_l12' = sum(delq_sts == 1, na.rm = TRUE),
        #                 '#60_dl_l12' = sum(delq_sts == 2, na.rm = TRUE)) %>%
        #   filter(row_number() >= (n())) 
        
        
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
                 svcg_cycle = as.Date(paste0(as.character(svcg_cycle),"01"), format = "%Y%m%d"))# %>% sample_n(1000)
        performance$cd_zero_bal <- as.numeric(performance$cd_zero_bal)
        
        # clean performance data Maybe wait with dropping NA's til the data is combined in the final dataset
        # performance_v1 <- performance %>% # drop_na(current_upb, delq_sts, current_int_rt) %>% 
        #   mutate(svcg_cycle = as.Date(paste0(as.character(svcg_cycle),"01"), format = "%Y%m%d")) %>%
        #   dplyr::group_by(id_loan) %>% filter(row_number() >= (n())) %>%
        #   replace_na(list())
        
        performance$temp   = if_else(performance$delq_sts >= 3, performance$svcg_cycle, as.Date(NA))
        performance$temp_2 = if_else(performance$delq_sts == 1, performance$svcg_cycle, as.Date(NA))
        performance$temp_3 = if_else(performance$delq_sts == 1, performance$loan_age, as.numeric(NA))
        performance$temp_4 = if_else(performance$delq_sts >= 3, performance$loan_age, as.numeric(NA))
        
        # Select and calculate performance features 
        performance_features <- performance %>% 
          dplyr::group_by(id_loan) %>%
          dplyr::mutate(prev = lag(delq_sts, order_by=id_loan),
                        survived = ifelse(prev != 0 & delq_sts == 0, 1, 0)) %>%
          dplyr::mutate('#30_dl'                = sum(delq_sts == 1, na.rm = TRUE),
                        '#60_dl'                = sum(delq_sts == 2, na.rm = TRUE),
                        '#90+_dl'               = sum(delq_sts >= 3, na.rm = TRUE),
                        'first_dt_delq'         = dplyr::first(na.omit(temp_2)),
                        'first_delq_age'        = dplyr::first(na.omit(temp_3)),
                        'first_delq_mths_remng' = dplyr::first(na.omit(temp_4)),
                        'first_eltv'            = dplyr::first(na.omit(dplyr::first(eltv))),
                        'last_eltv'             = dplyr::last(na.omit(dplyr::last(eltv))),  
                        'surv_binary'           = mean(survived, na.rm=T) > 0,
                        '#surv'                 = sum(survived, na.rm=T),
                        'recovered'             = dplyr::last(delq_sts) == 0,
                        'rt_change'             = dplyr::first(current_int_rt) - dplyr::last(current_int_rt),
                        'default_v0'            = if_else(sum(delq_sts >= 3, na.rm = TRUE) > 0, TRUE, FALSE),
                        'dt_default_v0'         = dplyr::first(na.omit(temp)),
                        'default_v1'            = if_else(dplyr::last(delq_sts) != 0 & sum(delq_sts >= 3, na.rm = TRUE) > 0, TRUE, FALSE),
                        'default_v2'            = if_else(dplyr::last(cd_zero_bal) != 1 & sum(delq_sts >= 3, na.rm = TRUE) > 0, TRUE, FALSE),
                        'default_v3'            = if_else(dplyr::last(cd_zero_bal) >= 9 & sum(delq_sts >= 3, na.rm = TRUE) > 0, TRUE, FALSE)) %>%
          filter(dplyr::row_number() >= (n() - 11)) %>%  
          dplyr::mutate('dt_default_v1'        = if_else(dplyr::first(na.omit(default_v1)), dplyr::first(svcg_cycle[default_v1==TRUE]), as.Date(NA)),
                        'dt_default_v2'        = if_else(dplyr::first(na.omit(default_v2)), dplyr::first(svcg_cycle[default_v2==TRUE]), as.Date(NA)),
                        'dt_default_v3'        = if_else(dplyr::first(na.omit(default_v3)), dplyr::first(svcg_cycle[default_v3==TRUE]), as.Date(NA)),
                        '#current_l12' = sum(delq_sts == 0, na.rm = TRUE),
                        '#30_dl_l12' = sum(delq_sts == 1, na.rm = TRUE),
                        '#60_dl_l12' = sum(delq_sts >= 2, na.rm = TRUE)) %>%
          filter(row_number() >= (n())) 
        
        
        
        # Merge origination- and performance feature data
        Final <- left_join(origination, performance_features, by = "id_loan") #%>% 
        # drop_na(current_upb, delq_sts, current_int_rt, current_upb, cd_zero_bal, default)
        
        variable_names <- colnames(Final)
        readr::write_delim(Final, path = paste0(getwd(), "/data/Final_v2_", i, '.txt'), col_names = FALSE, delim = "|")
      }
      
    }
  return(variable_names)}



# performance function for evaluation the models
perform_class = function (pre_class,y,classlabels) {
  confus=table(pre_class,y)
  TN=sum(pre_class==y & y==classlabels[1])
  TP=sum(pre_class==y & y==classlabels[2])
  FN=sum(pre_class!=y & y==classlabels[2])
  FP=sum(pre_class!=y & y==classlabels[1])
  
  specificity=TN/(TN+FP)
  sensitivity=TP/(TP+FN)
  ppv = TP / (TP+FP)
  npv = TN / (TN+FN)
  accuracy=(TN+TP)/(TN+TP+FN+FP)
  return(list(confus=confus,specificity=specificity,sensitivity=sensitivity,accuracy=accuracy, ppv = ppv, npv = npv))
}
