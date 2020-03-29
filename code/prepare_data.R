
prepare_data_v2 <- function(START_YEAR, END_YEAR, sample=TRUE) {
  
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
  
  # find origination and perofrmance data files 
  # origination_files <- list.files(path=paste0(getwd(), "/data"), pattern="^sample_orig.*\\.txt", full.names=TRUE)
  # performance_files <- list.files(path=paste0(getwd(), "/data"), pattern="^sample_svcg.*\\.txt", full.names=TRUE)
  
  origination_files <- list.files(path=paste0(getwd(), "/data"), pattern="^historical_data1_Q.*\\.txt", full.names=TRUE)
  performance_files <- list.files(path=paste0(getwd(), "/data"), pattern="^historical_data1_time_.*\\.txt", full.names=TRUE)
  
  #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #  
  # - - - - - - - - - - Origination data - - - - - - - - - - # 
  #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  
  origination = as_tibble(ldply(origination_files, fread, sep="|", header=FALSE, stringsAsFactors = FALSE, colClasses=origination_classes))
  names(origination) <- origination_names
  origination %<>% select(id_loan, dt_first_pi, everything())  %>% #drop_na(fico, orig_upb, int_rt) %>%
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
  
  # Handling Missing values
  (Missing <- origination %>% 
      select(everything()) %>%  # replace to your needs
      summarise_all(funs(sum(is.na(.)))))
  
  origination%<>% drop_na(fico)
  origination %<>% mutate(cltv         = replace_na(cltv, stats::median(cltv, na.rm = T)),
                          dti          = replace_na(dti,  stats::median(dti, na.rm = T)),
                          ltv          = replace_na(dti,  stats::median(ltv, na.rm = T)),
                          prop_type    = replace_na(prop_type,  'U'),
                          cnt_borr     = replace_na(cnt_borr, 1))
  
  
  #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #  
  # - - - - - - - - - - Performance data - - - - - - - - - - # 
  #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  
  performance = as_tibble(ldply(performance_files, fread, sep="|", header=FALSE, stringsAsFactors = FALSE, colClasses=performance_classes))
  names(performance) <- performance_names
  
  performance %<>% 
    mutate(delq_sts = replace(delq_sts, delq_sts == '   ', NA),
           repch_flag = replace(repch_flag, repch_flag == ' ', NA),
           svcg_cycle = as.Date(paste0(as.character(svcg_cycle),"01"), format = "%Y%m%d"))
  
  performance$cd_zero_bal <- as.numeric(performance$cd_zero_bal)
  
  # create temporary variables
  performance$temp   = if_else(performance$delq_sts >= 3, performance$svcg_cycle, as.Date(NA))
  performance$temp_2 = if_else(performance$delq_sts == 1, performance$svcg_cycle, as.Date(NA))
  performance$temp_3 = if_else(performance$delq_sts == 1, performance$loan_age, as.numeric(NA))
  performance$temp_4 = if_else(performance$delq_sts >= 3, performance$loan_age, as.numeric(NA))
  
  
  
  performance_features <- performance %>% 
    dplyr::group_by('F199Q1030459') %>%
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
                  'default_v1'            = if_else(dplyr::last(delq_sts) != 0 & sum(delq_sts >= 3, na.rm = TRUE) > 0 , TRUE, FALSE),
                  'default_v2'            = if_else(dplyr::last(cd_zero_bal) != 1 & sum(delq_sts >= 3, na.rm = TRUE) > 0, TRUE, FALSE),
                  'default_v3'            = if_else(dplyr::last(cd_zero_bal) >= 9 & sum(delq_sts >= 3, na.rm = TRUE) > 0, TRUE, FALSE)) %>%
    filter(dplyr::row_number() >= (n() - 11)) %>%  
    dplyr::mutate('dt_default_v1'        = if_else(dplyr::first(na.omit(default_v1)), dplyr::first(svcg_cycle[default_v1==TRUE]), as.Date(NA)),
                  'dt_default_v2'        = if_else(dplyr::first(na.omit(default_v2)), dplyr::first(svcg_cycle[default_v2==TRUE]), as.Date(NA)),
                  'dt_default_v3'        = if_else(dplyr::first(na.omit(default_v3)), dplyr::first(svcg_cycle[default_v3==TRUE]), as.Date(NA)),
                  '#current_l12' = sum(delq_sts == 0, na.rm = TRUE),
                  '#30_dl_l12' = sum(delq_sts == 1, na.rm = TRUE),
                  '#60_dl_l12' = sum(delq_sts >= 2, na.rm = TRUE)) #%>%
  #filter(row_number() >= (n())) 
  
  
  
  
  
  return(variable_names)}