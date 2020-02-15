#Clearing variables
#rm(list=ls())

#Setting working directory to where the Rscrip is located
# setwd(here())

# obtain a vector of all avalible origination- and performance files in the folder
# origination_files = list.files(path=paste0(getwd(), "/data/origination"), pattern="*.txt", full.names=TRUE)
# performance_files = list.files(path=paste0(getwd(), "/data/performance"), pattern="*.txt", full.names=TRUE)
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
      origination <- read_delim(origination_files[i], 
                                "|", escape_double = FALSE, col_names = origination_names, 
                                trim_ws = TRUE)
      
      # Loading the performance dataset
      performance <- read_delim(paste0(performance_files[i]), 
                                "|", escape_double = FALSE, col_names = performance_names, 
                                trim_ws = TRUE)
      
      
      origination <- origination %>% 
        select(id_loan, dt_first_pi, everything()) %>% 
        mutate(dt_first_pi     = as.Date(paste0(as.character(dt_first_pi),"01"), format = "%Y%m%d"),
               dt_matr = as.Date(paste0(as.character(dt_matr),"01"), format = "%Y%m%d"))
      
      performance <- performance %>% 
        mutate(svcg_cycle = as.Date(paste0(as.character(svcg_cycle),"01"), format = "%Y%m%d"))
      
      # calculate performance features 
      performance$temp <- if_else(performance$delq_sts == 1, performance$svcg_cycle, as.Date(NA))
      
      performance_features <- performance %>% 
        dplyr::group_by(id_loan) %>% 
        dplyr::mutate(prev = lag(delq_sts, order_by=id_loan),
                      survived = ifelse(prev != 0 & delq_sts == 0, 1, 0)) %>%
        dplyr::summarize(delic_mean   = mean(delq_sts, na.rm=T),
                         delic_binary = mean(delq_sts, na.rm=T) > 0,
                         delic_date   = dplyr::first(na.omit(temp)),
                         surv_binary  = mean(survived, na.rm=T) > 0,
                         surv         = sum(survived, na.rm=T),
                         max_unpaid   = max(current_upb, na.rm=T),
                         recovered    = last(delq_sts) == 0)  %>% 
        dplyr::mutate(first_complete_stop = ifelse(delic_binary == TRUE & surv_binary == FALSE, TRUE, FALSE),
                      recovered           = ifelse(delic_binary == TRUE & recovered == TRUE, TRUE, FALSE))
      
      
      # Merge origination- and performance feature data
      FM_data <- left_join(origination, performance_features, by = "id_loan")
      
      variable_names <- colnames(FM_data)
      readr::write_delim(FM_data, path = paste0(getwd(), "/data/Final_", i + START_YEAR - 1, '.txt'), col_names = FALSE, delim = "|")
    }}
  return(variable_names)}


