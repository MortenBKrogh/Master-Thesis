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

  origination_files <- list.files(path=paste0(getwd(), "/data"), pattern="^sample_orig.*\\.txt", full.names=TRUE)
  performance_files <- list.files(path=paste0(getwd(), "/data"), pattern="^sample_svcg.*\\.txt", full.names=TRUE)

  origination = as_tibble(ldply(origination_files, fread, sep="|", header=FALSE, stringsAsFactors = FALSE, colClasses=origination_classes))
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
  performance = as_tibble(ldply(performance_files, fread , sep="|", header=FALSE, stringsAsFactors = FALSE, colClasses=performance_classes))
  names(performance) <- performance_names
  performance %<>%
    mutate(delq_sts = replace(delq_sts, delq_sts == '   ', NA),
           repch_flag = replace(repch_flag, repch_flag == ' ', NA),
           svcg_cycle = as.Date(paste0(as.character(svcg_cycle),"01"), format = "%Y%m%d"))  