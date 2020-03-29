library(DBI)
# Connect to a specific postgres database i.e. Heroku
con <- dbConnect(RPostgres::Postgres(),dbname = 'postgres', 
                 host = '104.197.126.151', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'postgres',
                 password = 'yfe47mns')

dbListTables(con)


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
origination_files <- list.files(path=paste0(getwd(), "/data"), pattern="^sample_orig.*\\.txt", full.names=TRUE)
performance_files <- list.files(path=paste0(getwd(), "/data"), pattern="^sample_svcg.*\\.txt", full.names=TRUE)

# origination_files <- list.files(path=paste0(getwd(), "/data"), pattern="^historical_data1_Q.*\\.txt", full.names=TRUE)
# performance_files <- list.files(path=paste0(getwd(), "/data"), pattern="^historical_data1_time_.*\\.txt", full.names=TRUE)




for (i in 1:length(origination_files)) {
  
  #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #  
  # - - - - - - - - - - Origination data - - - - - - - - - - # 
  #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  origination = data.table::fread(origination_files[i], sep="|", header=FALSE, stringsAsFactors = FALSE, colClasses=origination_classes)  
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
  

  
  #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #  
  # - - - - - - - - - - Performance data - - - - - - - - - - # 
  #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  
  performance = data.table::fread(performance_files[i], sep="|", header=FALSE, stringsAsFactors = FALSE, colClasses=performance_classes)  
  names(performance) <- performance_names
  performance %<>% 
    mutate(delq_sts = replace(delq_sts, delq_sts == '   ', NA),
           repch_flag = replace(repch_flag, repch_flag == ' ', NA),
           svcg_cycle = as.Date(paste0(as.character(svcg_cycle),"01"), format = "%Y%m%d"))
  
  #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #  
  # - - - - - - - - - - Add to database  - - - - - - - - - - # 
  #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  
  if (i==1) {
    DBI::dbCreateTable(con, "origination", origination, overwrite = TRUE)
    DBI::dbCreateTable(con, "performance", performance, overwrite = TRUE)
  }else {
    DBI::dbAppendTable(conn = con, name = "origination", value = origination)
    DBI::dbAppendTable(conn = con, name = "performance", value = performance)
    
  }
     
  
}


#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #  
#  - - - - - - - Create Table final table  - - - - - - - - # 
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
dbRemoveTable(con, "final")
DBI::dbExecute(con, "create table Final as select t1.id_loan,
                                                  t1.dt_first_pi,
                                                  t1.fico,
                                                  t1.flag_fthb,
                                                  t1.dt_matr,
                                                  t1.cd_msa,
                                                  t1.mi_pct,
                                                  t1.cnt_units,
                                                  t1.occpy_sts,
                                                  t1.cltv,
                                                  t1.dti,
                                                  t1.orig_upb,
                                                  t1.ltv,
                                                  t1.int_rt,
                                                  t1.channel,
                                                  t1.ppmt_pnlty,
                                                  t1.prod_type,
                                                  t1.zipcode,
                                                  t1.loan_purpose,
                                                  t1.orig_loan_term,
                                                  t1.cnt_borr,
                                                  t1.seller_name,
                                                  t1.servicer_name,
                                                  t2.svcg_cycle,
                                                  t2.current_upb,
                                                  t2.delq_sts,
                                                  t2.loan_age,
                                                  t2.mths_remng,
                                                  t2.repch_flag,
                                                  t2.flag_mod,
                                                  t2.cd_zero_bal,
                                                  t2.dt_zero_bal,
                                                  t2.current_int_rt,
                                                  t2.non_int_brng_upb,
                                                  t2.dt_lst_pi,
                                                  t2.mi_recoveries,
                                                  t2.net_sale_proceeds,
                                                  t2.non_mi_recoveries,
                                                  t2.expenses,
                                                  t2.legal_costs,
                                                  t2.maint_pres_costs,
                                                  t2.taxes_ins_costs,
                                                  t2.misc_costs,
                                                  t2.actual_loss,
                                                  t2.modcost,
                                                  t2.stepmod_ind,
                                                  t2.dpm_ind,
                                                  t2.eltv
               from origination t1 LEFT JOIN performance t2 ON t1.id_loan = t2.id_loan")
# Take a look at a single loan 
dbGetQuery(con, "select * from Final where id_loan = \'F100Q1000012\' ")

dbRemoveTable(con, "final2")
DBI::dbExecute(conn = con, "CREATE TABLE FINAL2 AS SELECT * FROM FINAL WHERE id_loan =\'F100Q1000012\' ")

dbRemoveTable(con, "final3")
DBI::dbExecute(conn = con, "CREATE TABLE FINAL3 AS SELECT *, 
                                                          CASE
                                                            WHEN (cd_zero_bal ='01') THEN 'T'
                                                            ELSE 'F'
                                                          END AS Default
                            FROM FINAL2")

dbReadTable(con, "final3")

DBI::dbExecute(conn = con, "ALTER TABLE final2 ADD \'default\' ")






dbListFields(conn = con, 'performance')
dbListTables(con)
dbReadTable(con, "final2")


dbExistsTable(con, "Final") # Ok Final Table exists



dbGetQuery(conn = con, "select * from performance")


 dbGetQuery(con, "select * from performance limit 5")







#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #  
#  - - - - - - - - - - - - - END - - - - - - - - - - - - - #  
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #  


 
 
 #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #  
 #  - - - - - - - - - Build Final table - - - - - - - - - - #  
 #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - #  
 
 dbListTables(con)
 dbRemoveTable(con, 'final')

  DBI::dbExecute(conn = con, SQL('create table FINAL as SELECT (t1.*, t2.*) FROM origination t1 WHERE performance.id_loan = \'F100Q1000004\'
                                 left join performance t2 ON t1.id_loan = t2.id_loan'))
 
 DBI::dbExecute(conn = con, SQL('SELECT * FROM FINAL LEFT JOIN performance t1 ON FINAL.id_loan = t1.id_loan'))
 
 dbReadTable(con, "final")

dbGetQuery(con,'SELECT * FROM performance limit 5')

DBI::dbRemoveTable(conn = con, 'origination')


