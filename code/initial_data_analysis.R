# FM_data is loaded in the MAIN.R file.

var_names <- c("id_loan", "dt_first_pi", "fico", "flag_fthb", "dt_matr", "cd_msa", 
               "mi_pct", "cnt_units", "occpy_sts", "cltv", "dti", "orig_upb", 
               "ltv", "int_rt", "channel", "ppmt_pnlty", "prod_type", "st", 
               "prop_type", "zipcode", "loan_purpose", "orig_loan_term", "cnt_borr", 
               "seller_name", "servicer_name", "flag_sc", "svcg_cycle", "current_upb", 
               "delq_sts", "loan_age", "mths_remng", "repch_flag", "flag_mod", 
               "cd_zero_bal", "dt_zero_bal", "current_int_rt", "non_int_brng_upb", 
               "dt_lst_pi", "mi_recoveries", "net_sale_proceeds", "non_mi_recoveries", 
               "expenses", "legal_costs", "maint_pres_costs", "taxes_ins_costs", 
               "misc_costs", "actual_loss", "modcost", "stepmod_ind", "dpm_ind", 
               "eltv", "temp", "temp_2", "temp_3", "temp_4", "prev", "survived", 
               "#30_dl", "#60_dl", "#90+_dl", "first_dt_delq", "first_delq_age", 
               "first_delq_mths_remng", "first_eltv", "last_eltv", "surv_binary", 
               "#surv", "recovered", "rt_change", "default_v0", "dt_default_v0", 
               "default_v1", "dt_default_v1", "#current_l12", "#30_dl_l12", 
               "#60_dl_l12")

FM_data_files <- list.files(path=paste0(getwd(), "/data"), pattern="^Final_v2_.*\\.txt", full.names=TRUE)
FM_data <- ldply(FM_data_files, data.table::fread, sep = "|", header = FALSE, col.names = var_names, stringsAsFactors = FALSE)
sum(is.na(FM_data$default_v1))

# first take a look at how many defaults and how many does not default 
table(FM_data$default_v1)
table(FM_data$default_v2)


# choose default definition 1) or 2) 
FM_data$'default'    <- FM_data$default_v0
FM_data$'dt_default' <- FM_data$dt_default_v0

data <- subset(FM_data, !is.na(FM_data$default)) %>% 
  select(default, dt_default, fico, flag_fthb, mi_pct, cnt_units,
         occpy_sts, cltv, dti, orig_upb, ltv, int_rt,
         channel, st, prop_type, prod_type, loan_purpose, delq_sts,
         orig_loan_term, cnt_borr, seller_name,
         servicer_name, current_int_rt, loan_age,
         mths_remng, cd_zero_bal, `#30_dl`,
         `#60_dl`, first_dt_delq, first_delq_age, first_delq_mths_remng, 
         first_eltv, last_eltv, `#surv`, recovered, rt_change, 
         #default_v0, dt_default_v0, default_v1, dt_default_v1,
         `#current_l12`, `#30_dl_l12`) # %>% drop_na()




# define theme 
base    <-  "#08306b"
basem1  <- "#4f6980"
basem2  <- "#849db1"
basem3  <- '#003d85'
basem4  <- '#FF7E00' #FF8C00'# '#ff9c0f'
basevec <- c("#4f6980","#849db1","#a2ceaa","#638b66","#bfbb60","#f47942","#fbb04e","#b66353","#d7ce9f", "#b9aa97","#7e756d")


thd <- theme(plot.title        = element_text(size  = 10),
             plot.background   = element_blank(), #element_rect(fill  = "#f3f3f3", color = "#f3f3f3"),
             panel.background  = element_blank(), #element_rect(fill  = "#f3f3f3", color = NA), 
             legend.background = element_blank(), #element_rect(fill  = "#f3f3f3", color = NA),
             legend.key        = element_rect(fill  = "#f3f3f3", color = NA),
             legend.position   = 'bottom', 
             strip.background  = element_blank(), #element_rect(fill  = "#f3f3f3", color = NA),
             panel.border      = element_blank(), #element_rect(fill  = NA,        color = "black", size = 0.3),
             panel.grid.major  = element_blank(), #element_line(colorr ="#003d85"), 
             panel.grid.minor  = element_line(color ="#f3f3f3"), 
             title             = element_text(color = "black"),
             plot.subtitle     = element_text(color = "grey40", size = 8),
             plot.caption      = element_text(color = "grey70"),
             strip.text        = element_text(face  = "bold"),
             axis.text         = element_text(color = "black"),
             axis.ticks        = element_line(color = "black"))

th <- thd + theme(panel.grid.major  = element_blank(), 
                  panel.grid.minor  = element_blank())



# plot 1) fico vs. default rate 
dat <- data %>% select(fico, default) %>% # drop_na(fico)  %>%
  #mutate(fico_1 = if_else(fico < 550, 550, 0)) #%>%
  dplyr::count(fico, default) %>%  
  group_by(fico) %>% 
  dplyr::mutate(total = sum(n, na.rm = T)) %>%
  dplyr::mutate(pct   = n / total * 100) %>%
  filter(default == T, fico > 500) 


p1 <- ggplot(data = dat, aes(y = pct, x = fico, group = 1 )) + geom_line(color=basem3) +
  thd +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = 'Default rate',
       subtitle = 'By Credit Score',
       #caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = '',
       y = '%') +th+ scale_fill_manual(values = c(basem3, basem4)) 

# p1
# %>% mutate(interval = if_else(between(fico, 350, 400), 350,
#                                                         if_else(between(fico, 401, 450), 400,
#                                                         if_else(between(fico, 451, 500), 450,
#                                                         if_else(between(fico, 501, 550), 500,
#                                                         if_else(between(fico, 551, 600), 550,
#                                                         if_else(between(fico, 601, 650), 600,
#                                                         if_else(between(fico, 651, 700), 650,
#                                                         if_else(between(fico, 701, 750), 700,
#                                                         if_else(between(fico, 751, 800), 750,
#                                                         if_else(between(fico, 801, 850), 800,
#                                                         if_else(between(fico, 350, 600), 850, 0))))))))))))


# plot 2) ltv vs. default rate

dat <- data %>% select(ltv, default) %>% # drop_na(fico)  %>%
  #mutate(fico_1 = if_else(fico < 550, 550, 0)) #%>%
  dplyr::count(ltv, default) %>%  
  group_by(ltv) %>% 
  dplyr::mutate(total = sum(n, na.rm = T)) %>%
  dplyr::mutate(pct   = n / total * 100) %>%
  filter(default == T) 


p2 <- ggplot(data = dat, aes(y = pct, x = ltv, group = 1 )) + geom_line(color=basem3) +
  thd +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = 'Default rate',
       subtitle = 'By Loan To Value',
       caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = '',
       y = '%') +th+ scale_fill_manual(values = c(basem3, basem4)) 

# p1
library(patchwork)
(p1 | p2) #/ (p2)







#########################################################################################################
#########################################################################################################
#########################################################################################################



# Load 'raw' data
# var_names <- c("id_loan","dt_first_pi","fico","flag_fthb","dt_matr","cd_msa","mi_pct","cnt_units",
#                "occpy_sts","cltv","dti","orig_upb","ltv","int_rt","channel","ppmt_pnlty",
#                "prod_type","st","prop_type","zipcode","loan_purpose","orig_loan_term","cnt_borr","seller_name",
#                "servicer_name","flag_sc","default","current","delq_sts","current_upb","current_int_rt","loan_age",
#                "mths_remng","cd_zero_bal","#current","#30_dl","#60_dl","#90+_dl","#current_l12","#30_dl_l12",
#                "#60_dl_l12","#90+_dl_l12")

# var_names for Final_v2
var_names <- c("id_loan", "dt_first_pi", "fico", "flag_fthb", "dt_matr", "cd_msa", 
        "mi_pct", "cnt_units", "occpy_sts", "cltv", "dti", "orig_upb", 
        "ltv", "int_rt", "channel", "ppmt_pnlty", "prod_type", "st", 
        "prop_type", "zipcode", "loan_purpose", "orig_loan_term", "cnt_borr", 
        "seller_name", "servicer_name", "flag_sc", "svcg_cycle", "current_upb", 
        "delq_sts", "loan_age", "mths_remng", "repch_flag", "flag_mod", 
        "cd_zero_bal", "dt_zero_bal", "current_int_rt", "non_int_brng_upb", 
        "dt_lst_pi", "mi_recoveries", "net_sale_proceeds", "non_mi_recoveries", 
        "expenses", "legal_costs", "maint_pres_costs", "taxes_ins_costs", 
        "misc_costs", "actual_loss", "modcost", "stepmod_ind", "dpm_ind", 
        "eltv", "temp", "temp_2", "temp_3", "temp_4", "prev", "survived", 
        "#30_dl", "#60_dl", "#90+_dl", "first_dt_delq", "first_delq_age", 
        "first_delq_mths_remng", "first_eltv", "last_eltv", "surv_binary", 
        "#surv", "recovered", "rt_change", "default_v0", "dt_default_v0", 
        "default_v1", "dt_default_v1", "#current_l12", "#30_dl_l12", 
        "#60_dl_l12")

FM_data_files <- list.files(path=paste0(getwd(), "/data"), pattern="^Final_v2_.*\\.txt", full.names=TRUE)
data <- ldply(FM_data_files, data.table::fread, sep = "|", header = FALSE, col.names = var_names, stringsAsFactors = FALSE) %>%
                select(dt_first_pi, dt_matr, fico, delq_sts,flag_fthb, occpy_sts, cltv, dti, orig_upb,
                       ltv, int_rt, channel, ppmt_pnlty, prod_type, st, prop_type, loan_purpose, 
                       orig_loan_term, cnt_borr, seller_name, servicer_name, current_upb,
                       loan_age, cd_zero_bal, 
                       `#30_dl`, `#60_dl`,  first_dt_delq, first_delq_age, first_delq_mths_remng,
                       first_eltv, last_eltv, rt_change, default_v0, dt_default_v0, 
                       default_v1, dt_default_v1, `#30_dl_l12`)# %>%
                drop_na(current_upb, delq_sts, cd_zero_bal, default_v1)

# data <- subset(data, !is.na(data$default)) %>%
#   #select(-excluded_vars)
#   select(default, fico, flag_fthb, mi_pct, cnt_units,
#          occpy_sts, cltv, dti, orig_upb, ltv, int_rt,
#          channel, st, prop_type, prod_type, loan_purpose, delq_sts,
#          orig_loan_term, cnt_borr, seller_name,
#          servicer_name, current_int_rt, loan_age,
#          mths_remng, cd_zero_bal, current, `#current`, `#30_dl`,
#          `#60_dl`, `#current_l12`, `#30_dl_l12`, dt_default) # %>% drop_na() #%>% 
# # drop_na(current_upb, delq_sts, current_int_rt, current_upb, cd_zero_bal, default)




# define theme 
base    <-  "#08306b"
basem1  <- "#4f6980"
basem2  <- "#849db1"
basem3  <- '#003d85'
basem4  <- '#FF7E00' #FF8C00'# '#ff9c0f'
basevec <- c("#4f6980","#849db1","#a2ceaa","#638b66","#bfbb60","#f47942","#fbb04e","#b66353","#d7ce9f", "#b9aa97","#7e756d")


thd <- theme(plot.title        = element_text(size  = 10),
             plot.background   = element_blank(), #element_rect(fill  = "#f3f3f3", color = "#f3f3f3"),
             panel.background  = element_blank(), #element_rect(fill  = "#f3f3f3", color = NA), 
             legend.background = element_blank(), #element_rect(fill  = "#f3f3f3", color = NA),
             legend.key        = element_rect(fill  = "#f3f3f3", color = NA),
             legend.position   = 'bottom', 
             strip.background  = element_blank(), #element_rect(fill  = "#f3f3f3", color = NA),
             panel.border      = element_blank(), #element_rect(fill  = NA,        color = "black", size = 0.3),
             panel.grid.major  = element_blank(), #element_line(color ="#003d85"), 
             panel.grid.minor  = element_line(color ="#f3f3f3"), 
             title             = element_text(color = "black"),
             plot.subtitle     = element_text(color = "grey40", size = 8),
             plot.caption      = element_text(color = "grey70"),
             strip.text        = element_text(face  = "bold"),
             axis.text         = element_text(color = "black"),
             axis.ticks        = element_line(color = "black"))

th <- thd + theme(panel.grid.major  = element_blank(), 
                  panel.grid.minor  = element_blank())
# 


# plot skewness of data
###
p_fico_density <- ggplot(data = data, aes(x = fico, fill = default_v1)) +
  geom_bar(width = 1, alpha = 0.7,position = "identity", stat = 'density', na.rm = F) +
  scale_color_manual(values=c(basem3, basem4)) + scale_fill_manual(values=c(basem3, basem)) +
  labs(title = 'Credit Score',
       subtitle = 'Density of credit score of the groups default and non-default.',
       caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = 'Credit Score',
       y = 'Frequency') + thd

p_ltv_density <- ggplot(data = FM_data, aes(x = ltv, fill = default)) +
  geom_bar(width = 1, alpha = 0.7,position = "identity", stat = 'density', na.rm = F) +
  scale_color_manual(values=c(basem3, basem4)) + scale_fill_manual(values=c(basem3, basem)) +
  labs(title = 'Loan-To-Value',
       subtitle = 'Density of loan-to-value of the groups default and non-default.',
       caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = 'Loan-To-Value',
       y = 'Frequency') + thd


p_dti_density <- ggplot(data = FM_data, aes(x = dti, fill = default)) +
  geom_bar(width = 1, alpha = 0.7,position = "identity", stat = 'density', na.rm = F) +
  scale_color_manual(values=c(basem3, basem4)) + scale_fill_manual(values=c(basem3, basem)) +
  labs(title = 'Debt-To-Income',
       subtitle = 'Density of debt-to-income of the groups default and non-default.',
       caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = 'Debt-To-Income',
       y = 'Frequency') + thd


library(patchwork)

pw_1 <- (p_fico_density | p_ltv_density | p_dti_density )  & theme(legend.position = "bottom") 
# Remove title from second subplot
pw_1[[1]] = pw_1[[1]] + theme(plot.title = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.y = element_blank(),
                              plot.subtitle = element_blank(),
                              plot.caption = element_blank())
pw_1[[2]] = pw_1[[2]] + theme(plot.title = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.y = element_blank(),
                              plot.subtitle = element_blank(),
                              plot.caption = element_blank())

# Remove title from third subplot
pw_1[[3]] = pw_1[[3]] + theme(plot.title = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.y = element_blank(),
                              plot.subtitle = element_blank(),
                              #plot.caption = element_blank()
)
pw_1 + plot_annotation(tag_levels = "I")+ plot_layout(guides="collect") 


### --------------------------------------- ###
#  y = default_rate, x = fico 
### --------------------------------------- ###


dat <- data %>% select(fico, default_v0) %>% filter(!is.na(default_v1)) #group_by(fico)  %>%  dplyr::count(fico, default_v0) #filter(!is.na(default_v1)) #%>% group_by(fico)
  
  dplyr::count(fico, default_v1) %>%
                                                      mutate(n_pct = n / sum(n) * 100) %>%
  ggplot(aes(fico, n_pct)) + geom_line()

p_sell <- data %>% filter(!is.na(default))%>% dplyr::count(seller_name, default) %>%
  mutate(n_pct = n / sum(n) * 100) %>% filter(seller_name %in% topsell$seller_name) %>% 
  ggplot(aes(reorder(seller_name,n_pct), n_pct)) + 
  geom_col(aes(fill=default)) + coord_flip() + labs(title = "Seller", x = NULL, y = '%',fill = "Default") + 
  th + theme(legend.position = 'none')+ scale_fill_manual(values = c(basem3, basem4))


###
dat <- FM_data %>% 
  group_by(date = substr(as.character(FM_data$dt_first_pi), start = 1, stop = 4)) %>% 
  transmute(default_rate = sum(default == TRUE, na.rm = T) /  n() * 100) %>% 
  filter(row_number() >= n()) 



P_orig_dr <- ggplot(data = dat, aes(x = date, y = default_rate, group = 1)) +
  geom_line(color="#003d85", size = 1, linetype = 'dashed') +
  geom_point(color="#ff9c0f", size=2) +
  thd +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = 'Default rate',
       subtitle = 'By origination date',
       caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = '',
       y = '%')




dat <- FM_data %>% 
  group_by(date = substr(as.character(dt_default), start = 1, stop = 4)) %>% 
  transmute(default_rate = sum(default == TRUE, na.rm = T) ) %>%
  #sum = n()) #sum(default == FALSE | TRUE, na.rm = T))# %>% # (n() - sum(is.na(FM_data$default))) * 100)# %>% 
  filter(row_number() >= (n())) %>% drop_na

P_default_dt <- ggplot(data = dat, aes(x = date, y = default_rate, group = 1)) +
  geom_line(color="#003d85", size = 1, linetype = 'dashed') +
  geom_point(color="#ff9c0f", size=2) +
  thd +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = 'Number of Defaults pr. year',
       subtitle = 'By default date',
       caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = '',
       y = '#')

P_default_dt

dat <- FM_data %>% 
  group_by(date = substr(as.character(dt_delq), start = 1, stop = 4)) %>% 
  transmute(default_rate = sum(delq_sts == 1, na.rm = T) ) %>%
  #sum = n()) #sum(default == FALSE | TRUE, na.rm = T))# %>% # (n() - sum(is.na(FM_data$default))) * 100)# %>% 
  filter(row_number() >= (n())) %>% drop_na

P_delq_dt <- ggplot(data = dat, aes(x = date, y = default_rate, group = 1)) +
  geom_line(color="#003d85", size = 1, linetype = 'dashed') +
  geom_point(color="#ff9c0f", size=2) +
  thd +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = 'Number of delinquencies pr. year',
       subtitle = 'By delinquency date',
       caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = '',
       y = '#')

P_delq_dt


pw_2 = (P_orig_dr / P_default_dt / P_delq_dt)

# Remove title from second subplot
pw_2[[1]] = pw_2[[1]] + theme(axis.text.x = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              #axis.title.y = element_blank(),
                              plot.subtitle = element_blank(),
                              plot.caption = element_blank())

# Remove title from third subplot
pw_2[[2]] = pw_2[[2]] + theme(axis.text.x = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              #axis.title.y = element_blank(),
                              plot.subtitle = element_blank(),
                              plot.caption = element_blank())
pw_2[[3]] = pw_2[[3]] + theme(#axis.text.x = element_blank(),
  #axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  #axis.title.y = element_blank(),
  plot.subtitle = element_blank())
#plot.caption = element_blank())
pw_2 <- pw_2 + 
  plot_annotation(#title ='Credit Behavior',
    #tag_levels = "I",
    theme = theme(plot.margin     = unit(c(.5, 0.1, 0.1, 0.1), "cm"))) + 
  plot_layout(guides="collect")#, axis.title.x="collect") 
pw_2
ggsave(pw_2, filename = "figures/pw_2.pdf", width=8, height=6, dpi=600)

data %>% dplyr::count(default)
data %>% dplyr::count(delq_sts == 6)

data %>% 
  # 
  library(dplyr)
# First the categorical variables
data <- data_model

p_fthb <- data %>% filter(flag_fthb != 9) %>% dplyr::count(flag_fthb,default_v1) %>% mutate(n_pct = n / sum(n) * 100) %>% ggplot(aes(flag_fthb, n_pct)) + geom_col(aes(fill=default_v1), show.legend = F) + labs(title="New Homeowner", x = NULL, y = '%') + th+ scale_fill_manual(values = c(basem3, basem4))
#p_fthb <- p_fthb %>% ggplot(aes(flag_fthb, n_pct)) + geom_col(aes(fill=default_v1), show.legend = F) + labs(title="New Homeowner", x = NULL, y = '%') + th+ scale_fill_manual(values = c(basem3, basem4))

p_unit <- data %>% filter(cnt_units != 99) %>% dplyr::count(cnt_units,default_v1) %>% mutate(n_pct = n / sum(n) * 100) 
p_unit <- p_unit  %>%  ggplot(aes(cnt_units, n_pct)) + geom_col(aes(fill=default_v1), show.legend = F) + labs(title="Number of Units", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))

p_occ  <- data %>% dplyr::count(occpy_sts,default_v1) %>%mutate(n_pct = n / sum(n) * 100) 
p_occ  <- p_occ   %>%  ggplot(aes(occpy_sts, n_pct)) + geom_col(aes(fill=default_v1), show.legend = F) + labs(title="Ocupancy Status", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
  
p_chan <- data %>% dplyr::count(channel,default_v1) %>% mutate(n_pct = n / sum(n) * 100)  ggplot(aes(channel, n_pct)) + geom_col(aes(fill=default_v1), show.legend = F)+ labs(title="Channel", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_chan <- p_chan %>% ggplot(aes(channel, n_pct)) + geom_col(aes(fill=default_v1), show.legend = F)+ labs(title="Channel", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4)) 
  
p_prod <- data %>% dplyr::count(prod_type,default_v1) %>% mutate(n_pct = n / sum(n) * 100) %>% ggplot(aes(prod_type, n_pct)) + geom_col(aes(fill=default_v1), show.legend = F)+ labs(title="Loan Type", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4)) 
p_prop <- data %>%  dplyr::count(prop_type,default_v1) %>% mutate(n_pct = n / sum(n) * 100) %>% drop_na() %>% ggplot(aes(prop_type, n_pct)) + geom_col(aes(fill=default_v1), show.legend = F)+ labs(title="Property Type", x = NULL, y = '%') + th+ scale_fill_manual(values = c(basem3, basem4))
p_purp <- data %>% dplyr::count(loan_purpose,default_v1) %>% mutate(n_pct = n / sum(n) * 100) %>% ggplot(aes(loan_purpose, n_pct)) + geom_col(aes(fill=default_v1), show.legend = F)+ labs(title="Purpose of Loan", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_term <- data %>% dplyr::count(orig_loan_term,default_v1) %>% filter(n>200) %>% mutate(n_pct = n / sum(n) * 100) %>% ggplot(aes(as.factor(orig_loan_term), n_pct)) + geom_col(aes(fill=default_v1))+ labs(title="Number of Terms", x = NULL, y = NULL, fill="default") + 
  th + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values = c(basem3, basem4))
p_borr <- data %>% filter(cnt_borr != 99) %>% dplyr::count(cnt_borr,default_v1) %>% mutate(n_pct = n / sum(n) * 100) %>% ggplot(aes(cnt_borr, n_pct)) + geom_col(aes(fill=default_v1), show.legend = F) + labs(title="Number of Borrowers", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))

(p_fthb | p_unit | p_occ | p_chan | p_purp | p_prod) / (p_prop | p_term | p_borr) + 
  plot_annotation(#title ='Credit Behavior',
    caption = 'Data: Freddie Mac Single Family Loan-Level 2019.',
    #labs(y = '%'),
    tag_levels = "I",
    theme = theme(plot.margin     = unit(c(.5, 0.1, 0.1, 0.1), "cm")))



# p_fthb <- data %>% filter(flag_fthb != 9) %>% dplyr::count(flag_fthb,default) %>% ggplot(aes(flag_fthb, n/1000)) + geom_col(aes(fill=default), show.legend = F) + labs(title="New Homeowner", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
# p_unit <- data %>% filter(cnt_units != 99) %>% dplyr::count(cnt_units,default) %>% ggplot(aes(cnt_units, n/1000)) + geom_col(aes(fill=default), show.legend = F) + labs(title="Number of Units", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
# p_occ  <- data %>% dplyr::count(occpy_sts,default) %>% ggplot(aes(occpy_sts, n/1000)) + geom_col(aes(fill=default), show.legend = F) + labs(title="Ocupancy Status", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
# p_chan <- data %>% dplyr::count(channel,default) %>% ggplot(aes(channel, n/1000)) + geom_col(aes(fill=default), show.legend = F)+ labs(title="Channel", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
# p_prod <- data %>% dplyr::count(prod_type,default) %>% ggplot(aes(prod_type, n/1000)) + geom_col(aes(fill=default), show.legend = F)+ labs(title="Loan Type", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4)) 
# p_prop <- FM_data %>% drop_na() %>% dplyr::count(prop_type,default) %>% ggplot(aes(prop_type, n/1000)) + geom_col(aes(fill=default), show.legend = F)+ labs(title="Property Type", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
# p_purp <- data %>% dplyr::count(loan_purpose,default) %>% ggplot(aes(loan_purpose, n/1000)) + geom_col(aes(fill=default), show.legend = F)+ labs(title="Purpose of Loan", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
# p_term <- data %>% dplyr::count(orig_loan_term,default) %>% filter(n>200) %>% ggplot(aes(as.factor(orig_loan_term), n/1000)) + geom_col(aes(fill=default))+ labs(title="Number of Terms", x = NULL, y = NULL, fill="default") + th+ scale_fill_manual(values = c(basem3, basem4))
# p_borr <- data %>% filter(cnt_borr != 99) %>% dplyr::count(cnt_borr,default) %>% ggplot(aes(cnt_borr, n/1000)) + geom_col(aes(fill=default), show.legend = F) + labs(title="Number of Borrowers", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))


# then the numerical variables
p_cred <- data %>% filter(fico != 9999,!is.na(default)) %>% ggplot(aes(fico)) + geom_histogram(bins=100, aes(fill=default)) + labs(fill = "Default", title="Credit Score", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_cltv <- data %>% filter(cltv != 999,!is.na(default)) %>% ggplot(aes(cltv)) + geom_histogram(bins=100, aes(fill=default)) + labs(fill = "Default", title="Complete Loan to Value", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_nltv <- data %>% filter(ltv != 999,!is.na(default)) %>% ggplot(aes(ltv)) + geom_histogram(bins=100, aes(fill=default)) + labs(fill = "Default", title="Loan to Value", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_dtin <- data %>% filter(dti != 999,!is.na(default)) %>% ggplot(aes(dti)) + geom_histogram(bins=60, aes(fill=default)) + labs(fill = "Default", title="Debt to Income", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_upba <- data %>% filter(!is.na(default)) %>% ggplot(aes(orig_upb)) + geom_histogram(bins=100, aes(fill=default)) + labs(fill = "Default", title="Unpaid Balance", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_rate <- data %>% filter(!is.na(default)) %>% ggplot(aes(int_rt)) + geom_histogram(bins=40, aes(fill=default)) + labs(fill = "Default", title="Interest rate", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))

(p_cred | p_cltv|p_nltv) / (p_dtin | p_upba | p_rate)

p_dq30 <- data %>% filter(`#30_dl` < 100) %>% ggplot(aes(`#30_dl`)) + geom_histogram(bins = 20, aes(fill = default), stat = 'density', position = "identity") + labs(fill = "Default", title="Times of delinquency", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_dq60 <- data %>% filter(`#60_dl` < 100) %>% ggplot(aes(`#60_dl`)) + geom_histogram(bins = 20, aes(fill = default), stat = 'density', position = "identity") + labs(fill = "Default", title="Times of delinquency", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))


geom_bar(width = 1, alpha = 0.7,position = "identity", stat = 'density', na.rm = F) 

p_cred <- data %>% filter(fico != 9999,!is.na(default)) %>% ggplot(aes(fico)) + geom_histogram(bins=100, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Credit Score", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_cltv <- data %>% filter(cltv != 999,!is.na(default)) %>% ggplot(aes(cltv)) + geom_histogram(bins=100, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Complete Loan to Value", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_nltv <- data %>% filter(ltv != 999,!is.na(default)) %>% ggplot(aes(ltv)) + geom_histogram(bins=100, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Loan to Value", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_dtin <- data %>% filter(dti != 999,!is.na(default)) %>% ggplot(aes(dti)) + geom_histogram(bins=60, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Debt to Income", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_upba <- data %>% filter(!is.na(default)) %>% ggplot(aes(orig_upb)) + geom_histogram(bins=100, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Unpaid Balance", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_rate <- data %>% filter(!is.na(default)) %>% ggplot(aes(int_rt)) + geom_histogram(bins=40, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Interest rate", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))

(p_cred | p_cltv|p_nltv) / (p_dtin | p_upba | p_rate) +
  plot_layout(guides="collect") + plot_annotation(theme = theme(legend.position = 'bottom'))


# top 10 servicer and seller
topsell <- data %>% dplyr::count(seller_name) %>% top_n(20)
topserv <- data %>% dplyr::count(servicer_name) %>% top_n(20)

p_sell <- data %>% filter(!is.na(default))%>% dplyr::count(seller_name, default) %>%mutate(n_pct = n / sum(n) * 100) %>% filter(seller_name %in% topsell$seller_name) %>% ggplot(aes(reorder(seller_name,n_pct), n_pct)) + 
  geom_col(aes(fill=default)) + coord_flip() + labs(title = "Seller", x = NULL, y = '%',fill = "Default") + th + theme(legend.position = 'none')+ scale_fill_manual(values = c(basem3, basem4))

p_serv <- data %>% filter(!is.na(default)) %>% dplyr::count(servicer_name, default) %>%mutate(n_pct = n / sum(n) * 100) %>% filter(servicer_name %in% topserv$servicer_name) %>% ggplot(aes(reorder(servicer_name,n_pct), n_pct)) + 
  geom_col(aes(fill=default)) + coord_flip() + labs(title = "Servicer", x = NULL, y = '%',fill = "Default") + th + scale_fill_manual(values = c(basem3, basem4))

p_sell | p_serv  +
  plot_layout(guides="collect") +
  plot_annotation(#title ='Credit Behavior',
    #tag_levels = "I",
    theme = theme(plot.margin = unit(c(.5, 0.1, 0.1, 0.1), "cm"),
                  legend.position = 'top'),
    caption = 'Data: Freddie Mac Single Family Loan-Level 2019.') 







#### 
# loan <- performance %>% filter(id_loan == 'F107Q2229204')
# #a <- loan %>% mutate('default' = if_else(delq_sts >= 3 & dplyr::last(delq_sts) != , TRUE, FALSE))
# 
# loan$temp   = if_else(loan$delq_sts >= 3, loan$svcg_cycle, as.Date(NA))
# loan$temp_2 = if_else(loan$delq_sts == 1, loan$svcg_cycle, as.Date(NA))
# loan$temp_3 = if_else(loan$delq_sts == 1, loan$loan_age, as.numeric(NA))
# loan$temp_4 = if_else(loan$delq_sts >= 3, loan$loan_age, as.numeric(NA))
# loan$temp_5 = if_else(loan$delq_sts == 1, loan$mths_remng, as.numeric(NA))
# loan$temp_6 = if_else(loan$delq_sts >= 3, loan$mths_remng, as.numeric(NA))
# 
# 
# aa <- loan %>%
#   dplyr::group_by(id_loan) %>%
#   dplyr::mutate(prev = lag(delq_sts, order_by=id_loan),
#                 survived = ifelse(prev != 0 & delq_sts == 0, 1, 0)) %>%
#   dplyr::mutate('#30_dl'     = sum(delq_sts == 1, na.rm = TRUE),
#                 '#60_dl'        = sum(delq_sts == 2, na.rm = TRUE),
#                 'first_dt_delq'       = dplyr::first(na.omit(if_else(loan$delq_sts == 1, loan$svcg_cycle, as.Date(NA)))),
#                 'first_delq_age'      = dplyr::first(na.omit(if_else(loan$delq_sts == 1, loan$loan_age, as.numeric(NA)))),
#                 'first_delq_mths_remng' = dplyr::first(na.omit(if_else(loan$delq_sts == 1, loan$mths_remng, as.numeric(NA)))),
#                 'max_eltv'      = max(eltv, na.rm = T),
#                 'min_eltv'      = min(eltv, na.rm = T),
#                 'last_eltv'     = if_else(is.na(dplyr::last(eltv)), 0, dplyr::last(eltv)),
#                 'surv_binary'   = mean(survived, na.rm=T) > 0,
#                 '#surv'         = sum(survived, na.rm=T),
#                 'recovered'     = last(delq_sts) == 0,
#                 'rt_change'     = max(current_int_rt) - min(current_int_rt),
#                 'default'       = if_else(last(delq_sts) != 0 & sum(delq_sts >= 3, na.rm = TRUE) & survived == FALSE , TRUE, FALSE),
#                 'dt_default'    = dplyr::first(na.omit(if_else(if_else(last(delq_sts) != 0 & sum(delq_sts >= 3, na.rm = TRUE) & survived == FALSE & non_int_brng_upb > 0 , TRUE, FALSE), loan$svcg_cycle, as.Date(NA))))) %>% 
#   filter(dplyr::row_number() >= (n() - 11)) %>%  
#   dplyr::mutate('#current_l12' = sum(delq_sts == 0, na.rm = TRUE),
#                 '#30_dl_l12' = sum(delq_sts == 1, na.rm = TRUE),
#                 '#60_dl_l12' = sum(delq_sts == 2, na.rm = TRUE)) %>%
#   filter(row_number() >= (n())) 

