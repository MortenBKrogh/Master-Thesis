# FM_data is loaded in the MAIN.R file.

# Load 'raw' data
# dput(var_names)
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
data <- ldply(FM_data_files, data.table::fread, sep = "|", header = FALSE, col.names = var_names, stringsAsFactors = FALSE)
sum(is.na(data$default_v1))

# first take a look at how many defaults and how many does not default 
table(data$default_v0)
table(data$default_v1)

# choose default definition 1) or 2) 
data$'default'    <- data$default_v1
data$'dt_default' <- data$dt_default_v1


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
             panel.border      = element_blank(), #eelement_rect(fill  = NA,        color = "black", size = 0.3),
             panel.grid.major  = element_line(color ="#f3f3f3", size = 0.3), 
             panel.grid.minor  = element_line(color ="#f3f3f3", size = 0.3), 
             title             = element_text(color = "black"),
             plot.subtitle     = element_text(color = "grey40", size = 8),
             plot.caption      = element_text(color = "grey70"),
             strip.text        = element_text(face  = "bold"),
             axis.text         = element_text(color = "black"),
             axis.ticks        = element_line(color = "black"))

th <- thd + theme(panel.grid.major  = element_blank(), 
                  panel.grid.minor  = element_blank())


# patchwork figure 1)
# plot 1 default rate and active loans per year
# calculate the yearly default rate
# data <- FM_data# %>% sample_n(2000)

dat <- tibble()
first_year = 1999
last_year = 2018
for (i in first_year:last_year) {
  dat[(i-(first_year-1)),1] <- i
  dat[(i-(first_year-1)),2:3] <- data %>% select(dt_first_pi, dt_zero_bal, default) %>% 
    drop_na(dt_zero_bal) %>%  # slå til hvis du kun vil lave grafen på lån der er afsluttet..
    #mutate(dt_zero_bal = if_else(is.na(dt_zero_bal), 201809, as.integer(dt_zero_bal)))
    mutate(dt_first_pi = format(as.Date(dt_first_pi, format = '%Y-%m-%d'), '%Y'),
           dt_zero_bal = format(as.Date(paste0(as.character(dt_zero_bal),"01"), format = "%Y%m%d"), '%Y')) %>%
    mutate(i = if_else(data.table::between(x = paste0(i), lower = dt_first_pi, upper = dt_zero_bal), TRUE, FALSE)) %>%
    dplyr::count(i, default) %>% 
    filter(i == TRUE) %>% 
    mutate(total = sum(n)) %>% 
    transmute(default_pct = n / total *100,
              loan_year   = total) %>%
    filter(row_number() >= n()) 
}

# plot 1)
p1 <- ggplot(data = dat, aes(y = default_pct, x = 1999:2018, group = 1 )) + geom_line(color=basem3) + geom_point(color=basem4, size=2) +
  thd +
  theme(#axis.text.x = element_text(angle = 45),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),) +
  labs(title = 'Default rate',
       subtitle = 'By active loans pr. year.',
       #caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = '',
       y = '%') + scale_fill_manual(values = c(basem3, basem4)) 

# plot 2)
p2 <- ggplot(data = dat, aes(y = loan_year/1000000, x = 1999:2018, group = 1 )) + geom_line(color=basem3) + 
  geom_point(color=basem4, size=2) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  thd +
  theme(axis.text.x = element_text(angle = 45),
        axis.text.y = element_text(angle = 45)) +
  labs(title = 'Active loans pr. year',
       #subtitle = 'By active loans pr. year.',
       caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = '',
       y = 'Millions') + scale_fill_manual(values = c(basem3, basem4))  

# load patchwork package
library(patchwork)
pw1 <- (p1) / (p2) + plot_annotation(tag_levels = "I")
pw1
# save to disk
ggsave(pw1, filename = "Figures/pw_1.pdf", width=8, height=6, dpi=600)


# patchwork figure 2)
# plot 3) fico vs. default rate 
dat <- data %>% select(fico, default) %>% drop_na(fico)  %>%
  mutate(fico = round_any(fico, 25, f = ceiling)) %>%
  dplyr::count(fico, default) %>%  
  group_by(fico) %>% 
  dplyr::mutate(total = sum(n, na.rm = T)) %>%
  dplyr::mutate(pct   = n / total * 100) %>%
  filter(default == T, fico >= 550) 

p3 <- ggplot(data = dat, aes(y = pct, x = fico, group = 1 )) + 
  geom_line(color=basem3) + geom_point(color = basem4) +
  thd +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(#title = 'Default rate',
    #subtitle = 'By Credit Score',
    #caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
    x = 'Credit Score',
    y = 'Default rate %') + scale_fill_manual(values = c(basem3, basem4)) 
#p3

# plot 4) ltv vs. default rate

dat <- data %>% select(ltv, default) %>% #  drop_na(fico)  %>%
  mutate(ltv = round_any(ltv, 5, f = ceiling)) %>%
  dplyr::count(ltv, default) %>%  
  group_by(ltv) %>% 
  dplyr::mutate(total = sum(n, na.rm = T)) %>%
  dplyr::mutate(pct   = n / total * 100) %>%
  filter(default == T, ltv > 10, ltv < 100) 


p4 <- ggplot(data = dat, aes(y = pct, x = ltv, group = 1 )) + 
  geom_line(color=basem3) + geom_point(color = basem4) +
  thd +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(#title = 'Default rate',
    #subtitle = 'By Loan to Value',
    caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
    x = 'Loan to Value',
    y = 'Default rate %') + scale_fill_manual(values = c(basem3, basem4)) 
#p4
pw2 <- (p3) / (p4) + plot_annotation(tag_levels = "I")
pw2

ggsave(pw2, filename = "Figures/pw_2.pdf", width=8, height=6, dpi=600)

# patchwork figure 3)
p_st <- data %>%  dplyr::count(st,default) %>% mutate(n_pct = n / sum(n) * 100)  %>% drop_na(default) %>%  
  mutate(n_pct_order = if_else(default == TRUE, n_pct,  0)) %>%
  ggplot(aes(x = reorder(st, -n_pct_order), y= n_pct)) + 
  geom_col(aes(fill=default), show.legend = F) + 
  labs(#title="Default rate by state", 
       x = NULL, y = '%',
       subtitle = '1999 - 2018'
       #caption = 'Data: Freddie Mac Single Home Loan Level Dataset'
       )+ 
  thd + th +
  scale_fill_manual(values = c(basem3, basem4)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #geom_text(aes(label=round(n_pct, digits = 2)),angle = 90, vjust=0.5, hjust=-0.4, color="darkorange", size=2.5)

# top five states on default
top_state <- data %>% dplyr::count(st) %>% top_n(5)

dat <- tibble()
first_year = 1999
last_year = 2018
for (j in 1:5) {
  for (i in first_year:last_year) {
    dat[(i-(first_year-1)),1] <- i
    dat[(i-(first_year-1)),1+j] <- data %>% select(st, dt_first_pi, dt_zero_bal, default) %>% filter(st == top_state[[1]][j]) %>% 
      drop_na(dt_zero_bal) %>%  # slå til hvis du kun vil lave grafen på lån der er afsluttet..
      #mutate(dt_zero_bal = if_else(is.na(dt_zero_bal), 201809, as.integer(dt_zero_bal)))
      mutate(dt_first_pi = format(as.Date(dt_first_pi, format = '%Y-%m-%d'), '%Y'),
             dt_zero_bal = format(as.Date(paste0(as.character(dt_zero_bal),"01"), format = "%Y%m%d"), '%Y')) %>%
      mutate(i = if_else(data.table::between(x = paste0(i), lower = dt_first_pi, upper = dt_zero_bal), TRUE, FALSE)) %>%
      dplyr::count(i, default) %>% 
      filter(i == TRUE) %>% 
      mutate(total = sum(n)) %>% 
      transmute(default_pct = n / total *100) %>%
      filter(row_number() >= n()) 
  }
}
colnames(dat) <- c('year', top_state[[1]][1], top_state[[1]][2], top_state[[1]][3], top_state[[1]][4], top_state[[1]][5])
dat_long = reshape2::melt(data = dat, id=c("year"))

p_st_2 <- dat_long %>% ggplot(aes(x = year, y= value, color = variable)) + geom_line()  + geom_point(aes(shape = variable), color = basem4)+ labs(title="", x = NULL, y = '%') + thd  + th + scale_color_manual(values = c(basem3,basem3,basem3,basem3,basem3)) + theme(legend.title = element_blank(), legend.key  = element_blank()) + labs(caption = 'Data: Freddie Mac Single Home Loan Level Dataset') 


pw3 = p_st / p_st_2 + plot_annotation(#title ='Credit Behavior',
  tag_levels = "I",
  theme = theme(plot.margin     = unit(c(.5, 0.1, 0.1, 0.1), "cm")))

# save to disk
ggsave(pw3, filename = "Figures/pw_3.pdf", width=8, height=6, dpi=600)


# patchwork figure 4)
# First the categorical variables
p_fthb <- data %>% filter(flag_fthb != 9) %>% drop_na(default,flag_fthb) %>% dplyr::count(flag_fthb,default) %>% mutate(n_pct = n / sum(n) * 100) # %>% 
p_fthb <- p_fthb %>% ggplot(aes(flag_fthb, n_pct)) + geom_col(aes(fill=default), show.legend = F) + labs(title="New Homeowner", x = NULL, y = '%') + th + scale_fill_manual(values = c(basem3, basem4))


p_unit <- data %>% filter(cnt_units != 99) %>% dplyr::count(cnt_units,default) %>% mutate(n_pct = n / sum(n) * 100) 
p_unit <- p_unit %>% ggplot(aes(cnt_units, n_pct)) + geom_col(aes(fill=default), show.legend = F) + labs(title="Number of Units", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))

p_occ  <- data %>% dplyr::count(occpy_sts,default) %>%mutate(n_pct = n / sum(n) * 100) 
p_occ  <- p_occ %>% ggplot(aes(occpy_sts, n_pct)) + geom_col(aes(fill=default), show.legend = F) + labs(title="Ocupancy Status", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))

p_chan <- data %>% dplyr::count(channel,default) %>% mutate(n_pct = n / sum(n) * 100)  
p_chan <-p_chan %>% ggplot(aes(channel, n_pct)) + geom_col(aes(fill=default), show.legend = F)+ labs(title="Channel", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
  
p_prod <- data %>% dplyr::count(prod_type,default) %>% mutate(n_pct = n / sum(n) * 100) 
p_prod <- p_prod %>% ggplot(aes(prod_type, n_pct)) + geom_col(aes(fill=default), show.legend = F)+ labs(title="Loan Type", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4)) 

p_prop <- data %>%  dplyr::count(prop_type,default) %>% mutate(n_pct = n / sum(n) * 100) %>% drop_na() 
p_prop <- p_prop %>% ggplot(aes(prop_type, n_pct)) + geom_col(aes(fill=default), show.legend = F)+ labs(title="Property Type", x = NULL, y = '%') + th+ scale_fill_manual(values = c(basem3, basem4))

p_purp <- data %>% dplyr::count(loan_purpose,default) %>% mutate(n_pct = n / sum(n) * 100) 
p_purp <- p_purp %>% ggplot(aes(loan_purpose, n_pct)) + geom_col(aes(fill=default), show.legend = F)+ labs(title="Purpose of Loan", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))

p_term <- data %>% select(orig_loan_term, default) %>% mutate(orig_loan_term = round_any(orig_loan_term, 20, f = ceiling)) %>% dplyr::count(orig_loan_term,default) %>% mutate(n_pct = n / sum(n) * 100)  %>% filter(orig_loan_term >= 120, orig_loan_term <= 360) %>% drop_na()
p_term <- p_term %>% ggplot(aes(as.factor(orig_loan_term), n_pct)) + geom_col(aes(fill=default))+ labs(title="Number of Terms", x = NULL, y = NULL, fill="default") + th + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(values = c(basem3, basem4))

p_borr <- data %>% select(cnt_borr, default) %>% filter(cnt_borr != 99) %>% drop_na(cnt_borr, default) %>% dplyr::count(cnt_borr,default) %>% mutate(n_pct = n / sum(n) * 100) 
p_borr <- p_borr %>% ggplot(aes(cnt_borr, n_pct)) + geom_col(aes(fill=default), show.legend = F) + labs(title="Number of Borrowers", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))

library(patchwork)
pw4 <- (p_fthb | p_unit | p_occ | p_chan | p_purp | p_prod) / (p_prop | p_term | p_borr) + 
  plot_annotation(#title ='Credit Behavior',
    caption = 'Data: Freddie Mac Single Family Loan-Level 2019.',
    #labs(y = '%'),
    tag_levels = "I"
  )

# save to disk
ggsave(pw4, filename = "Figures/pw_4.pdf", width=8, height=6, dpi=600)


# patchwork figure 5) needs 180 GB of memmory to plot this...
p_cred <- data %>% filter(fico != 9999,!is.na(default)) %>% ggplot(aes(fico)) + geom_histogram(bins=100, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Credit Score", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_cltv <- data %>% filter(cltv != 999,!is.na(default)) %>% ggplot(aes(cltv)) + geom_histogram(bins=100, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Complete Loan to Value", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_nltv <- data %>% filter(ltv != 999,!is.na(default)) %>% ggplot(aes(ltv)) + geom_histogram(bins=100, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Loan to Value", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_dtin <- data %>% filter(dti != 999,!is.na(default)) %>% ggplot(aes(dti)) + geom_histogram(bins=60, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Debt to Income", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))
p_upba <- data %>% filter(!is.na(default), orig_upb < 500000) %>% ggplot(aes(orig_upb)) + geom_histogram(bins=100, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Unpaid Balance", x = NULL, y = NULL) + th + theme(axis.text.x = element_text(angle = 45))+ scale_fill_manual(values = c(basem3, basem4)) + scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) + scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))
p_rate <- data %>% filter(!is.na(default)) %>% ggplot(aes(int_rt)) + geom_histogram(bins=40, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Interest rate", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))

pw5 <- (p_cred | p_cltv |p_nltv) / (p_dtin | p_upba | p_rate) +
  plot_layout(guides="collect") + plot_annotation(theme = theme(legend.position = 'bottom'),
                                                  caption = 'Data: Freddie Mac Single Family Loan-Level 2019.',
                                                  tag_levels = "I")

# save to disk
ggsave(pw5, filename = "Figures/pw_5.pdf", width=8, height=6, dpi=600)



# patchwork figure 6)
# top 20 servicer and seller
topsell <- data %>% dplyr::count(seller_name) %>% top_n(10)
topserv <- data %>% dplyr::count(servicer_name) %>% top_n(10)

p_sell <- data %>% filter(!is.na(default))%>% dplyr::count(seller_name, default) %>% mutate(n_pct = n / sum(n) * 100) %>% filter(seller_name %in% topsell$seller_name) %>%  mutate(n_pct_order = if_else(default == TRUE, n_pct,  0)) %>% ggplot(aes(reorder(seller_name,n_pct_order), n_pct)) + 
  geom_col(aes(fill=default)) + coord_flip() + labs(title = "Seller", x = NULL, y = '%',fill = "Default") + th + theme(legend.position = 'none')+ scale_fill_manual(values = c(basem3, basem4))

p_serv <- data %>% filter(!is.na(default)) %>% dplyr::count(servicer_name, default) %>%mutate(n_pct = n / sum(n) * 100) %>% filter(servicer_name %in% topserv$servicer_name) %>% mutate(n_pct_order = if_else(default == TRUE, n_pct,  0)) %>% ggplot(aes(reorder(servicer_name,n_pct_order), n_pct)) + 
  geom_col(aes(fill=default)) + coord_flip() + labs(title = "Servicer", x = NULL, y = '%',fill = "Default") + th + scale_fill_manual(values = c(basem3, basem4))

pw6 <- (p_sell | p_serv)  +
  plot_annotation(#title ='Credit Behavior',
    #tag_levels = "I",
    theme = theme(plot.margin = unit(c(.5, 0.1, 0.1, 0.1), "cm"),
                  legend.position = 'bottom'),
    caption = 'Data: Freddie Mac Single Family Loan-Level 2019.') +
  plot_layout(guides="collect") 


# save to disk
ggsave(pw6, filename = "Figures/pw_6.pdf", width=8, height=6, dpi=600)


# patchwork figure 7)
p_rt_change <- data %>% filter(rt_change != 0, rt_change > -2,!is.na(default)) %>% ggplot(aes(rt_change)) + geom_histogram(alpha = 0.9, bins=100, aes(fill=default), stat = 'density', position = "identity") + labs(fill = "Default", title="Interest rate change", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem3, basem4))

pw7 <- p_rt_change  +
  plot_annotation(#title ='Credit Behavior',
    #tag_levels = "I",
    theme = theme(plot.margin = unit(c(.5, 0.1, 0.1, 0.1), "cm"),
                  legend.position = 'bottom'),
    caption = 'Data: Freddie Mac Single Family Loan-Level 2019.') +
  plot_layout(guides="collect") 
# save to disk
ggsave(pw7, filename = "Figures/pw_7.pdf", width=8, height=6, dpi=600)


