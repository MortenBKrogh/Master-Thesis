# FM_data is loaded in the MAIN.R file.

# Load 'raw' data
var_names <- c("id_loan","dt_first_pi","fico","flag_fthb","dt_matr","cd_msa","mi_pct","cnt_units",
               "occpy_sts","cltv","dti","orig_upb","ltv","int_rt","channel","ppmt_pnlty",
               "prod_type","st","prop_type","zipcode","loan_purpose","orig_loan_term","cnt_borr","seller_name",
               "servicer_name","flag_sc","default","current","delq_sts","current_upb","current_int_rt","loan_age",
               "mths_remng","cd_zero_bal","#current","#30_dl","#60_dl","#90+_dl","#current_l12","#30_dl_l12",
               "#60_dl_l12","#90+_dl_l12")

FM_data_files <- list.files(path=paste0(getwd(), "/data"), pattern="^Final_.*\\.txt", full.names=TRUE)
FM_data <- ldply(FM_data_files, data.table::fread, sep = "|", header = FALSE, col.names = var_names, stringsAsFactors = FALSE)


# define theme 
base    <- "#08306b"
basem1  <- "#4f6980"
basem2  <- "#849db1"
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

# th <- thd + theme(panel.grid.major  = element_blank(), 
#                   panel.grid.minor  = element_blank())
# 
 

# plot skewness of data
###
p_fico_density <- ggplot(data = FM_data, aes(x = fico, fill = default)) +
  geom_bar(width = 1, alpha = 0.7,position = "identity", stat = 'density', na.rm = F) +
  scale_color_manual(values=c("#003d85", "#ff9c0f")) + scale_fill_manual(values=c("#003d85", "#ff9c0f")) +
  labs(title = 'Credit Score',
       subtitle = 'Density of credit score of the groups default and non-default.',
       caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = 'Credit Score',
       y = 'Frequency') + thd

p_ltv_density <- ggplot(data = FM_data, aes(x = ltv, fill = default)) +
  geom_bar(width = 1, alpha = 0.7,position = "identity", stat = 'density', na.rm = F) +
  scale_color_manual(values=c("#003d85", "#ff9c0f")) + scale_fill_manual(values=c("#003d85", "#ff9c0f")) +
  labs(title = 'Loan-To-Value',
       subtitle = 'Density of loan-to-value of the groups default and non-default.',
       caption = 'Data: Freddie Mac Single Home Loan Level Dataset',
       x = 'Loan-To-Value',
       y = 'Frequency') + thd


p_dti_density <- ggplot(data = FM_data, aes(x = dti, fill = default)) +
  geom_bar(width = 1, alpha = 0.7,position = "identity", stat = 'density', na.rm = F) +
  scale_color_manual(values=c("#003d85", "#ff9c0f")) + scale_fill_manual(values=c("#003d85", "#ff9c0f")) +
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

ggsave(pw_2, filename = "figures/pw_2.pdf", width=8, height=6, dpi=600)




