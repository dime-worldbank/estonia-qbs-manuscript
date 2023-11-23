

#
# SET-UP-------------------------------------------------------------------
# 

### Clean the environment
# rm(list=ls())
# gc()
start1 = Sys.time()

### Source the '00_global.R' script with required packages and functions

if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}

### Make copy of the file -------------------------------------------------------------------
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)




#
# READ DATA -------------------------------------------------------------------
#

### Treatment groups  
patient_ecm_eligible_demo = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible_demo.csv')) %>% mutate(id = as.character(id))



### Death dates
dta_deaths = read_parquet(file.path(project_path, 'Data', 'Clean', 'Deaths_all.parquet'))

### Combine with ECM acceptance 
### NOTE: MOVE TO EARLIER SCRIPT (02_bills_clean.R)---------------------------------------------------------------------

patient_ecm_accept = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_accept.csv')) %>% mutate(id = as.character(id))

dta_deaths = left_join(dta_deaths, patient_ecm_accept) %>% 
  mutate(ecm_status_patient = ifelse(is.na(ecm_status_patient), 'Not enroll', 'Enrolled'))

table(dta_deaths$ecm_include_patient[!is.na(dta_deaths$dateofdeath)],
      dta_deaths$ecm_status_patient[!is.na(dta_deaths$dateofdeath)])


tapply(dta_deaths$ecm_status_patient, dta_deaths$ecm_include_patient, sf)


0/3162 # 0% among always-takers
111/3162 # 3.51% among control compliers
22/1518 # 1.45% among treatment compliers
15/203 # 7.4% among never-takers
37/1725 # 2.14% overall among ECM treatment assigned



### Last treatment date 
### NOTE: MOVE TO EARLIER SCRIPT (02_bills_clean.R)---------------------------------------------------------------------
dta_diagnosis = read_parquet(file.path(project_path, 'Data/Clean', 'dta_diagnosis.parquet'))

temp = dta_diagnosis %>% filter(n_all) %>% dplyr::select(c(id, year_month_day)) %>% 
  group_by(id) %>% summarise_all(., max_miss) %>% rename('startoftreatment_max' = 'year_month_day')


dta_deaths = left_join(dta_deaths, temp)




### Nov-22 MISP - for labelling of patients we know post-hoc were excluded
### NOTE: This is done to check the assumption that the very high mortality rate in 'pure control'
### group is caused by not accounting for those patients who should be excluded (e.g. due to overlly high
### number of commorbidities, terminal condition of a disease etc.)
# patient_eligible_nov22  = clean_names(fread(file.path(project_path, 'Data','Raw',  'ECM Inclusion', "ECM_eligible_nov22.csv"), encoding = 'UTF-8'))
# 
# patient_eligible_nov22 = patient_eligible_nov22 %>%  
#   rename('list_id_nov22' = 'nimistu', # ... rename columns
#          'list_id_new_nov22' = 'uus_nimistu',
#          'id' = 'patsient',
#          'eligible_code_nov22' = 'valjaarv_p_kood',
#          'inclusion_date_nov22' = 'lisamise_kp',
#          'exclusion_date_nov22' = 'valjaarv_kp',
#          'comorbidities' = 'kaasuvaid_haiguseid'
#   ) %>% 
#   dplyr::select(c(id, list_id_nov22, list_id_new_nov22, eligible_code_nov22,  # ...drop and re-order columns
#                   inclusion_date_nov22, exclusion_date_nov22, comorbidities)) %>%
#   mutate(eligible_patient = 'Eligible', #... create new 'eligible_patient' column
#          inclusion_date_nov22 = dmy(inclusion_date_nov22),
#          exclusion_date_nov22 = dmy(exclusion_date_nov22),
#          id = as.character(id)) %>%  #... treat as character
#   filter(eligible_code_nov22 %in% c('', 'JVP92', 'JVP99'))  # ... filter away those with exclusion codes other than 'JVP22' (ECM control?) and 'JVP99' (deceased)
# 
# sf(dta_deaths$ecm_include_patient)
# 
# ### Exclude those we know should be excluded as of Nov-22
# dta_deaths = dta_deaths %>% filter((id %in% patient_eligible_nov22$id))


### Remove all other groups
dta_deaths = dta_deaths %>% filter(!is.na(ecm_include_patient))


### Remove deaths pre-treatment
dta_deaths = dta_deaths %>% filter(death_treat_period == 1 | is.na(death_treat_period)) 

dta_deaths_save = dta_deaths



### Checks
# sf(dta_deaths$ecm_include_patient)
# summary(dta_deaths$dateofdeath)
# tapply(dta_deaths$death_year_rel, dta_deaths$ecm_include_patient, pr_na)
# 
# tapply(dta_deaths$dateofdeath %>% is.na(), dta_deaths$ecm_include_patient, pr)
# tapply(dta_deaths$dateofdeath %>% is.na(), dta_deaths$ecm_include_patient, sf)
# tapply(dta_deaths$dateofdeath, dta_deaths$ecm_include_patient, summary)



### Covid-19 excess mortality NUTS-3 regions ----

### NOTE: That data is from another project, it measures mortality at NUTS-3 region level across Europe
### I thought it might be interesting to see what level of excess mortality due to Covid-19 we might 
### expect in our data

# mortality = fread( file.path('~/Moje/Maps/Europe mortality (weekly, NUTS3)', 'Data', 'mortality_europe_19_23.csv')) %>%
#   filter(cntr_code == 'EE') %>% 
#   mutate(order = 1:nrow(.))
# 
# 
# g1=ggplot(mortality %>% filter(year >= 2018),
#        aes(x = reorder(time_period, order), y = deaths_rel)) +
#   geom_boxplot() +
#   stat_summary(fun=mean, geom="line", group=1,
#                color="red", linewidth = .6) +
#   stat_summary(fun=mean, geom="point",
#                shape=22, color="black", fill="red", size = 5) +
#   geom_vline(aes(xintercept = '2020_10'), linewidth = 1.6, linetype= 'dashed', color = 'red')+
#   theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 1))
# 
# ggsave(file.path(project_path, 'Figures', 'Deaths', paste0('Death dates - Eurostat NUTS-3', ".png")),
#        plot=g1, width = 60, height = 47, units = 'cm')





### Prepare data -------------------------------------------------------------------

dta_deaths = dta_deaths_save %>% 
  mutate(status = ifelse(is.na(dateofdeath), 0, 1),
         time = ifelse(test = is.na(dateofdeath), 
                       yes = as.numeric(ymd(20230331) - ymd(20210601)),
                       no  = as.numeric(dateofdeath - ymd(20210601))),
         time2 = ifelse(test = is.na(dateofdeath), 
                       yes = as.numeric(ymd(startoftreatment_max) - ymd(20210601)),
                       no  = as.numeric(dateofdeath - ymd(20210601))),
         age = ifelse(test = is.na(dateofdeath), 
                      yes = round(as.numeric(ymd(20230331) - ymd(dateofbirth)) /  365.25, 1),
                      no  = round(as.numeric(dateofdeath    - ymd(dateofbirth)) /  365.25, 1)),
         age2 = round(as.numeric(ymd(20210601) - ymd(dateofbirth)) /  365.25, 1)
  )


# cor(dta_deaths$time, dta_deaths$time2, use = 'complete.obs')
# plot(dta_deaths$time, dta_deaths$time2)




### Create strata -------------------------------------------------------------------
dta_deaths = dta_deaths %>% 
  add_column(., .after = 'class_code', strata = paste(dta_deaths$list_id, dta_deaths$class_code))



### Save object  -------------------------------------------------------------------
dta_deaths_save = dta_deaths


#
# PLOT SURVIVAL RATES (PACKAGE) ----
#
### Resources: https://rpkgs.datanovia.com/survminer/

### Define theme  ----------------------------------------------------------------------------------------
t1 = theme_minimal() +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(face = 'plain', color = 'black', size = 34),
    axis.title.y = element_text(face = 'plain', color = 'black', size = 34),
    axis.text.x  = element_text(face = 'plain', color = 'black', size = 27),
    axis.text.y  = element_text(face = 'plain', color = 'black', size = 27),
    legend.text = element_text(face = 'plain', size = 28),
    legend.title = element_blank(),
    legend.position = "top",
    legend.direction  = "horizontal",
    legend.key.height = unit(2.5, "cm"),
    plot.background = element_rect(fill='white', color=NA), #transparent plot bg
    #panel.grid.major.x = element_blank(), #remove major gridlines
    #panel.grid.major.y = element_blank(), #remove major gridlines
    panel.grid.minor.x = element_blank(), #remove minor gridlines
    panel.grid.minor.y = element_blank(), #remove minor gridlines
    strip.text = element_markdown(size = 20, face = 'bold'),
    strip.background = element_rect(fill = 'white', color = NA),
    plot.title = element_markdown(color = 'black', size = 41, hjust = 0.5),
    plot.subtitle = element_markdown(color = 'grey30', size = 35, hjust = 0.5),
    plot.caption= element_textbox_simple(color = 'black', size = 15, hjust = 0),
  )


### LONGRANK TEST 
### https://en.wikipedia.org/wiki/Logrank_test
# -> The logrank test statistic compares estimates of the hazard functions of the two groups at each 
#    observed event time. It is constructed by computing the observed and expected number of events in
#    one of the groups at each observed event time and then adding these to obtain an overall summary 
#     across all-time points where there is an event.
# -> The logrank statistic can be derived as the score test for the Cox proportional hazards
#    model comparing two groups. It is therefore asymptotically equivalent to the likelihood ratio test statistic based from that model.
### https://datatab.net/tutorial/log-rank-test


### Predict() function for Cox models
# https://stat.ethz.ch/R-manual/R-devel/library/survival/html/predict.coxph.html
# ?predict


# m4 <- coxph(Surv(time, status) ~ strata + age + gender + ecm_include_patient, 
#             cluster = list_id,
#             data = temp)
# 
# fit <- survfit(Surv(time2, status) ~ ecm_include_patient, data = temp)




### Leave only ECM control and treatment --------------------------------------------------------------------
temp = dta_deaths %>% filter(!(ecm_include_patient %in% c('Pure control')))

### Rename columns to work easier with survfit function
# temp2 = temp %>% rename('cluster_strata' = 'strata', 'strata' = 'ecm_include_patient')




### Loop over risk groups --------------------------------------------------------------------
panel1 = 'a'

for(panel1 in c('a', 'b', 'c')){
  
  if(panel1 == 'a'){
    risk_group = c('Mild/moderate', 'Severe')
    subtitle1 = 'All-risk patients'
    }
  if(panel1 == 'b'){
    risk_group = c('Mild/moderate')
    subtitle1 = 'Mild-risk patients'
    }
  if(panel1 == 'c'){
    risk_group = c('Severe')
    subtitle1 = 'Severe-risk patients'
    }
  
  # Leave only ECM control and treatment 
  temp2 = temp %>% filter(class_code %in% risk_group)
  
  # Fit the survival curve and model
  fit <- survfit(Surv(time, status) ~ ecm_include_patient,
                 data = temp2)
  
  cox_model <- coxph(Surv(time, status) ~ ecm_include_patient + age + gender, data = temp2)

  
  ###  Plot survival curves ----------------------------------------------------------------------------------------
  
  library(survminer)
  
  g1 = ggsurvplot(
    fit,
    data = temp2,
    size = 1,                 # change line size
    palette = c("grey67", "black"),# custom color palettes
    conf.int = TRUE,          # Add confidence interval
    pval = TRUE,  pval.method	 = T, pval.coord = c(0, 0.97),
    
    risk.table = TRUE,        # Add risk table
    risk.table.col = "strata",# Risk table color by groups
    ylim = c(.95,1),         # pr esent narrower X axis, but not affect
    xlim = c(0,668),
    
    xlab = "\n Days since ECM onset (June 2021)",   # customize X axis label.
    ylab = "\n Survival probability\n",   # customize Y axis label.
    
    break.time.by = 90,     # break X axis in time intervals by 500.
    legend.labs = c("ECM Control", "ECM Treatment"),    # Change legend labels
    risk.table.height = 0.25, # Useful to change when you have multiple groups
    risk.table.y.text.col = T, # colour risk table text annotations.
    risk.table.y.text = FALSE, # show bars instead of names in text annotations
    #ggtheme = theme_light(), # customize plot and risk table with a theme.
    
    title = '<b>Survival probability by ECM groups</b>',
    subtitle = paste0(subtitle1, '<br>'),
    caption = '<br><br><b>Notes:</b> The plot shows survival probability
              based on <b>Cox-Proportional Hazards Model</b>, which 
               measures survival times (in days) from 01/06/2021,  censored at 31/03/2023 for the patients who survived the entire period.
               for ECM control and treatment patients.
              <b>P-value is calculated using log-rank test.</b><br><br>',
    ggtheme = t1
    
  )
  
  png(file.path(project_path, 'Figures', 'Deaths',
                paste0('Survival rates by group (', 
                       subtitle1,
                       ").png")),
      width = 4000, height = 4000, res = 210, pointsize = 20)
  
  print(g1)
  
  
  
  dev.off()


}


  
#
# COX PROP. HAZARDS MODEL --------------------------------------------------------------------
#


### Check if risk groups equally distributed --------------------------------------------------------------

# tapply(patient_ecm_eligible$class_code, patient_ecm_eligible$ecm_include_patient, pr)
# 
# t.test(patient_ecm_eligible$class_code[patient_ecm_eligible$ecm_include_patient %in% c('Control', 'Treatment')] == 'Severe' ~
#          patient_ecm_eligible$ecm_include_patient[patient_ecm_eligible$ecm_include_patient %in% c('Control', 'Treatment')])
# 
# patient_ecm_eligible_demo$age = as.numeric((ymd(20230331) - ymd(patient_ecm_eligible_demo$dateofbirth)))/365.25
# 
# temp = lm(class_code == 'Severe'  ~ list_id + ecm_include_patient,
#           data = patient_ecm_eligible[patient_ecm_eligible$ecm_include_patient %in% c('Control', 'Treatment'),])
# summary(temp)

### R: Roughly - <3 p.p more mild-RISK patient in ECM treatment; difference just statistically significant in t-test (4.7%),
### but completely not significant (96.5%) in a model with provider FE
### R: Similar for GENDER
### R: But marginally significant AGE difference (0.043%)


### Fit the models  -------------------------------------------------------------------

# Re-level treatment variable
dta_deaths$ecm_include_patient = factor(dta_deaths$ecm_include_patient , levels = c('Pure control','Control', 'Treatment'))

# Ensure relevant variables are strings, not numbers
dta_deaths$block_categorical = as.character(dta_deaths$block_categorical)



panel1 = 'c'

for(panel1 in c('a', 'b', 'c')){
  
  print(panel1)
  
  if(panel1 == 'a'){risk_group = c('Mild/moderate', 'Severe')}
  if(panel1 == 'b'){risk_group = c('Mild/moderate')}
  if(panel1 == 'c'){risk_group = c('Severe')}
  

  ##### (1) OLS (FE) -------------------------------------------------------------------
  
  # Prepare the data by...
  temp = dta_deaths %>% filter(!(ecm_include_patient %in% c('Pure control'))) %>% # ... leaving only desired ECM groups
    mutate(ecm_include_patient = droplevels(ecm_include_patient)) %>%  # ... dropping spare levels (code might produce an error otherwise)
    filter(class_code %in% risk_group) # ... leaving only patients of a given risk class
  
  # Fit the model
  m1 <- lm(status  ~ strata + ecm_include_patient,
           data = temp)
  
  # Extract coeffiicents
  m1 = coeftest(m1, cluster.vcov(m1, temp$list_id, df_correction = T))
  
  
  
  #### (2) OLS (FE + age + gender) -------------------------------------------------------------------
  temp = dta_deaths %>% filter(!(ecm_include_patient %in% c('Pure control')))  %>% 
    mutate(ecm_include_patient = droplevels(ecm_include_patient)) %>% 
    filter(class_code %in% risk_group)
  
  m2 <- lm(status  ~ strata + age + gender + ecm_include_patient,
           data = temp)
  
  m2 = coeftest(m2, cluster.vcov(m2, temp$list_id, df_correction = T))
  
  
  nobs(m2)
  

  
  #### (3) OLS (EXPANDED + age + gender) -------------------------------------------------------------------
  
  if(panel1 == 'a'){
    
    temp = dta_deaths %>% filter(!(ecm_include_patient %in% c('Control'))) %>% 
      mutate(ecm_include_patient = droplevels(ecm_include_patient)) 
    
    
    m3 <- lm(status  ~ block_categorical + age + gender + ecm_include_patient,
             data = temp)
    
    m3 = coeftest(m3, cluster.vcov(m3, temp$list_id, df_correction = T))
    
  }
  
  
  
  
  #### (4) COX (FE) -------------------------------------------------------------------
  temp = dta_deaths %>% filter(!(ecm_include_patient %in% c('Pure control')))  %>% 
    mutate(ecm_include_patient = droplevels(ecm_include_patient)) %>% 
    filter(class_code %in% risk_group)
  
  m4 <- coxph(Surv(time, status) ~ strata + ecm_include_patient, 
              cluster = list_id,
              data = temp)
  summary(m4)
  
  
  #### (5) COX (FE + age + gender) -------------------------------------------------------------------
  
  # dta_deaths = dta_deaths %>% group_by(strata) %>% mutate(N = n()) %>% ungroup()
  # View(temp %>% filter(strata %in% c('N0141 Severe')))
  
  temp = dta_deaths %>% filter(!(ecm_include_patient %in% c('Pure control')))  %>% 
    mutate(ecm_include_patient = droplevels(ecm_include_patient)) %>% 
    filter(class_code %in% risk_group)
  
  m5 <- coxph(Surv(time, status) ~ strata + age + gender + ecm_include_patient, 
              cluster = list_id,
              data = temp )

  # Save predictions for the plot
  temp = left_join(temp, basehaz(m5))
  temp$pred = temp$hazard * exp(predict(m5, newdata = temp, type = 'lp'))
  assign(paste0('pred_', panel1), temp %>% mutate('panel' = panel1))
  
  
  
  #### (6) COX (EXPANDED + age + gender) -------------------------------------------------------------------
  if(panel1 == 'a'){
    
    temp = dta_deaths %>% filter(!(ecm_include_patient %in% c('Control')))  %>%
                  mutate(ecm_include_patient = droplevels(ecm_include_patient))

    m6 <- coxph(Surv(time, status) ~  block_categorical + age + gender + ecm_include_patient,
                cluster = list_id,
                data = temp)

    
  }
  
  
  ### Store all model results
  # First create empty data frames if m3 and m6 not run in this iteration
  m3 = if(exists('m3')){extract_coeftest(m3, 2)}else{data.frame(var = 'ecm_include_patientTreatment', beta = '')}
  m6 = if(exists('m6')){extract_coeftest(m6, 2)}else{data.frame(var = 'ecm_include_patientTreatment', beta = '')}
  
  m_list <- list(extract_coeftest(m1, 0), extract_coeftest(m2, 2), m3,
                 extract_coeftest(m4, 0), extract_coeftest(m5, 2), m6)
  m_list <- m_list %>% reduce(full_join, by='var')
  
  assign(paste0('m_panel_', panel1), m_list)
  

  save(m1, m2, m3,
       m4, m5, m6,
       file = file.path(project_path, 'Tables', 'Models (R)', paste0('table_deaths_', panel1, '.RData')))
  
  ### Remove model objects before next iteration
  rm( list = Filter( exists, c('m1','m2', 'm3', 'm4', 'm5', 'm6')) ) 
  
}



m_panel_a
m_panel_b
m_panel_c


pred_a$pred %>% summary
pred_b$pred %>% summary
pred_c$pred %>% summary




### LaTeX -----------------------------------------------------------------

# Combine the results
dta_table = rbindlist(list(m_panel_a, m_panel_b, m_panel_c))


# Substitute all missing for dashes
dta_table[is.na(dta_table)] = '$-$'
dta_table[dta_table == ''] = '$-$'

# Clean column names
dta_table = dta_table %>% mutate(var  = dplyr::recode(var, 
                                               'ecm_include_patientTreatment' = 'ECM Treatment',
                                               'age' = 'Age (years)',
                                               'genderMale' = 'Gender (male)'))

# Put all columns in one dataframe column with LaTeX table separators
dta_table$cell1 = paste0(apply(dta_table, 1, paste, collapse = "&"), '\\')


# Add section headings
dta_table = dta_table %>% add_row(.before = 1) %>% add_row(.before = 5) %>% add_row(.before = 9)

dta_table$cell1[c(1,5,9)] = paste0('\\multicolumn{7}{r{.69\\textwidth}}{\\textbf{',  'Panel A', '}}\\')

### Replace $-$ with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)


### Print to copy to Overleaf  ---------------------------------------------------------------------------------------------------
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}


### Means ------------------------------------------------------------------
tapply(dta_deaths$status, dta_deaths$ecm_include_patient, function(x) pr(x)/100)

tapply(dta_deaths$status[dta_deaths$class_code == 'Mild/moderate'], 
       dta_deaths$ecm_include_patient[dta_deaths$class_code == 'Mild/moderate'], 
       function(x) pr(x)/100)

tapply(dta_deaths$status[dta_deaths$class_code == 'Severe'], 
       dta_deaths$ecm_include_patient[dta_deaths$class_code == 'Severe'], 
       function(x) pr(x)/100)




### Save model results to avoid re-estimation (especially the expanded sample ones)

# load(file.path(project_path, 'Tables', 'Models (R)', paste0('table_deaths', '.RData')))


# fwrite(tidy(m4), file.path(project_path, 'Tables', 'Models (R)', paste0('table_deaths_', 'm6', '.csv')), na = NA, row.names = F)




#
# PREDICTION PLOTS -----------------------------------------------------------------
#

temp = rbindlist(list(pred_a, pred_b, pred_c)) %>% 
          mutate(panel = dplyr::recode(panel,
                                       'a' =  paste0('<b>Panel A: All-risk patients</b> (N=', format(nrow(pred_a), big.mark = ","),')'),
                                       'b' =  paste0('<b>Panel B: Mild-risk patients</b> (N=', format(nrow(pred_b), big.mark = ",") ,')'),
                                       'c' =  paste0('<b>Panel C: Severe-risk patients</b> (N=', format(nrow(pred_c), big.mark = ",") ,')')))


mean_miss((temp$pred[round(temp$age, 0) == 80 & temp$ecm_include_patient == 'Treatment']))
mean_miss((temp$pred[round(temp$age, 0) == 75 & temp$ecm_include_patient == 'Control']))

mean_miss((temp$pred[round(temp$age, 0) == 75 & temp$ecm_include_patient == 'Treatment' & temp$class_code == 'Mild/moderate']))
mean_miss((temp$pred[round(temp$age, 0) == 65 & temp$ecm_include_patient == 'Control' & temp$class_code == 'Mild/moderate']))




g1=ggplot(temp %>% filter(!is.na(gender)), 
          aes(x = age, y = pred,
              col = paste0(ecm_include_patient, ' - ', gender),
              group = paste0(ecm_include_patient, ' - ', gender))) +
  
  geom_hline(aes(yintercept = 0), linewidth = 1.2, color = 'black') +
  # geom_point(aes(fill = as.character(status)), shape = 21,
  #            size = 0,
  #            col = 'black', alpha = .5) +
  geom_smooth(method = 'loess', linewidth = 2.4,
              alpha = .3,
              fullrange = T, span = 0.5) +
  
  scale_color_manual(name = '', values = c( '#F4C978', '#82A1B5', '#EFA313', '#176EA6')) +
  guides(fill = guide_legend(title.position = 'top'))+
  facet_wrap(~panel, scales = 'fixed')+
  
  scale_y_continuous(
                     # limits = c(0, max(temp$pred)),
                     expand = expansion(mult = c(0,.01))) +
  
  scale_x_continuous(
    limits = c(50, 85),
    breaks = seq(50,85,5)
    # limits = c(quantile(dta_deaths$age, probs=.25, na.rm=T)[1], quantile(dta_deaths$age, probs=.75, na.rm=T)[1])
    ) +
  
  labs(title = 'Predictions derived from Cox Proportional-Hazards Model (column 5)<br>',
       x = 'Age', y = 'Expected number of events',
       caption = paste0("<br><br><b>Notes:</b> 
       Plots display predicted risk of death between ECM onset on 01/06/2021 and the end of the observation period
       on 31/03/2023. The predicted values are based on Cox Proportional-Hazards Model as specified in column (5)
       of Table XX. Predictions are made for every individual meeting the criteria specified in the panel heading,
       based on their provider fixed effects, age, gender, and ECM treatment status. Lines of best fit shown 
       with 95% confidence intervals<br><br>" ),
    
       ) +
  
  theme_bw()+ 
    theme(
      plot.title  = element_markdown(size = 35, family = 'Calibiri', face = 'bold', hjust = .5),
      plot.subtitle  = element_markdown(size = 28, family = 'Calibiri'),
      plot.caption  = element_textbox_simple(size = 13, family = 'Calibiri'),
      axis.ticks = element_line(),
      axis.title.x = element_markdown(size = 31, family = 'Calibiri'),
      axis.title.y = element_markdown(size = 31, family = 'Calibiri'),
      axis.text.x = element_text(size = 25, angle = 0, hjust = .5, family = 'Calibiri'),
      axis.text.y = element_text(size = 28, family = 'Calibiri'),
      legend.text = element_text(face = 'plain', size = 23, family = 'Calibiri'),
      legend.title = element_text(face = 'plain', size = 40, family = 'Calibiri'),
      legend.key.height = unit(1, "cm"),
      legend.key = element_rect(fill = NA, color = NA),
      legend.position = 'top',
      legend.direction = 'horizontal',
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.background = element_rect(fill = NA, color = NA),
      strip.text = element_markdown(size = 27)
    )
  
ggsave(file.path(project_path, 'Figures', 'Deaths', paste0('Cox Model predictions.png')),
       plot = g1, width = 60, height = 24, unit = 'cm')



#
# LIFE EXPECTANCY  ------------------------------------------
#

dta_deaths = dta_deaths_save

dta_deaths = dta_deaths %>% filter(ecm_include_patient %in% c('Control', 'Treatment'))
table(dta_deaths$status)

tapply(dta_deaths$age, paste0(dta_deaths$status, dta_deaths$ecm_include_patient, dta_deaths$gender), summary)


tapply(dta_deaths$time, paste0(dta_deaths$ecm_include_patient, cut(dta_deaths$age, breaks = c(0, seq(50,100,15)))), summary)

tapply(dta_deaths$time, paste0(dta_deaths$ecm_include_patient, cut(dta_deaths$age2, breaks = c(0, seq(50,100,15)))), summary)


table(dta_deaths$ecm_include_patient)
tapply(dta_deaths$time, paste0(dta_deaths$ecm_include_patient), function(x) sum_miss(x)/365.25)


life_exp  = data.frame(ecm_include_patient = c(rep('Control', 4), rep('Treatment', 4)),
            # gender = c('Female', 'Male'),
            age_start = c(0, 60, 70, 80),
            age_end = c(60, 70, 80, 120))


i = 1
for(i in 1:nrow(life_exp)){
  
  temp = dta_deaths[dta_deaths$ecm_include_patient == life_exp$ecm_include_patient[i] &
                  dta_deaths$age2 > life_exp$age_start[i] &
                  dta_deaths$age2 <= life_exp$age_end[i]]
                
  n = nrow(temp)
  d = sum(temp$status)
  q = 100*d/n
  L = sum(temp$time) / 365.25
  m = 100*d/L
  e = L/n
  
  life_exp$alive_start[i] = n
  life_exp$deaths_n[i] = d
  life_exp$years_lived[i] = L
  life_exp$deaths_prob[i] = q
  life_exp$mortality_rate[i] = m
  life_exp$life_expectancy[i] = e
}

life_exp

# Save the results
fwrite(life_exp,  file.path(project_path, 'Tables', 'Life Expectancy.csv'))



5884.312/3272
3111.050 /1721


643/365.25
655/365.25

7182893/103

668/365.25




### Correlate survival probability with care plan quality ------------------------------------------

# care = fread(file.path(project_path, 'Data', 'Clean', 'Other', 'Care plan evaluations.csv'))
# 
# risk_group = c('Mild/moderate', 'Severe')
# 
# temp = dta_deaths %>% filter(!(ecm_include_patient %in% c('Pure control')))  %>% 
#   mutate(ecm_include_patient = droplevels(ecm_include_patient)) %>% 
#   filter(class_code %in% risk_group)
# 
# n_distinct(temp$list_id)
# 
# temp = left_join(temp, care %>% dplyr::select(c(list_id, care_plan_ev_pc1)))
# 
# m1 = coxph(Surv(time, status) ~  age + gender + ecm_include_patient * care_plan_ev_pc1,
#            data = temp,
#            cluster = list_id)
# 
# summary(m1)

# cor(temp$care_plan_ev_pc1, temp$pred, use = 'complete.obs')
# 
# ggplot(temp, aes(care_plan_ev_pc1, pred)) +
#   geom_point(shape = 21) +
#   geom_smooth(method = 'loess')






# '  --------------------------------------------------------------------
# SCRAPBOOK --------------------------------------------------------------------
#  
  
### Cox - manual predictions ---------------------------------------------------  
  
  temp = dta_deaths %>% filter(!(ecm_include_patient %in% c('Pure control')))  %>% 
    mutate(ecm_include_patient = droplevels(ecm_include_patient)) %>% 
    filter(class_code %in% risk_group)
  
  
  m5 <- coxph(Surv(time, status) ~ list_id + age + gender + ecm_include_patient, 
              cluster = list_id,
              data = temp)
  summary(m5)
  
  extract_coeftest(m5, 3)
  
  
  ### NOTE: predict()
  ### type = 'lp' -> sum of model terms (IMPORTANT: CONTINOUS PREDICTORS HAVE REFERENCE VALUES OF THEIR MEAN NOT OF 0!)
  ### type = 'risk' -> sum of model terms exontiated exp(Xi x  Bi) = predict(type = 'risk')
  ### type = 'expected' -> type = 'risk' * base hazard (from basehaz(model1))
  ### type = 'survival' -> exp(-expected)
  
  temp$pred_lp = predict(m5, newdata = temp, type = 'lp')
  temp$pred_risk = predict(m5, newdata = temp, type = 'risk')
  temp$pred_exp = predict(m5, newdata = temp, type = 'expected')
  temp$pred_sur = predict(m5, newdata = temp, type = 'survival')
  summary(temp$pred_sur)
  
  temp = left_join(temp, basehaz(m5) )
  coef(summary(m5))[,1]
  
  temp$my_lp  = (( (temp$age - mean(temp$age, na.rm=T)) * coef(summary(m5))[1,1] +
                     (temp$gender == 'Male') * coef(summary(m5))[2,1] +
                     (temp$ecm_include_patient == 'Treatment')* coef(summary(m5))[3,1]))
  
  temp$my_risk = exp(temp$my_lp)
  temp$my_exp = temp$hazard * temp$my_risk
  temp$my_sur = exp(-temp$my_exp)
  
  View(temp %>% dplyr::select(time, status, age, gender, ecm_include_patient,
                              hazard,contains('pred_'), contains('my_')))
  

  # temp$pred1 = predict(m5, newdata = temp, type = 'survival')
  # 
  # tapply(temp$pred1, temp$status, summary)
  # 
  # ggplot(temp %>% filter(status == 1),
  #        aes(time, pred1,
  #            group = ecm_include_patient, 
  #            color = ecm_include_patient, 
  #            fill = ecm_include_patient))+
  #   # geom_hline(aes(yintercept = 0), linewidth=1.5)+
  #   geom_smooth()+
  #   geom_point(shape = 21, size = 3, color = 'black')
  
  
  
# Survival time as DV (seems too skewed) -----------------------------------------------------
hist(dta_deaths$time)

m4 <- lm(time ~  strata + ecm_include_patient, 
         data = dta_deaths)

summary(m4)

coeftest(m4, cluster.vcov(m4, dta_deaths$list_id, df_correction = T))






#
# BY YEAR ----
# 

### Read year_rel data
dta_diagnosis_year_rel = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_year_rel.parquet'))


### Leave only relevant groups and columns
dta_diagnosis_year_rel = dta_diagnosis_year_rel %>% 
  ungroup() %>% 
  filter(id %in% dta_deaths$id) %>% 
  dplyr::select(c(id, year_rel))


### Add patient-level controls and treatment
dta_diagnosis_year_rel = left_join(dta_diagnosis_year_rel, patient_ecm_eligible)

### Create strata
dta_diagnosis_year_rel = dta_diagnosis_year_rel %>% 
  add_column(., .after = 'class_code', strata = paste(dta_diagnosis_year_rel$list_id, dta_diagnosis_year_rel$class_code))

### Add death dates
dta_diagnosis_year_rel = left_join(dta_diagnosis_year_rel, 
                 dta_deaths %>% mutate(year_rel = death_year_rel) %>% dplyr::select(c(id, year_rel, dateofdeath, death_year_rel)))

### Create 'deceased' dummy
dta_diagnosis_year_rel$deceased = ifelse(is.na(dta_diagnosis_year_rel$dateofdeath), 0, 1)

### Remove patients once they died
tapply(dta_diagnosis_year_rel$deceased, dta_diagnosis_year_rel$ecm_include_patient, sf)

dta_diagnosis_year_rel = dta_diagnosis_year_rel %>%
                            filter(year_rel > 0) %>%  
                            group_by(id) %>% 
                            fill(death_year_rel) %>% 
                            ungroup() %>% 
                            filter(death_year_rel >= year_rel | is.na(death_year_rel))

tapply(dta_diagnosis_year_rel$deceased, dta_diagnosis_year_rel$ecm_include_patient, sf)


### Fit the model
m1 = glm(deceased ~ strata + as.character(year_rel) + ecm_include_patient, 
        data = dta_diagnosis_year_rel,
        family = binomial(link = 'logit'))

summary(m1)


coeftest(m1, cluster.vcov(m1, dta_diagnosis_year_rel$id, df_correction = T))




#
# IV/TOT ----
#


### ECM acceptance
patient_ecm_accept = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_accept.csv')) %>% mutate(id = as.character(id))

temp = left_join(patient_ecm_accept, patient_ecm_eligible)

sf(temp$ecm_include_patient)


dta_deaths_accept = left_join(dta_deaths, patient_ecm_accept) %>% 
  mutate(ecm_status_patient = ifelse(is.na(ecm_status_patient), 0, 1))

dta_deaths_accept$deceased = ifelse(is.na(dta_deaths_accept$dateofdeath), 0, 1)

### Base
m1 = iv_robust(deceased ~ ecm_status_patient  | ecm_include_patient, 
               cluster = list_id, fixed_effects = ~strata, data = dta_deaths_accept)

summary(m1)

### Interaction with risk classification
m2 = iv_robust(deceased ~ ecm_status_patient * class_code | ecm_include_patient * class_code, 
               cluster = list_id, fixed_effects = ~list_id, data = dta_deaths_accept)

summary(m2)



### Survival by ECM acceptance date ----
temp = dta_deaths_accept %>%
  filter(!is.na(dateofdeath)) %>% 
  mutate(status = ifelse(is.na(dateofdeath), 0, 1),
         time = ifelse(test = is.na(dateofdeath), 
                       yes = as.numeric(ymd(20230331) - ymd(ecm_status_patient_date)),
                       no  = as.numeric(dateofdeath - ymd(ecm_status_patient_date))))


m1 <- coxph(Surv(time, status) ~ strata + ecm_include_patient, 
            cluster = list_id,
            data = dta_deaths_accept)

summary(m1)





#
# EVENT STUDY ----
#

### By year_rel

dta_diagnosis_year_rel = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_year_rel.parquet'))


### By month?
# dta_diagnosis_month = read_parquet(file.path(project_path, 'Data/Clean', 'All_outcomes_month.parquet'))

# dta_diagnosis_month = dta_diagnosis_month %>% 
#   mutate(
#     time_length(difftime(as.Date(paste0(year_month, '-01')), as.Date("2021-06-01")), "months") %>% sign() *
#     time_length(difftime(as.Date(paste0(year_month, '-01')), as.Date("2021-06-01")), "months") %>% ceiling()
#   )


dta_diagnosis_year_rel$treatment_year = 1
dta_diagnosis_year_rel = dta_diagnosis_year_rel %>% as.data.table()
dta_diagnosis_year_rel$year_rel = as.integer(dta_diagnosis_year_rel$year_rel)
dta_diagnosis_year_rel$treatment_year = as.integer(dta_diagnosis_year_rel$treatment_year)

install.packages('digest')
install.packages('githubinstall')
library(githubinstall)
githubinstall('eventstudies',force=TRUE)
library(eventstudies)

es <- eventstudy(firm.returns = n_inpatient,
                event.list = treatment_year,
                event.window = 2,
                type = 'None', #We are using 'None' as the type here.
                to.remap = TRUE,
                remap = 'cumsum',
                inference = TRUE,
                inference.strategy = 'bootstrap')


# results <- ES(long_data = dta_diagnosis_year_rel, 
#               outcomevar = "n_inpatient", 
#               unit_var = "id",
#               cal_time_var = "year_rel", 
#               onset_time_var = "treatment_year",
#               cluster_vars = "id")


#
# RE-RANDOMIZE ----
#

n1 = 100

patient_ecm_eligible_randp = fread(file.path(project_path, 'Data', 'Clean', paste0('randp', n1, '.csv'))) %>% 
                                as.data.frame() %>% 
                                filter(id %in% dta_deaths$id)


treat1 = 'rand21'

count = 0
for(treat1 in c(paste0('rand', 1:n1))){
  
  # Control the loop
  print(treat1)
  count = count + 1
  
  
  # Assign placebo treatment
  dta_deaths$rand = patient_ecm_eligible_randp[, treat1]
  
  # Estimate the model 
  m1 <- coxph(Surv(time, status) ~ strata + rand + ecm_include_patient, 
              cluster = list_id,
              data = dta_deaths)
  
  # summary(m1)
  
  m1 = m1$coefficient[c(len1, len1-1)] %>% t() %>% as.data.frame() %>% 
              add_column(.before = 1, 'treat' = treat1) 
  
  
  # Assign results to a dataframe
  if(count == 1){
    dta = m1
  }else{
    dta = rbind(dta, m1)
  }
  
  print(pr(abs(dta$ecm_include_patientTreatment) < abs(dta$randTreatment)))
}

pr(abs(dta$ecm_include_patientTreatment) < abs(dta$randTreatment))




# Save the results
fwrite(dta,  file.path(project_path, 'Tables', 'Randomized Inference', paste0(var2, '_', n1, '.csv')))


#
# Plot density of beta coefficients ----
# 

### (Re-)read the relevant model results
n1 = 100
var2 = 'deceased'

dta = fread(file.path(project_path, 'Tables', 'Randomized Inference', paste0(var2, '_', n1, '.csv')))

prop1 = pr(abs(dta$randTreatment)  > abs(dta$ecm_include_patientTreatment))
prop1 = round(prop1['TRUE'], 1)
prop1

dta_plot = dta %>% dplyr::select(c(ecm_include_patientTreatment, randTreatment)) %>% gather()

### Plot
g1=ggplot(dta,
          aes(x = randTreatment)) +
  
  geom_rect(
    aes(xmin = -Inf, xmax = mean(dta$ecm_include_patientTreatment),
        ymin = -Inf, ymax = Inf),
    fill = "gray",
    alpha = 0.1
  ) +
  
  geom_vline(aes(xintercept = 0),
             linewidth = .6, color = 'black') +
  
  geom_histogram(fill = 'grey50',
                 # binwidth = .001,
                 color = 'black') + 
  geom_density(linewidth = 2) + 
  
  geom_vline(aes(xintercept = mean(dta$ecm_include_patientTreatment)),
             linewidth = 1.6, color = 'red', linetype = 'dashed') +
  
  geom_vline(aes(xintercept = mean(dta$randTreatment)),
             linewidth = 1.6, color = 'black', linetype = 'dashed') +
  
  geom_text(aes(x = mean(dta$ecm_include_patientTreatment),
                y = 0.162*n1,
                label = paste0('Average β\n(original treatment)')),
            size = 4, family = 'Calibiri', color = 'black',
            angle = 90, vjust = -.5) +
  
  geom_text(aes(x = mean(dta$randTreatment),
                y = 0.162*n1,
                label = paste0('Average β\n(placebo treatment)')),
            size = 4, family = 'Calibiri', color = 'black',
            angle = 90, vjust = -.5) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
  
  labs(
    
    x = '<b>β coefficient</b><br>',
    y = '<b>Count</b><br>',
    
    
    
    title = paste0('<b>', dict_outcomes$name[dict_outcomes$var == var2], '</b><br>'),
    subtitle = paste0('Randomization inference p-value: <b>', prop1, '%</b><br>'),
    
    caption = paste0("<br><b>Notes: </b>", 'Based on <b>', n1, '</b> iterations of re-randomization of treatment assignment. 
                     β coefficient estimated using <b>Cox Proportional-Hazards model</b> with strata fixed effects and clustered by healthcare provider. ',
                     'Outcome variable shown measures ',  dict_outcomes$description[dict_outcomes$var == var2], ".
                   Black line shows density of observations. All observations lying in the grey area
                   of the plot are smaller than the average effect of the original ECM treatment assignment. <br>")
  ) +
  theme(
    plot.title  = element_markdown(size = 31, family = 'Calibiri'),
    plot.subtitle  = element_markdown(size = 28, family = 'Calibiri'),
    plot.caption  = element_textbox_simple(size = 13, family = 'Calibiri'),
    axis.ticks = element_line(),
    axis.title.x = element_markdown(size = 31, family = 'Calibiri'),
    axis.title.y = element_markdown(size = 31, family = 'Calibiri'),
    axis.text.x = element_text(size = 25, angle = 0, hjust = .5, family = 'Calibiri'),
    axis.text.y = element_text(size = 28, family = 'Calibiri'),
    legend.text = element_text(face = 'plain', size = 23, family = 'Calibiri'),
    legend.title = element_text(face = 'plain', size = 40, family = 'Calibiri'),
    legend.key.height = unit(1, "cm"),
    legend.key = element_rect(fill = NA, color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(project_path, 'Figures', 'Randomized Inference', paste0(var2, '_', n1, '.png')),
       plot = g1, width = 40, height = 23, unit = 'cm')








#
# PLOT SURVIVAL RATES (CUSTOM) ----
#


### Survival by day ----
date_sequence <- seq(ymd(20210601), ymd(20230331), by="days")
length(date_sequence)

day = date_sequence[1]

dta_alive <- lapply(date_sequence, function(day) {
  dta_deaths %>% 
    dplyr::select(c(ecm_include_patient, dateofdeath)) %>% 
    group_by(ecm_include_patient) %>%
    mutate(N = n()) %>% ungroup() %>% 
    filter(dateofdeath > day | is.na(dateofdeath)) %>%
    dplyr::select(-c(dateofdeath)) %>% 
    group_by(ecm_include_patient) %>% 
    mutate(alive_N = n(),
           alive_per = alive_N / N) %>%
    summarise_all(.,mean) %>%
    mutate(day = day) %>% 
    ungroup()
})

dta_alive <- do.call(rbind, dta_alive)



# Note - Pure control death rate flat?? ----
# R: Yes, probably some exclusion code from 'ECM_eligible.csv', originally 'WB_MISP_nov_2022.csv', means 'dead'
# See, how they only start to drop off in November 2022, which is the last month covered by WB_MISP

### Plot ----
# dta_alive$ecm_include_patient = factor(dta_alive$ecm_include_patient, levels = c('Pure control', 'Pure control (excluded Nov-22)', 'Control', 'Treatment'))
dta_alive$ecm_include_patient = factor(dta_alive$ecm_include_patient, levels = c('Pure control', 'Control', 'Treatment'))


sf(patient_ecm_eligible$ecm_include_patient)

g1 = ggplot(dta_alive,
            aes(x = day, y = alive_per,
                group = ecm_include_patient, col = ecm_include_patient))+
  geom_line(linewidth = 2) +
  
  scale_color_manual(name = '',
                     values = c('Treatment' = '#7EF86F',
                                'Control' = '#E0260C',
                                'Pure control' = 'grey60',
                                'Pure control (excluded Nov-22)' = 'black'
                     ))+
  
  scale_x_date(date_breaks = '1 month', date_labels = '%b-%y',
               expand = expansion(add = c(20, 20))) +
  scale_y_continuous(labels = percent_format()) + 
  
  labs(
    x = '<br>Day', y = 'Share of patients alive<br>',
    title = 'Survival rates by ECM treatment group<br>'
  ) +
  
  guides(color = guide_legend(override.aes = list(linewidth = 5))) +
  theme(
    legend.key.width = unit(3.5, 'cm'),
    legend.key.height = unit(3, 'cm'),
    legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(face = 'plain', size = 28, family = 'Calibiri'),
    legend.title = element_text(face = 'plain', size = 40, family = 'Calibiri'),
    plot.title  = element_markdown(size = 34, face = 'bold', family = 'Calibiri'),
    plot.caption  = element_textbox_simple(size = 11, family = 'Calibiri'),
    axis.ticks = element_line(),
    axis.title.x = element_markdown(size = 30, family = 'Calibiri'),
    axis.title.y = element_markdown(size = 30, family = 'Calibiri'),
    axis.text.x = element_text(size = 23, angle = 90, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 28, family = 'Calibiri'),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank())


ggsave(file.path(project_path, 'Figures', 'Deaths', paste0('Survival rates by group (custom)', ".png")),
       plot=g1, width = 47, height = 30, units = 'cm')




#
# END OF CODE ----
#
