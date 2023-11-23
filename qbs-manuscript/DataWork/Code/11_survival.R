

#
# SET-UP-------------------------------------------------------------------
# 

### Clean the environment
# rm(list=ls())
# gc()

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




#
# READ DATA -------------------------------------------------------------------
#

### Treatment groups  
patient_ecm_eligible_demo = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible_demo.csv')) %>% mutate(id = as.character(id))

# Create strata
patient_ecm_eligible_demo = patient_ecm_eligible_demo %>% 
  add_column(., .after = 'class_code', strata = paste(patient_ecm_eligible_demo$list_id, patient_ecm_eligible_demo$class_code))



### Outcomes period
dta_outcomes_period = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_period_09_23.parquet')) %>% mutate(id = as.character(id))


### Combine
dta_outcomes_period = left_join(dta_outcomes_period, patient_ecm_eligible_demo)


### Look up severe condtions
tapply(dta_outcomes_period$n_sever_diag > 0, paste(dta_outcomes_period$treat_period, ' - ', dta_outcomes_period$ecm_include_patient), pr)
tapply(dta_outcomes_period$n_sever_diag > 0, paste(dta_outcomes_period$treat_period, ' - ', dta_outcomes_period$ecm_include_patient), function(x) round(pr(x), 4))

tapply(dta_outcomes_period$stroke > 0, paste(dta_outcomes_period$treat_period, ' - ', dta_outcomes_period$ecm_include_patient), sf)
tapply(dta_outcomes_period$heart_failure > 0, paste(dta_outcomes_period$treat_period, ' - ', dta_outcomes_period$ecm_include_patient), pr)
tapply(dta_outcomes_period$myocardial_infarction > 0, paste(dta_outcomes_period$treat_period, ' - ', dta_outcomes_period$ecm_include_patient), pr)
tapply(dta_outcomes_period$copd > 0, paste(dta_outcomes_period$treat_period, ' - ', dta_outcomes_period$ecm_include_patient), pr)
tapply(dta_outcomes_period$pneumonia > 0, paste(dta_outcomes_period$treat_period, ' - ', dta_outcomes_period$ecm_include_patient), pr)




### All outcomes  ------------------------------------------------------------------------------
dta_diagnosis_all  = read_parquet(file.path(project_path, 'Data/Clean', 'dta_diagnosis.parquet')) %>% mutate(id = as.character(id))
dta_procedures_all = read_parquet(file.path(project_path, 'Data/Clean', 'dta_procedures.parquet')) %>% mutate(id = as.character(id))


# dta_outcomes = dta_diagnosis_all %>% 
#                   filter(n_sever_diag) %>% 
#                   dplyr::select(c(id, treat_period, year_month_day, 
#                                   n_sever_diag, stroke, heart_failure, myocardial_infarction, copd, pneumonia))

### Death dates ------------------------------------------------------------------------------
dta_deaths = read_parquet(file.path(project_path, 'Data', 'Clean', 'Deaths_all.parquet'))



#
# COX PROP. HAZARDS MODEL --------------------------------------------------------------------
#


### Fit the models  -------------------------------------------------------------------

#### To decide: --------------------------------------------------------
start1 = 20210601 # end of observation period
end1 = 20230331 # end of observation period
var1 = 'covid_test'  # outcome = var1
treat1 = 1  # treatment period = treat1
panel1 = 'b' # panel (risk group) = panel1 
# pre_var1 = 'all' # pre-treatment outcomes (only observations with/without) - 'yes', 'no', 'all' 
death1 = 0 # 0 = survival-to-event  VS.  1 = event-to-death 



### LOOP --------------------------------------------------------
count=0


for(var1 in c('n_inpatient',
              'n_sever_diag', 'stroke', 'heart_failure', 'myocardial_infarction', 'copd', 'pneumonia',
              'admit_ambulance', 'readmit_30_any', 'readmit_90_any', 'readmit_30_severe', 'readmit_90_severe',
              'asthma', 'diabetes_2', 'hypertension', 'cancer',
              'covid_test', 'covid_incidence', 'covid_vaccine')){
  
  # Control loop
  print(paste(var1))
  
  count = count+1
  
  for(panel1 in c('a', 'b', 'c')){
  
    print(panel1)
    
    if(panel1 == 'a'){risk_group = c('Mild/moderate', 'Severe')}
    if(panel1 == 'b'){risk_group = c('Mild/moderate')}
    if(panel1 == 'c'){risk_group = c('Severe')}
    
      
    ### Decide which dataset to use
    if(var1 %in% names(dta_procedures_all)){
        dta_outcomes_all = dta_procedures_all
    }else{
        dta_outcomes_all = dta_diagnosis_all
    }
      
  
    ### Find minimum date  of occurrence per patient-period
    temp = dta_outcomes_all %>% 
          mutate(var = !!rlang::ensym(var1)) %>% # ...iteratively define outcome variable
          filter(var == T | var == 1) %>% # ... filter only positive instances of the varia ble
          filter(treat_period == 1) %>%  # ... in the post-treatment period
          dplyr::select(c(id, treat_period, var, year_month_day)) %>% # ... only necessary columns
          group_by(id) %>%  # ... group by patient 
          filter(year_month_day == min(year_month_day)) %>%  # ... find first instance of the variable
          distinct() # .... leave only unique rows
    
      
      
    ### (Re-)add all ECM patients (previous command would remove anyone who didn't have a positive instance of this variable)
    temp = left_join(patient_ecm_eligible_demo, temp)
      
    
    
      ### Pre-treatment outcomes
      # pre_cond1 = dta_outcomes_all %>% 
      #                 filter(treat_period == 0)%>% 
      #                 mutate(var = !!rlang::ensym(var1)) %>% filter(var) %>% 
      #                 dplyr::select(c(id, treat_period, var)) %>%
      #                 filter(id %in% temp$id) %>% 
      #                 distinct()
                    
      # temp$var_pre = ifelse(temp$id %in% pre_cond1$id, T, F)
    
      # if(pre_var1 == 'yes'){
      #   temp = temp %>% filter((var_pre))
      # }
      # if(pre_var1 == 'no'){
      #   temp = temp %>% filter(!(var_pre))
      # }
    
        
    ### Event/death -------------------------------------------------
    if(death1 == 1){
      
      # Add death dates
      temp = left_join(temp, dta_deaths %>% dplyr::select(c(id, dateofdeath)))
      
      # If added, we only look at the patients WITH a given condition
      # temp = temp %>% filter(var)
    
      
      # Prepare data for Cox models
      temp = temp %>% 
        mutate(status = ifelse(is.na(dateofdeath), 0, 1), # ... classify if patient died
               time = ifelse(test = is.na(dateofdeath), 
                             yes = as.numeric(ymd(end1) - ymd(year_month_day)), #... time of survival = whole period since the event for those who survived
                             no  = as.numeric(dateofdeath - ymd(year_month_day))),  #... time of survival = date of death minus occurrence of the even
               age = ifelse(test = is.na(dateofdeath),  # ... get age at the date of last observation
                            yes = round(as.numeric(ymd(end1) - ymd(dateofbirth)) /  365.25, 1),
                            no  = round(as.numeric(dateofdeath  - ymd(dateofbirth)) /  365.25, 1))
        )
      
      
      
    }else{
      # Prepare data for Cox models
      temp = temp %>% 
        mutate(status = ifelse(is.na(var), 0, 1),# ... classify if patient died
               time = ifelse(test = is.na(year_month_day), 
                             yes = as.numeric(ymd(end1) - ymd(start1)), #... time of survival = whole period for those who survived
                             no  = as.numeric(year_month_day - ymd(start1))), #... time of survival = date of event minus start of ECM
               age = ifelse(test = is.na(year_month_day), # ... get age at the date of last observation
                            yes = round(as.numeric(ymd(end1) - ymd(dateofbirth)) /  365.25, 1),
                            no  = round(as.numeric(year_month_day    - ymd(dateofbirth)) /  365.25, 1)))
    }
    
    
    ### Re-level treatment variable
    temp$ecm_include_patient = factor(temp$ecm_include_patient , levels = c('Pure control','Control', 'Treatment'))
      
    
    ### Save the object for regressions 
    temp_save = temp
  
  
    ### (1) OLS (FE) -------------------------------------------------------------------
    temp = temp_save %>% filter(!(ecm_include_patient %in% c('Pure control'))) %>% 
              mutate(ecm_include_patient = droplevels(ecm_include_patient)) %>% 
              filter(class_code %in% risk_group)
    
    m1 <- lm(status  ~ strata + ecm_include_patient,
             data = temp)
    
    m1 = coeftest(m1, cluster.vcov(m1, temp$list_id, df_correction = T))
    
    n1 = nobs(m1)
    x1 = round(mean(temp$status[temp$ecm_include_patient != 'Treatment']), 3)
    
    
    
    ### (2) OLS (FE + age + gender) -------------------------------------------------------------------
    temp = temp_save %>% filter(!(ecm_include_patient %in% c('Pure control')))  %>% 
                mutate(ecm_include_patient = droplevels(ecm_include_patient)) %>% 
                filter(class_code %in% risk_group)
    
    m2 <- lm(status  ~ strata + age + gender + ecm_include_patient,
             data = temp)
    summary(m2)
    mean(temp$status[temp$ecm_include_patient == 'Treatment'])
    mean(temp$status[temp$ecm_include_patient != 'Treatment'])
    
    m2 = coeftest(m2, cluster.vcov(m2, temp$list_id, df_correction = T))
    
    n2 = nobs(m2)
    x2 = round(mean(temp$status[temp$ecm_include_patient != 'Treatment']), 3)
    
    
    
    ### (3) OLS (EXPANDED + age + gender) -------------------------------------------------------------------
    
    if(panel1 == 'a'){
      
      temp = temp_save %>% filter(!(ecm_include_patient %in% c('Control'))) %>% 
        mutate(ecm_include_patient = droplevels(ecm_include_patient)) 
      
      
      m3 <- lm(status  ~ block_categorical + age + gender + ecm_include_patient,
               data = temp)
      
      m3 = coeftest(m3, cluster.vcov(m3, temp$list_id, df_correction = T))
      
      n3 = nobs(m3)
      x3 = round(mean(temp$status[temp$ecm_include_patient != 'Treatment']), 3)
      
      
      
    }
    
    
    
    
    ### (4) COX (FE) -------------------------------------------------------------------
    temp = temp_save %>% filter(!(ecm_include_patient %in% c('Pure control')))  %>% 
      mutate(ecm_include_patient = droplevels(ecm_include_patient)) %>% 
      filter(class_code %in% risk_group)
    
    
    m4 <- coxph(Surv(time, status) ~ strata + ecm_include_patient, 
                cluster = list_id,
                data = temp)
    
    n4 = nobs(m4) # Doesn't seem to work, so default to OLS below
    
    x4 = round(mean(temp$status[temp$ecm_include_patient != 'Treatment']), 3)
    
    
    ### (5) COX (FE + age + gender) -------------------------------------------------------------------
    
    temp = temp_save %>% filter(!(ecm_include_patient %in% c('Pure control')))  %>% 
      mutate(ecm_include_patient = droplevels(ecm_include_patient)) %>% 
      filter(class_code %in% risk_group)
    
    
    m5 <- coxph(Surv(time, status) ~ strata + age + gender + ecm_include_patient, 
                cluster = list_id,
                data = temp)
    
    coef(summary(m5))
    
    n5 = nobs(m5)
    x5 = round(mean(temp$status[temp$ecm_include_patient != 'Treatment']), 3)
    
    # Save predictions for the plot
    temp = left_join(temp, basehaz(m5))
    temp$pred = temp$hazard * exp(predict(m5, newdata = temp, type = 'lp'))
    assign(paste0('pred_', panel1), temp %>% mutate('panel' = panel1))
    
    ### (6) COX (EXPANDED + age + gender) -------------------------------------------------------------------
    if(panel1 == 'a'){
      
      temp = temp_save %>% filter(!(ecm_include_patient %in% c('Control')))  %>%
        mutate(ecm_include_patient = droplevels(ecm_include_patient))
      
      m6 <- coxph(Surv(time, status) ~  block_categorical + age + gender + ecm_include_patient,
                  cluster = list_id,
                  data = temp)
      
      n6 = nobs(m6)
      x6 = round(mean(temp$status[temp$ecm_include_patient != 'Treatment']), 3)
    }
    
    
    
    ### Store all model results
    m_list <- list(extract_coeftest(m1, 0), extract_coeftest(m2, 0), if(exists('m3')){extract_coeftest(m3, 0)}else{data.frame(var = 'ecm_include_patientTreatment', beta = '')},
                   extract_coeftest(m4, 0), extract_coeftest(m5, 0), if(exists('m6')){extract_coeftest(m6, 0)}else{data.frame(var = 'ecm_include_patientTreatment', beta = '')})
    m_list <- m_list %>% reduce(full_join, by='var')
    
    m_list
    
    # m_list[nrow(m_list)+1, ] <- list('Fixed Effects',  'Strata', 'Strata', 'Bloc', 'Strata', 'Strata', 'Bloc')
    m_list[nrow(m_list)+1, ] <- list('Mean',x1, x2, x3, x4, x5, x6)
    # m_list[nrow(m_list)+1, ] <- list('N'   ,n1, n2, n3, n1, n2, n3)
    
    
    assign(paste0('m_panel_', panel1), m_list)
    
  
    ### Remove model objects before next iteration
    rm( list = Filter( exists, c('m1','m2', 'm3', 'm4', 'm5', 'm6')) ) 
    
    
  }

  dta_temp = rbindlist(list(m_panel_a %>% mutate(panel = 'a', outcome = var1, death1 = death1),
                            m_panel_b %>% mutate(panel = 'b', outcome = var1, death1 = death1), 
                            m_panel_c %>% mutate(panel = 'c', outcome = var1, death1 = death1)) )

                      
  if(count == 1){
    dta_table_save = dta_temp
  }else{
    dta_table_save = rbind(dta_table_save, dta_temp)
  }

}
count = 0
dta_table_save

# Save the results
fwrite(dta_table_save,  file.path(project_path, 'Data', 'Clean', 'Tables', 'tableXX_survival.csv'))




#
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#

### Re-read the table --------------------------------------------------------------------------------------------------
dta_table_save = fread(file.path(project_path, 'Data', 'Clean', 'Tables', 'tableXX_survival.csv'))


### Clean table -----------------------------------------------------------------------------------------------------
dta_table = dta_table_save

# Substitute all missing for dashes
dta_table[is.na(dta_table)] = '$-$'
dta_table[dta_table == ''] = '$-$'


# Clean column names
dta_table = dta_table %>% mutate(var  = dplyr::recode(var, 
                                                      'ecm_include_patientTreatment' = 'ECM Treatment',
                                                      'age' = 'Age (years)',
                                                      'genderMale' = 'Gender (male)'
                                                      ))


# Add empty rows section headings
dta_table = dta_table %>% arrange(panel)

dta_table = dta_table %>% add_row(.before = min(which(grepl('a', dta_table$panel)))) 
dta_table = dta_table %>% add_row(.before = min(which(grepl('b', dta_table$panel)))) 
dta_table = dta_table %>% add_row(.before = min(which(grepl('c', dta_table$panel))))




# Add clean outcome names from the dictionary

dta_table = left_join(dta_table, 
                      dict_outcomes %>% dplyr::select(c(var,name)) %>% rename('outcome' = 'var'))

# Change clean outcoe name to 'mean' in every second row
dta_table$name = ifelse(dta_table$var == 'Mean', 'Mean', dta_table$name)


# Put all columns in one dataframe column with LaTeX table separators
dta_table$cell1 = paste0(apply(dta_table %>% dplyr::select(c(name, starts_with('beta'))), 1, paste, collapse = "&"), '\\')


dta_table$cell1[c(which(grepl('^NA',dta_table$cell1)))] <- paste0('\\multicolumn{7}{r{.69\\textwidth}}{\\textbf{Panel ', 
                                                          c('A: All-risk patients','B: Mild-risk patients','C: Severe-risk patients'),
                                                          '}}\\')


### Replace $-$ with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)


### Print to copy to Overleaf  ---------------------------------------------------------------------------------------------------

panel1 = 'a'

for(panel1 in c('a','b','c')){
  cat(paste0('\n\nPanel: ', toupper(panel1), '\n\n'))
  count = count = 0
  for(i in (min(which(dta_table$panel == panel1))-1):max(which(dta_table$panel == panel1))){
    count = count + 1
    if(count %% 2 != 0){
      cat(paste0(dta_table$cell1[i],'\\\ \\hline\n'))
    }else{
      cat(paste0(dta_table$cell1[i],'\\', '\n'))
    }
  }
}





### Means ------------------------------------------------------------------
tapply(dta_deaths$status, dta_deaths$ecm_include_patient, function(x) pr(x)/100)

tapply(dta_deaths$status[dta_deaths$class_code == 'Mild/moderate'], 
       dta_deaths$ecm_include_patient[dta_deaths$class_code == 'Mild/moderate'], 
       function(x) pr(x)/100)

tapply(dta_deaths$status[dta_deaths$class_code == 'Severe'], 
       dta_deaths$ecm_include_patient[dta_deaths$class_code == 'Severe'], 
       function(x) pr(x)/100)





#
# PLOT SURVIVAL CURVES -------------------------------------------------------------------
#

table(dta_diagnosis_all$n_sever_diag >0, dta_diagnosis_all$n_inpatient)

var1 = 'n_sever_diag'

# for(var1 in c('n_sever_diag', 'stroke', 'heart_failure', 'myocardial_infarction', 'copd', 'pneumonia',
#               'covid_incidence', 'n_inpatient')){

for(var1 in c('n_inpatient')){
  
  #for(var1 in c('covid_incidence', 'n_inpatient')){
  
  ### Control loop
  print(var1)
  
  ### Define treatment period and corresponding dates
  treat1 = 1
  
  if(treat1 == 0){
    start1 = 20090101
    end1   = 20210531
    # if(grepl('covid', var1)){next}
    
  }
  
  if(treat1 == 1){
    start1 = 20210601
    end1   = 20230331
  }
  
  
  
  ### Loop over risk groups --------------------------------------------------------------------
  panel1 = 'a'
  
  for(panel1 in c('a', 'b', 'c')){
    
    ### Define risk group
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
    
    
    
    
    ### Find minimum date  of occurrence per patient-period
    temp = dta_diagnosis_all %>% 
      mutate(var = !!rlang::ensym(var1)) %>% # ...iteratively define outcome variable
      filter(var) %>% 
      filter(treat_period == treat1) %>% 
      dplyr::select(c(id, treat_period, var, year_month_day)) %>%
      group_by(id, treat_period) %>% 
      filter(year_month_day == min(year_month_day)) %>% 
      distinct()
    
    
    ### Add all patients
    temp = left_join(patient_ecm_eligible_demo %>% filter(class_code %in% risk_group), temp) %>%
      filter(class_code %in% risk_group) %>% filter(!(ecm_include_patient %in% c('Pure control')))
    
    
    ### Prepare data for Cox models
    temp = temp %>% 
      mutate(status = ifelse(is.na(var), 0, 1),
             time = ifelse(test = is.na(year_month_day), 
                           yes = as.numeric(ymd(end1) - ymd(start1)),
                           no  = as.numeric(year_month_day - ymd(start1))),
             age = ifelse(test = is.na(year_month_day), 
                          yes = round(as.numeric(ymd(end1) - ymd(dateofbirth)) /  365.25, 1),
                          no  = round(as.numeric(year_month_day    - ymd(dateofbirth)) /  365.25, 1))
      )
    
    
    nrow(temp)
    n_distinct(temp$id)
    sf(temp$ecm_include_patient)
    sf(patient_ecm_eligible_demo$ecm_include_patient)
    
    ### Leave only ECM control and treatment --------------------------------------------------------------------
    temp = temp 
    
    
    fit <- survfit(Surv(time, status) ~ ecm_include_patient,
                   data = temp)
    
    ymin1 = pr(temp$status)[1]/100
    
    ###  Plot  ----------------------------------------------------------------------------------------
    
    library(survminer)
    
    g1 = ggsurvplot(
      fit,
      data = temp,
      size = 1,                 # change line size
      palette = c("grey67", "black"),# custom color palettes
      conf.int = TRUE,          # Add confidence interval
      pval = TRUE,  
      pval.method	 = T,
      pval.coord = c(50, ymin1+(1-ymin1)/3 ),
      
      risk.table = TRUE,        # Add risk table
      risk.table.col = "strata",# Risk table color by groups
      ylim = c(ymin1, 1),         # present narrower X axis, but not affect
      xlim = c(0, ifelse(treat1 == 1, 668, 4533)),
      
      xlab = paste0("\n Days ", ifelse(treat1 == 1, 'since', 'until'), " ECM onset (June 2021)"),   # customize X axis label.
      ylab = "\n Survival probability\n",   # customize Y axis label.
      
      # break.time.by = 90,     # break X axis in time intervals by 500.
      legend.labs = c("ECM Control", "ECM Treatment"),    # Change legend labels
      risk.table.height = 0.25, # Useful to change when you have multiple groups
      risk.table.y.text.col = T, # colour risk table text annotations.
      risk.table.y.text = FALSE, # show bars instead of names in text annotations
      #ggtheme = theme_light(), # customize plot and risk table with a theme.
      
      title = paste0('Survival probability until <b>', dict_outcomes$name[dict_outcomes$var == var1], '</b><br>
                     by ECM groups'),
      subtitle = paste0(subtitle1, '<br>'),
      caption = '<br><br><b>Notes:</b> The plot shows survival probability
                based on <b>Cox-Proportional Hazards Model</b>, which 
                 measures survival times (in days) from 01/06/2021,  censored at 31/03/2023 for the patients who survived the entire period.
                 for ECM control and treatment patients.
                <b>P-value is calculated using log-rank test.</b><br><br>',
      ggtheme = t1
      
    )
    
    png(file.path(project_path, 'Figures', 'Survival', ifelse(treat1 == 1, 'Post-treatment', 'Pre-treatment'),
                  paste0(var1, ' - survival rates by group (', 
                         subtitle1,
                         ").png")),
        width = 4000, height = 4000, res = 210, pointsize = 20)
    
    print(g1)
    
    
    
    dev.off()
    
    
  }
  
}   





#
# END OF CODE ----
#
