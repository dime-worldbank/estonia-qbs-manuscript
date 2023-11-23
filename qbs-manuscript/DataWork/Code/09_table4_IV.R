

#
# SET-UP ----
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

file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)


#
# PREPARE DATA ---------------------------------------------------------------------------------------------------
#

### Treatment groups   ---------------------------------------------------------------------------------------------------
patient_ecm_eligible = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible_demo.csv')) %>% mutate(id = as.character(id))

# Create strata + age
patient_ecm_eligible = patient_ecm_eligible %>% 
  add_column(., .after = 'class_code', strata = paste(patient_ecm_eligible$list_id, patient_ecm_eligible$class_code)) %>% 
  add_column(., .after = 'dateofbirth', age  = round(as.numeric(ymd(20210601) - ymd(patient_ecm_eligible$dateofbirth)) /  365.25, 0))


### ECM acceptance   ---------------------------------------------------------------------------------------------------
patient_ecm_accept = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_accept.csv')) %>% mutate(id = as.character(id))

# Combine
patient_ecm_eligible_accept = left_join(patient_ecm_eligible, patient_ecm_accept) %>% 
  mutate(ecm_status_patient = ifelse(is.na(ecm_status_patient), 0, 1))

### Outcomes  ---------------------------------------------------------------------------------------------------
dta_outcomes_09 = read_parquet(file.path(project_path, 'Data/Clean', 'All_outcomes_period_09_23.parquet')) %>% mutate(id = as.character(id))
dta_outcomes = read_parquet(file.path(project_path, 'Data/Clean', 'All_outcomes_period_18_23.parquet')) %>% mutate(id = as.character(id))



### Combine  ---------------------------------------------------------------------------------------------------
dta_outcomes_09 =  left_join(dta_outcomes_09, patient_ecm_eligible_accept) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))

dta_outcomes =  left_join(dta_outcomes, patient_ecm_eligible_accept) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))


### Leave only the required ECM groupings  ---------------------------------------------------------------------------------------------------
dta_outcomes_09 = dta_outcomes_09 %>% filter(!is.na(ecm_include_patient)) 
dta_outcomes    = dta_outcomes %>% filter(!is.na(ecm_include_patient)) 


### Leave only the selected groups 
dta_outcomes = dta_outcomes %>% filter(!is.na(ecm_include_patient) & ecm_include_patient %in% c('Control', 'Treatment'))


### Types of units
table(dta_outcomes$ecm_status_patient[dta_outcomes$treat_period == 1],
      dta_outcomes$ecm_include_patient[dta_outcomes$treat_period == 1])



# Prop. of Compliers and Always-Takers
1517/1722

# Prop of Compliers
1312/1517 - 158/3114


### Enrolment by risk class
# table(patient_ecm_eligible_accept$ecm_include_patient, patient_ecm_eligible_accept$ecm_status_patient, patient_ecm_eligible_accept$class_code)



#
# REGRESSION FUNCTION  ---------------------------------------------------------------------------------------------------
# 



type1 = 'count'  # count  / dummy - what version of the outcome should be used?
var1 = 'start_end_diff' # what outcome variable should be used
var2 = 'start_end_diff' # what outcome variable should be used
group1 = 'Control' # what is the reference group?
group2 = 'Treatment' # what is the treatment group?
winsorize1 = 0.999 # winsorizing maximum
# year1 = 2009 # Define if the pre-treatment outcomes should be measured from 2009 or 2018 (no other options for now here)
risk1 = c('Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 
risk1_lab = ifelse(all(risk1 == c('Mild/moderate', 'Severe')),  'all',  ifelse(all(risk1 == c('Severe')),   'severe', 'mild'))


iv_regression = function(type1, var2, group1, group2, winsorize1, risk1){
  
  ### Define parameters
  var1 = var2
  
  
  ### Prepare the dataset
  dta_reg = dta_outcomes %>% 
    filter(class_code %in% risk1) %>% 
    filter(treat_period == 1) %>% 
    mutate(var = !!rlang::ensym(var1)) %>% # ...iteratively define outcome variable
    mutate(var = Winsorize(as.numeric(var), minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7)) %>%  # ..winsorize at pre-defined level
    mutate(var = as.numeric(var) * 12/22) %>% # ...annualize
    filter(ecm_include_patient %in% c(group1, group2)) # ..filter the groups
  
  if(type1 == 'dummy'){ # Convert to 0/1 dummies
    dta_reg = dta_reg %>% mutate(var = ifelse(var > 0, 1, 0))
  }
  
  ### Add pre-treatment dummies (2018-2021)
  pre_treat18 = dta_outcomes %>% 
    filter(class_code %in% risk1) %>% 
    filter(treat_period == 0) %>% 
    mutate(var_pre18 = !!rlang::ensym(var1)) %>% # ...iteratively define outcome variable
    mutate(var_pre18 = Winsorize(as.numeric(var_pre18), minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7)) %>%  # ..winsorize at pre-defined level
    mutate(var_pre18 = ifelse(var_pre18 > 0, 1, 0)) %>%  # ... convert to dummies
    filter(ecm_include_patient %in% c(group1, group2)) %>%  # ...filter the groups
    dplyr::select(c(id, var_pre18)) # ... leave only relevant columns
  
  dta_reg = left_join(dta_reg, pre_treat18)
  
  
  ### Add pre-treatment dummies (2009-2021)
  pre_treat09 = dta_outcomes_09 %>% 
    filter(class_code %in% risk1) %>% 
    filter(treat_period == 0) %>% 
    mutate(var_pre09 = !!rlang::ensym(var1)) %>% # ...iteratively define outcome variable
    mutate(var_pre09 = Winsorize(as.numeric(var_pre09), minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7)) %>%  # ..winsorize at pre-defined level
    mutate(var_pre09 = ifelse(var_pre09 > 0, 1, 0)) %>%  # ... convert to dummies
    filter(ecm_include_patient %in% c(group1, group2))%>%  # ...filter the groups
    dplyr::select(c(id, var_pre09)) # ... leave only relevant columns
  
  dta_reg = left_join(dta_reg, pre_treat09)
  
  
  if(type1 == 'dummy'){ # Convert to 0/1 dummies
    dta_reg = dta_reg %>% mutate(var = ifelse(var > 0, 1, 0),
                                 var_pre18 = ifelse(var_pre18 > 0, 1, 0),
                                 var_pre09 = ifelse(var_pre09 > 0, 1, 0))
    }
  
  
  ### Reference category
  dta_reg$ecm_include_patient = relevel(factor(dta_reg$ecm_include_patient), ref = group1)
  # dta_reg$ecm_status_patient = relevel(factor(dta_reg$ecm_status_patient), ref = 'Not ECM')
  
  
  if(group1 == 'Pure control'){
    m1 = iv_robust(var ~ ecm_status_patient  + age + gender  | ecm_include_patient  + age + gender, 
                   cluster = list_id, fixed_effects = ~strata, data = dta_reg)
    
    m2 = iv_robust(var ~ ecm_status_patient + var_pre18  + age + gender | ecm_include_patient  + var_pre18  + age + gender, 
                   cluster = list_id, fixed_effects = ~strata, data = dta_reg)
    
    m3 = iv_robust(var ~ ecm_status_patient + var_pre09  + age + gender | ecm_include_patient  + var_pre09  + age + gender, 
                   cluster = list_id, fixed_effects = ~strata, data = dta_reg)
    
  }else if(group1 == 'Control'){
    m1 = iv_robust(var ~ ecm_status_patient  + age + gender  | ecm_include_patient  + age + gender, 
                   cluster = list_id, fixed_effects = ~strata, data = dta_reg)
    
    m2 = iv_robust(var ~ ecm_status_patient + var_pre18  + age + gender | ecm_include_patient  + var_pre18  + age + gender, 
                   cluster = list_id, fixed_effects = ~strata, data = dta_reg)
    
    m3 = iv_robust(var ~ ecm_status_patient + var_pre09  + age + gender | ecm_include_patient  + var_pre09  + age + gender, 
                   cluster = list_id, fixed_effects = ~strata, data = dta_reg)
  }else{
    print(group1)  
    break # If different groups used, the models might need to be adjusted, so break for now in that case
  }
  
  if(is.na(coef(m2))[length(coef(m2))]){m2 = ''}
  if(is.na(coef(m3))[length(coef(m3))]){m3 = ''}
  
  
  tryCatch({m1 <-  extract_coeftest(m1, 3) %>% filter(var == 'ecm_status_patient') %>% rename('beta_ancova' = 'beta')}, error = function(e) {m1 <<- data.frame('var' = var1, beta_ancova = '')})
  tryCatch({m2 <-  extract_coeftest(m2, 3) %>% filter(var == 'ecm_status_patient') %>% rename('beta_ancova18' = 'beta')}, error = function(e) {m2 <<- data.frame('var' = var1, beta_ancova18 = '')})
  tryCatch({m3 <-  extract_coeftest(m3, 3) %>% filter(var == 'ecm_status_patient') %>% rename('beta_ancova09' = 'beta')}, error = function(e) {m3 <<- data.frame('var' = var1, beta_ancova09 = '')})
  

    
  ### Combine all models and specify the DV
  m_all = left_join(m1, left_join(m2, m3))
  
  m_all$var = var1
  
  return(m_all)  
   
}


var2  = 'p_statins'
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 

iv_regression('count', var2, 'Control', 'Treatment', .999, risk1)

iv_regression('count', var2, 'Control', 'Treatment', .999, c('Mild/moderate', 'Severe'))
iv_regression('count', var2, 'Control', 'Treatment', .999, c('Mild/moderate'))
iv_regression('count', var2, 'Control', 'Treatment', .999, c('Severe'))



### NOTE: Why is it not working with var1, but only with var2 and var1=var2 inside the function?  ------------------------------------------------------------------------------------------

#
# CREATE TABLE  ---------------------------------------------------------------------------------------------------
# 




### CHOOSE: Define risk level ---------------------------------------------------------------------------------------------------
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 
risk1_lab = ifelse(all(risk1 == c('Mild/moderate', 'Severe')),  'all',  ifelse(all(risk1 == c('Severe')),   'severe', 'mild'))




### Define outcomes variables --------------------------------------------------------------------------------------------------
vars = names(dta_outcomes)[which(names(dta_outcomes) == 'start_end_diff'):which(names(dta_outcomes) == 'p_delay')]


risk1_lab = 'all'

for(risk1_lab in c('all', 'mild', 'severe')){
  
  if(risk1_lab == 'all'){
    risk1 = c('Mild/moderate', 'Severe')
  }else if(risk1_lab == 'mild'){
    risk1 = c('Mild/moderate')
  }else if(risk1_lab == 'mild'){
    risk1 = c('Severe')
  }
  
  
  
  if("tidylog" %in% (.packages())){detach("package:tidylog", unload=TRUE)} # Otherwise it prints way too many messages from the loop below
  
  count = 0
  
  for(var1 in vars){  
  
    print(var1)
    count = count + 1
  
    ### Combine....
    # ... all bits as lists
    dfs1 = list(
      
      # # ... Pre-treatment - C: ECM control vs. ECM treatment (count/N)
      # iv_regression('count', var1, 'Control', 'Treatment', .999) %>% 
      #   rename_with(.cols = -c(var), .fn = ~paste0(., '_pre_c_n')),
      # 
      # # ... Pre-treatment -  C: ECM control vs. ECM treatment (dummy)
      # iv_regression('dummy', var1, 'Control', 'Treatment', .999) %>%
      #   rename_with(.cols = -c(var), .fn = ~paste0(., '_pre_c_d')),
      
      # ... Post-treatment - C: ECM control vs. ECM treatment (count/N)
      iv_regression('count', var1, 'Control', 'Treatment', .999, risk1) %>% 
        rename_with(.cols = -c(var), .fn = ~paste0(., '_c_n')),
      
      # ... Post-treatment -  C: ECM control vs. ECM treatment (dummy)
      iv_regression('dummy', var1, 'Control', 'Treatment', .999, risk1) %>%
        rename_with(.cols = -c(var), .fn = ~paste0(., '_c_d'))
  
    )
    
    # ... merge together
    
    
    library(plyr) # NOTE: 'plyr' messses with a lot of other functions, so we detach it right away
    temp = join_all( dfs = dfs1, by = 'var', type = 'left')
    detach("package:plyr", unload=TRUE)
  
    if(count == 1){
      dta_table = temp
    }else{
      dta_table = rbind(dta_table, temp)
    }
  }
  
  ### Save object for cleaning   ---------------------------------------------------------------------------------------------------
  dta_table_save = dta_table
  
  
  fwrite(dta_table_save, file.path(project_path, 'Tables', 'as_csv', risk1_lab, 'table4_IV.csv'))
  
  
  
}



#
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#

### Re-read the table --------------------------------------------------------------------------------------------------
year1 =  2018  # 2009 / 2018
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 
risk1_lab = ifelse(all(risk1 == c('Mild/moderate', 'Severe')),  'all',  ifelse(all(risk1 == c('Severe')),   'severe', 'mild'))

dta_table_save = fread(file.path(project_path,  'Tables', 'as_csv' ,risk1_lab, 'table4_IV.csv'))



### Extract and remove outcomes that are empty - those will mess with F-statistics
vars_zero = dta_table_save$var[is.na(dta_table_save$dta_table_save$beta_class_c_n)]

dta_table = dta_table_save[!is.na(dta_table_save$beta_pre_c_n),]




### Clean coefficients -----------------------------------------------------------------------------------------------------

dta_table = dta_table_save


# Time differences - no dummies
dta_table[grepl('diff', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'

# No. diagnoses/procedures - no dummies
dta_table[grepl('n_diag|n_prcoedures', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'


# Prices - no dummies
dta_table[grepl('price', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'


# ECM consultation codes - no ANCOVA
dta_table[grepl('consult_include|consult_care_plan|consult_refuse', dta_table$var), 1:ncol(dta_table)]  = '$-$'

# Anything empty
dta_table[dta_table == ''] = '$-$'




### Clean based on dictionary of outcomes ---------------------------------------------------------------------------------------------------

# Higlight any missing variables
dta_table$var[!(dta_table$var %in% dict_outcomes$var)]


dta_table = left_join(dta_table, 
                      dict_outcomes %>% dplyr::select(c(var,name, group, order, nr))) %>%
  arrange(order, nr) %>% 
  relocate(.,c('group','name'), .after = 1) %>% 
  filter(!is.na(group)) %>% 
  dplyr::select(-c(var, order, nr))



# Put all columns in one dataframe column with LaTeX table separators
dta_table$cell1 = paste0(apply(dta_table %>% dplyr::select(-c(group)), 1, paste, collapse = "&"), '\\')

# Add section headings
for(group1 in unique(dta_table$group)){
  min1 = min(which(dta_table$group == group1))
  dta_table = dta_table %>% add_row(.before = min1)
  dta_table$cell1[min1] = paste0('\\multicolumn{7}{r{.45\\textwidth}}{\\textbf{', group1, '}}\\')
}


### Replace $-$ with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)


### Print to copy to Overleaf  ---------------------------------------------------------------------------------------------------
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}








# '  ---------------------------------------------------------------------------------------------------
# SCRAPBOOK --------------------------------------------------------------------------------------------------
#



### Manual Checks----

dta_outcomes$ecm_status_patient = relevel(factor(dta_outcomes$ecm_status_patient), ref = 'Not ECM')

names(dta_outcomes)
m1 = iv_robust(n_inpatient > 0 ~ ecm_status_patient  | ecm_include_patient, 
               cluster = list_id, fixed_effects = ~strata, data = dta_outcomes)

summary(m1)


m2 = ivreg(n_inpatient > 0 ~ ecm_status_patient  | ecm_include_patient, 
           data = dta_outcomes)

summary(m2)

coeftest(m2, cluster.vcov(m2, dta_outcomes$strata, df_correction = T))
# R: Almost (sic!) matches the results of the same model estimated using iv_robust()

first <- lm(ecm_status_patient == 'ECM accept' ~ ecm_include_patient, data = dta_outcomes)
vote_pred <- predict(first)
second <- lm(deceased > 0 ~ vote_pred, data = dta_outcomes)
summary(second)



### Alternative model specifications (from old scripts) ----
#m1 = glm(value ~  strata + group, data = dta_reg, family = binomial(link = 'logit'))


# Tobit regression model
# m1 = tobit(value ~ strata + group, left = 0, right = quantile(dta_reg$value, 0.75),
#                data = dta_reg)


# Poisson regression model
# m1 = tryCatch(
#                   expr = {return(glm(value ~  strata + group, data = hospitalization_reg, family = 'poisson'))},
#                   error = function(e){return(m1 = lm(value ~  strata + group, data = hospitalization_reg))}
#                   ) 


#
# END OF CODE ----
#