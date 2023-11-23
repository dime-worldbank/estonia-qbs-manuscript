

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

# Add care plan quality 
care = fread(file.path(project_path, 'Data', 'Clean', 'Other', 'Care plan evaluations.csv'))

patient_ecm_eligible = left_join(patient_ecm_eligible, care)
tapply(patient_ecm_eligible$list_id, patient_ecm_eligible$care_plan_ev_pc1 %>% is.na, n_distinct)


### Outcomes  ---------------------------------------------------------------------------------------------------
dta_outcomes_09 = read_parquet(file.path(project_path, 'Data/Clean', 'All_outcomes_period_09_23.parquet')) %>% mutate(id = as.character(id))
dta_outcomes = read_parquet(file.path(project_path, 'Data/Clean', 'All_outcomes_period_18_23.parquet')) %>% mutate(id = as.character(id))


### Combine  ---------------------------------------------------------------------------------------------------
dta_outcomes_09 =  left_join(dta_outcomes_09, patient_ecm_eligible) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))

dta_outcomes =  left_join(dta_outcomes, patient_ecm_eligible) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))



### Add QBS scores
list_all = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'list_all.csv'))

dta_outcomes    = left_join(dta_outcomes, list_all %>% dplyr::select(c(list_id, qbs_II, management_score)))
dta_outcomes_09 = left_join(dta_outcomes_09, list_all %>% dplyr::select(c(list_id, qbs_II, management_score)))


### Add care plan evaluations 
care = fread(file.path(project_path, 'Data', 'Clean', 'Other', 'Care plan evaluations.csv'))

dta_outcomes    = left_join(dta_outcomes, care %>% dplyr::select(c(list_id, care_plan_ev_pc1)))
dta_outcomes_09 = left_join(dta_outcomes_09, care %>% dplyr::select(c(list_id, care_plan_ev_pc1)))


# temp = dta_outcomes %>% dplyr::select(c(list_id, qbs_II, management_score, care_plan_ev_pc1)) %>% distinct()



### Leave only the required ECM groupings  ---------------------------------------------------------------------------------------------------
dta_outcomes_09 = dta_outcomes_09 %>% filter(!is.na(ecm_include_patient)) 
dta_outcomes    = dta_outcomes %>% filter(!is.na(ecm_include_patient)) 



#
# INTERACTION REG. FUNCTION  ---------------------------------------------------------------------------------------------------
# 
'consult_gp'
'consult_any'

type1 = 'count'  # count  / dummy - what version of the outcome should be used?
var1 = 'p_key' # what outcome variable should be used
var2 = 'p_key' # what outcome variable should be used
group1 = 'Control' # what is the reference group?
group2 = 'Treatment' # what is the treatment group?
winsorize1 = 0.999 # winsorizing maximum
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 
risk1_lab = ifelse(all(risk1 == c('Mild/moderate', 'Severe')),  'all',  ifelse(all(risk1 == c('Severe')),   'severe', 'mild'))



interact_regression = function(type1, var2, group1, group2, winsorize1, risk1){
  
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
  
  
  # Subset to a given set of risk patients
  if(group1 == 'Control'){
    dta_reg = dta_reg %>% filter(class_code %in% risk1)
    if(all(risk1 == c('Mild/moderate', 'Severe'))){
      dta_reg$fe = paste0(dta_reg$block_categorical, dta_reg$class_code)
    }else{
      dta_reg$fe = dta_reg$block_categorical
      
    }
  }
  
  
  # print(tapply(dta_reg$var_pre18, paste( dta_reg$ecm_include_patient, dta_reg$treat_period), sf))
  
  ### Reference category
  dta_reg$ecm_include_patient = relevel(factor(dta_reg$ecm_include_patient), ref = group1)

    
  if(group1 == 'Pure control'){
    # m1 = lm(var ~  factor(block_categorical) + qbs_II        * ecm_include_patient, data = dta_reg)
    # m2 = lm(var ~  factor(block_categorical) + comorbidities * ecm_include_patient, data = dta_reg)
    # m3 = lm(var ~  factor(block_categorical) + var_pre * ecm_include_patient, data = dta_reg)
  }else if(group1 == 'Control'){
    m1 = lm(var ~  factor(fe)  + age + gender + qbs_II  * ecm_include_patient, data = dta_reg)
    m2 = lm(var ~  factor(fe)  + age + gender + management_score  * ecm_include_patient, data = dta_reg)
    # m2 = lm(var ~  factor(list_id)   + age + gender+ class_code * ecm_include_patient, data = dta_reg)
    m3 = lm(var ~  factor(fe)   + age + gender + care_plan_ev_pc1   * ecm_include_patient, data = dta_reg)
    m4 = lm(var ~  factor(strata)   + age + gender + var_pre18   * ecm_include_patient, data = dta_reg)
    #m4 = lm(var ~  factor(strata)   + age + gender + var_pre09   * ecm_include_patient, data = dta_reg)
  }else{
    print(group1)  
    break # If different groups used, the models might need to be adjusted, so break for now in that case
  }

  
  # summary(m1)
  # summary(m2)
  # summary(m3)
  # summary(m4)

  if(is.na(coef(m3))[length(coef(m3))]){m3 = ''}
  if(is.na(coef(m4))[length(coef(m4))]){m4 = ''}
  
  ### Cluster SE
  m1 = extract_coeftest(coeftest(m1, cluster.vcov(m1, dta_reg$list_id, df_correction = T)), 1) %>%
    pivot_wider(names_from = var,  values_from = beta) %>% 
    add_column(.before = 1, 'var' = var1)
  
  names(m1) = c('var', 'beta_qbs', 'interact_qbs')


  m2 = extract_coeftest(coeftest(m2, cluster.vcov(m2, dta_reg$list_id, df_correction = T)), 1) %>%
    pivot_wider(names_from = var,  values_from = beta) %>% 
    add_column(.before = 1, 'var' = var1)
  
  # m2 = m2[, c(1,3,2)]
  names(m2) = c('var', 'beta_mng', 'interact_mng')
  
  
  # If everyone is 1 on the pre-treatment variables, the model would produce an error, so avoid that and produce a matching empty dataframe instead

  tryCatch({
    m3 <- extract_coeftest(coeftest(m3, cluster.vcov(m3, dta_reg$list_id, df_correction = T)), 1) %>%
      pivot_wider(names_from = var,  values_from = beta) %>% 
      add_column(.before = 1, 'var' = var1)
    m3 = m3[, c(1,3,2)]
    names(m3) <- c('var', 'beta_careq', 'interact_careq')
    }, error = function(e) { m3 <<- data.frame('var' = var1, beta_careq = '', interact_careq = '')})
 
  tryCatch({
    m4 <- extract_coeftest(coeftest(m4, cluster.vcov(m4, dta_reg$list_id, df_correction = T)), 1) %>%
      pivot_wider(names_from = var,  values_from = beta) %>% 
      add_column(.before = 1, 'var' = var1)
    names(m4) <- c('var', 'beta_pre18', 'interact_pre18')
  }, error = function(e) {m4 <<- data.frame('var' = var1, beta_pre18 = '', interact_pre18 = '')})
  
  
  
  if(group1 %in% c('Pure control')){
    ### Combine all models and specify the DV
    m_all = left_join(m1, left_join(m2, m3))
  }
  if(group1 %in% c('Control')){    ### Combine all models and specify the DV
    m_all = left_join(m1, left_join(m2, left_join(m3, m4)))
    }

  return(((m_all)))
  
  
}

names(dta_outcomes)
var2 = 'p_b_anti_thromb'
risk1 = c('Mild/moderate', 'Severe')

interact_regression('count', var2, 'Control', 'Treatment', .999, c('Mild/moderate', 'Severe'))

interact_regression('count', var2, 'Control', 'Treatment', .999, c('Mild/moderate', 'Severe'))
interact_regression('count', var2, 'Control', 'Treatment', .999, c('Mild/moderate'))
interact_regression('count', var2, 'Control', 'Treatment', .999, c('Severe'))



### Compare to ECM care plan quality effects among treatment grouip only   ---------------------------------------------------------------------------------------------------

var1 = 'p_key'

temp = dta_outcomes %>% 
  filter(class_code %in% risk1) %>% 
  filter(treat_period == 1) %>% 
  mutate(var = !!rlang::ensym(var1)) %>% # ...iteratively define outcome variable
  mutate(var = Winsorize(as.numeric(var), minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7)) %>%  # ..winsorize at pre-defined level
  mutate(var = as.numeric(var) * 12/22) %>% # ...annualize
  filter(ecm_include_patient %in% c('Treatment', 'Control')) # ..filter the groups

m1 = lm(var ~  factor(block_categorical)   + age + gender + care_plan_ev_pc1, data = temp)

summary(m1)

### NOTE: Why is it not working with var1, but only with var2 and var1=var2 inside the function?


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
      
  
      # ... C: ECM control vs. ECM treatment (count/N)
      interact_regression('count', var1, 'Control', 'Treatment', .999, risk1) %>% 
        rename_with(.cols = -c(var), .fn = ~paste0(., '_c_n')),
      
      # ... C: ECM control vs. ECM treatment (dummy)
      interact_regression('dummy', var1, 'Control', 'Treatment', .999, risk1) %>%
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
  
  count = 0
  
  ### Save object for cleaning   ---------------------------------------------------------------------------------------------------
  dta_table_save = dta_table
  
  
  fwrite(dta_table_save, file.path(project_path, 'Tables','as_csv', risk1_lab, 'table3_interact.csv'))

}


 

#
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#

### Re-read the table --------------------------------------------------------------------------------------------------
year1 =  2018  # 2009 / 2018
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 
risk1_lab = ifelse(all(risk1 == c('Mild/moderate', 'Severe')),  'all',  ifelse(all(risk1 == c('Severe')),   'severe', 'mild'))

dta_table_save = fread(file.path(project_path, 'Tables','as_csv',  risk1_lab, 'table3_interact.csv'))



### Clean coefficients -----------------------------------------------------------------------------------------------------

dta_table = dta_table_save


# Time differences - no dummies
dta_table[grepl('diff', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'

# No. diagnoses/procedures - no dummies
dta_table[grepl('n_diag|n_prcoedures', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'


# Prices - no dummies
dta_table[grepl('price', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'


# ECM consultation codes - no ANCOVA
dta_table[grepl('consult_include|consult_care_plan|consult_refuse', dta_table$var),
          which(grepl('_pre', names(dta_table)))]  = '$-$'

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



# Put all columns in one dataframe column with LaTeX table separators  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = paste0(apply(dta_table %>% dplyr::select(-c(group)), 1, paste, collapse = "&"), '\\')

# Add section headings
for(group1 in unique(dta_table$group)){
  min1 = min(which(dta_table$group == group1))
  dta_table = dta_table %>% add_row(.before = min1)
  dta_table$cell1[min1] = paste0('\\multicolumn{17}{r{.45\\textwidth}}{\\textbf{', group1, '}}\\')
}


### Replace $-$ with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)


### Print to copy to Overleaf  ---------------------------------------------------------------------------------------------------
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}






# ' ----
# REST ----
#

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