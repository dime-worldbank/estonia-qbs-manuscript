

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


### Outcomes  ---------------------------------------------------------------------------------------------------
dta_outcomes_09 = read_parquet(file.path(project_path, 'Data/Clean', 'All_outcomes_period_09_23.parquet')) %>% mutate(id = as.character(id))
dta_outcomes = read_parquet(file.path(project_path, 'Data/Clean', 'All_outcomes_period_18_23.parquet')) %>% mutate(id = as.character(id))


### Combine  ---------------------------------------------------------------------------------------------------
dta_outcomes_09 =  left_join(dta_outcomes_09, patient_ecm_eligible) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))

dta_outcomes =  left_join(dta_outcomes, patient_ecm_eligible) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))


### Leave only the required ECM groupings  ---------------------------------------------------------------------------------------------------
dta_outcomes_09 = dta_outcomes_09 %>% filter(!is.na(ecm_include_patient)) 
dta_outcomes    = dta_outcomes %>% filter(!is.na(ecm_include_patient)) 






#
# REGRESSION FUNCTION  ---------------------------------------------------------------------------------------------------
# 



type1 = 'count'  # count  / dummy - what version of the outcome should be used?
var1 = 'consult_any' # what outcome variable should be used
group1 = 'Control' # what is the reference group?
group2 = 'Treatment' # what is the treatment group?
winsorize1 = 0.999 # winsorizing maximum
year1 = 2018 # Define if the pre-treatment outcomes should be measured from 2009 or 2018 (no other options for now here)
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 
risk1_lab = ifelse(all(risk1 == c('Mild/moderate', 'Severe')),  'all',  ifelse(all(risk1 == c('Severe')),   'severe', 'mild'))




cross_regression = function(type1, var2, group1, group2, winsorize1, year1, risk1){
  
  ### Define parameters
  var1 = var2
  
  ### Define dataset to use
  dta_reg_save = dta_outcomes
  if(year1 == 2009){dta_reg_save = dta_outcomes_09}
  
  
  ### Prepare the dataset
  dta_reg = dta_reg_save  %>% 
    filter(treat_period == 1) %>% 
    mutate(var = !!rlang::ensym(var1)) %>% # ...iteratively define outcome variable
    mutate(var = Winsorize(as.numeric(var), minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7)) %>%  # ..winsorize at pre-defined level
    mutate(var = as.numeric(var) * 12/22) %>% # ...annualize
    filter(ecm_include_patient %in% c(group1, group2)) # ..filter the groups
    
  
  ### Add pre-treatment outcomes of a selected variable (annualized)
  pre_treat = dta_reg_save %>% 
    filter(class_code %in% risk1) %>% 
    filter(treat_period == 0) %>% 
    mutate(var_pre = !!rlang::ensym(var1)) %>% # ..iteratively define outcome variable
    mutate(var_pre = Winsorize(as.numeric(var_pre), minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7)) %>%  # ..winsorize at pre-defined level
    mutate(var_pre = as.numeric(var_pre) * ifelse(year1 == 2018, 12/41, 12/149)) %>%  # ...annualize
    filter(ecm_include_patient %in% c(group1, group2)) %>% # ..filter the groups
    dplyr::select(c(id, var_pre)) # ... leave only relevant columns
  
  
  dta_reg = left_join(dta_reg, pre_treat)

  if(type1 == 'dummy'){ # Convert to 0/1 dummies
    dta_reg = dta_reg %>% mutate(var = ifelse(var > 0, 1, 0),
                                 var_pre = ifelse(var_pre > 0, 1, 0))
  }
  
  
  # Subset to a given set of risk patients
  if(group1 == 'Control'){
    dta_reg = dta_reg %>% filter(class_code %in% risk1)
  }
  
  
  # print(tapply(dta_reg$var, paste( dta_reg$ecm_include_patient, dta_reg$treat_period), sf))
  
  
  ### Reference category
  dta_reg$ecm_include_patient = relevel(factor(dta_reg$ecm_include_patient), ref = group1)
  
  
  x1 = dta_reg$var[dta_reg$ecm_include_patient == 'Control'] %>% mean_miss %>% round(2)
  
  ### NOTE: Only add strata for ECM control vs treatment (and NOT for Pure control vs. treatment)
  
  if(group1 == 'Pure control'){
    m1 = lm(var ~   factor(block_categorical)  + age + gender + ecm_include_patient, data = dta_reg) # Cross-sectional
    m2 = lm(var ~   factor(block_categorical)  + age + gender + var_pre + ecm_include_patient, data = dta_reg) # ANOVA (controlling for pre-treatment outcomes)
  }else if(group1 == 'Control'){
    m1 = lm(var ~  factor(strata)  + age + gender + ecm_include_patient, data = dta_reg) # Cross-sectional
    m2 = lm(var ~  factor(strata)  + age + gender + var_pre + ecm_include_patient, data = dta_reg) # ANOVA (controlling for pre-treatment outcomes)
  }else{
    print(group1)  
    break # If different groups used, the models might need to be adjusted, so break for now in that case
  }
  nobs(m1)
  nobs(m2)
  
  ### Cluster SE
  m1 = extract_coeftest(coeftest(m1, cluster.vcov(m1, dta_reg$list_id, df_correction = T)), 0)
  m2 = extract_coeftest(coeftest(m2, cluster.vcov(m2, dta_reg$list_id, df_correction = T)), 0)
  
  ### Combine all models and specify the DV
  m_all = left_join(m1 %>% rename('beta_cross' = 'beta'),
                m2 %>% rename('beta_ancova' = 'beta'))
  
  m_all$var = var1

  print(x1)
  return(m_all)  
  

  
}

names(dta_outcomes)

var2 = 'n_diag'
risk1 = c('Mild/moderate', 'Severe')
cross_regression('count', var2, 'Control', 'Treatment', 0.999, 2018, risk1)
cross_regression('dummy', var2, 'Control', 'Treatment', 0.999, 2018, risk1)


cross_regression('dummy', var2, 'Control', 'Treatment', 0.999, 2018, c('Mild/moderate', 'Severe'))
cross_regression('dummy', var2, 'Control', 'Treatment', 0.999, 2018, c('Mild/moderate'))
cross_regression('dummy', var2, 'Control', 'Treatment', 0.999, 2018, c('Severe'))



names(dta_outcomes)
var2 = 'stroke_any'

ra = cross_regression('count', var2, 'Control', 'Treatment', 0.999, 2018, c('Mild/moderate', 'Severe'))
rb = cross_regression('count', var2, 'Control', 'Treatment', 0.999, 2018, c('Mild/moderate'))
rc = cross_regression('count', var2, 'Control', 'Treatment', 0.999, 2018, c('Severe'))

left_join(ra, left_join(rb, rc, by = 'var'), by = 'var')

names(dta_outcomes)

# tapply(dta_outcomes$copd, paste( dta_outcomes$ecm_include_patient, dta_outcomes$treat_period), function(x) quantile(x, probs=seq(0.96,1, .001)))


### NOTE: Why is it not working with var1, but only with var2 and var1=var2 inside the function?  ------------------------------------------------------------------------------------------

#
# CREATE TABLE  ---------------------------------------------------------------------------------------------------
# 




### CHOOSE: Define pre-treatment start year ---------------------------------------------------------------------------------------------------
year1 = 2018  # 2009 /2018


### CHOOSE: Define risk level ---------------------------------------------------------------------------------------------------
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 
risk1_lab = ifelse(all(risk1 == c('Mild/moderate', 'Severe')),  'all',  ifelse(all(risk1 == c('Severe')),   'severe', 'mild'))



### Define outcomes variables --------------------------------------------------------------------------------------------------
# vars = names(dta_outcomes)[which(names(dta_outcomes) == 'start_end_diff'):which(names(dta_outcomes) == 'price_primary')]
vars = names(dta_outcomes)[which(names(dta_outcomes) == 'start_end_diff'):which(names(dta_outcomes) == 'p_delay')]


### Loop across outcome variables  ---------------------------------------------------------------------------------------------------

risk1_lab = 'all'

for(year1 in c(2018)){
  for(risk1_lab in c( 'mild', 'severe')){
    
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
    
    print(paste0(var1, ' - ', year1, ' - ', risk1_lab))
    count = count + 1
    
    ### 
    # if(var1 %in% dta_table$var){next}
  
    ### Combine....
    # ... all bits as lists
    dfs1 = list(
      
      # ... A: Pure control vs. ECM control (count/N)
      
      cross_regression('count', var1, 'Pure control', 'Control', 0.999, year1, risk1) %>% 
        rename_with(.cols = -c(var), .fn = ~paste0(., '_a_n')),
      
      # ... A: Pure control vs. ECM control (dummy)
      cross_regression('dummy', var1, 'Pure control', 'Control', 0.999, year1, risk1) %>%
        rename_with(.cols = -c(var), .fn = ~paste0(., '_a_d')),
      
      # ... B: Pure control vs. ECM treatment (count/N)
      cross_regression('count', var1, 'Pure control', 'Treatment', 0.999, year1, risk1) %>% 
        rename_with(.cols = -c(var), .fn = ~paste0(., '_b_n')),
      
      # ... B: Pure control vs. ECM treatment (dummy)
      cross_regression('dummy', var1, 'Pure control', 'Treatment', 0.999, year1, risk1) %>%
        rename_with(.cols = -c(var), .fn = ~paste0(., '_b_d')),
      
      # ... C: ECM control vs. ECM treatment (count/N)
      cross_regression('count', var1, 'Control', 'Treatment', 0.999, year1, risk1) %>% 
        rename_with(.cols = -c(var), .fn = ~paste0(., '_c_n')),
      
      # ... C: ECM control vs. ECM treatment (dummy)
      cross_regression('dummy', var1, 'Control', 'Treatment', 0.999, year1, risk1) %>%
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
    
    print(dta_table)
    
    # fwrite(dta_table, file.path(project_path, 'Data', 'Clean', 'Tables', 'table2_cross_temp.csv'))
    
  }
  
  count = 0
  
  ### Save object for cleaning   ---------------------------------------------------------------------------------------------------
  dta_table_save = dta_table
  
  
  if(year1 == 2018){fwrite(dta_table_save, file.path(project_path,  'Tables','as_csv', risk1_lab, 'table2_cross.csv'))}
  if(year1 == 2009){fwrite(dta_table_save, file.path(project_path,  'Tables','as_csv', risk1_lab, 'table2_cross_09.csv'))}
  }
}


risk1 = c('Severe')  # c('Severe') / c('Mild/moderate')

 tapply(dta_outcomes$p_statins[dta_outcomes$treat_period == 1 & dta_outcomes$class_code %in% risk1],
       dta_outcomes$ecm_include_patient[dta_outcomes$treat_period == 1 & dta_outcomes$class_code %in% risk1],
       summary)



#
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#

### Re-read the table --------------------------------------------------------------------------------------------------
year1 =  2018  # 2009 / 2018
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 
risk1_lab = ifelse(all(risk1 == c('Mild/moderate', 'Severe')),  'all',  ifelse(all(risk1 == c('Severe')),   'severe', 'mild'))


if(year1 == 2018){dta_table_save = fread(file.path(project_path,  'Tables','as_csv', risk1_lab, 'table2_cross.csv'))}
if(year1 == 2009){dta_table_save = fread(file.path(project_path, 'Tables','as_csv', risk1_lab, 'table2_cross_09.csv'))}



### Clean coefficients -----------------------------------------------------------------------------------------------------

dta_table = dta_table_save


# Time differences - no dummies
dta_table[grepl('diff', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'

# No. diagnoses/procedures - no dummies
dta_table[grepl('n_diag|n_procedures', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'


# Prices - no dummies
dta_table[grepl('price', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'



# ECM consultation codes - no ANCOVA
dta_table[grepl('consult_include|consult_care_plan|consult_refuse', dta_table$var),
          which(grepl('ancova', names(dta_table)))]  = '$-$'

# Anything empty
dta_table[dta_table == ''] = '$-$'


### Extract and remove outcomes that are empty - those will mess with F-statistics --------------------------------------------------------------------------------------------------
vars_zero = dta_table$var[is.na(dta_table$beta_cross_b)]

dta_table = dta_table[!is.na(dta_table$beta_cross_c_n),]


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
  dta_table$cell1[min1] = paste0('\\multicolumn{5}{r{.45\\textwidth}}{\\textbf{', group1, '}}\\')
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


# # ... if the temporary file not complete  -----
# if(file.exists(file.path(project_path, 'Tables', 'table2_cross_temp.csv'))){
#   dta_table = fread(file.path(project_path,'as_csv', 'Tables', 'table2_cross_temp.csv'))
#   if(any(grepl(vars[length(vars)], dta_table$var))){
#     anew = T ###... stop the code to decide if we want to run the models anew
#   }
# }



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