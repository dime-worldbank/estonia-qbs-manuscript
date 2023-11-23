

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




### Checks
# tapply(dta_outcomes$id, dta_outcomes$ecm_include_patient, n_distinct)




#
# BALANCE REGRESSION FUNCTION  ---------------------------------------------------------------------------------------------------
# 




### Parameters requires  ---------------------------------------------------------------------------------------------------
type1 = 'count'  # count  / dummy - what version of the outcome should be used?
var = 'readmit_30_any' # what outcome variable should be used
treat_period1 = 0 # are we running the models for pre- or post-treatment outcomes?
group1 = 'Pure control' # what is the reference group?
group2 = 'Treatment' # what is the treatment group?
winsorize1 = 0.999 # winsorizing maximum
year1 = 2018 # Define if the pre-treatment outcomes should be measured from 2009 or 2018 (no other options for now here)
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 
risk1_lab = ifelse(all(risk1 == c('Mild/moderate', 'Severe')),  'all',  ifelse(all(risk1 == c('Severe')),   'severe', 'mild'))



balance_row = function(type1, var2, treat_period1, group1, group2, winsorize1, year1, risk1){
  
  var1 = var2
  
  
  if(treat_period1 == 0 & year1 == 2009){coef = 12/149}
  if(treat_period1 == 0 & year1 == 2018){coef = 12/41}
  if(treat_period1 == 1){coef = 12/22}
  
  # Define dataset to use
  dta_balance = dta_outcomes
  if(year1 == 2009){dta_balance = dta_outcomes_09}
  
  
  # Clean the dataset by....
  dta_balance = dta_balance %>% 
    mutate(var = !!rlang::ensym(var1)) %>% # ... iteratively defining the outcome variable
    filter(treat_period == treat_period1) %>% #... focusing on the treatment period
    mutate(var = Winsorize(as.numeric(var), minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7)) %>%  # ...winsorizing at pre-defined level
    mutate(var = as.numeric(var) * coef) %>%  # ... annualizing
    filter(ecm_include_patient %in% c(group1, group2)) # ....filtering the selected groups
  
  
  # Convert to 0/1 dummies, if this is the option selecte %>% 
  if(type1 == 'dummy'){ 
    dta_balance = dta_balance %>% mutate(var = ifelse(var > 0, 1, 0))
  }
  
  
  
  # Subset to a given set of risk patients
  if(group1 == 'Control'){
    dta_balance = dta_balance %>% filter(class_code %in% risk1)
  }
  
  
  # Re-level the grouping factors
  dta_balance$ecm_include_patient = factor(dta_balance$ecm_include_patient, levels = c(group1, group2))
  
  
  # Estimate SD, t-test, and anova (some are useful for summaries below)
  sd1 = dta_balance %>% group_by(ecm_include_patient) %>% dplyr::select(c(var)) %>% summarise_all(., sd_miss)
  t1  = t.test(dta_balance$var  ~ dta_balance$ecm_include_patient)
  # a1  = anova(lm(var  ~ ecm_include_patient, data = dta_balance)) # Old method for obtaining p-values
  
  ### Fit balance regression models
  if(group1 == 'Pure control'){
    m1 = lm(var ~  factor(block_categorical)  + age + gender + ecm_include_patient, data = dta_balance)
  }else if(group1 == 'Control'){
    m1 = lm(var ~  factor(strata) + age + gender + ecm_include_patient, data = dta_balance)
  }else{
    print(group1)  
    break # If different groups used, the models might need to be adjusted, so break for now in that case
  }
  
  
  ### Cluster SE
  m1 = coeftest(m1, cluster.vcov(m1, dta_balance$list_id, df_correction = T))
  
  
  ### Extract p-value - select NON-reference row and p-value column from the coeftest object
  p1 = sig_stars(m1[which(grepl(group2, rownames(m1))), 'Pr(>|t|)']) 
  
  ### Combine all models and specify the DV
  dta_balance = data.frame(
    var = var1,
    mean1 = paste0(round_flex(t1$estimate[1]), ' (', round_flex(sd1[1,2]), ')'),
    mean2 = paste0(round_flex(t1$estimate[2]), ' (', round_flex(sd1[2,2]), ')'),
    diff  = paste0(round_flex(t1$estimate[1] - t1$estimate[2]), ' (', round_flex(t1$stderr), ')'),
    p     = p1
  )
  
  dta_balance
  return(dta_balance)  
  
}

### Testing   ---------------------------------------------------------------------------------------------------
names(dta_outcomes)

var2 = 'myocardial_infarction_any'
var2 = 'heart_failure_any'
year1 = 2018
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 

balance_row('dummy',var2, 0, 'Control', 'Treatment', 0.999, year1, c('Mild/moderate', 'Severe'))

balance_row('count',var2, 0, 'Control', 'Treatment', 0.999, year1, c('Mild/moderate'))
balance_row('count',var2, 0, 'Control', 'Treatment', 0.999, year1, c('Severe'))

balance_row('count',var2, 0, 'Pure control', 'Control', 0.999, year1)
balance_row('count',var2, 0, 'Pure control', 'Treatment', 0.999, year1)


### NOTE: Why is it not working with var1, but only with var2 and var1=var2 inside the function?   ---------------------------------------------------------------------------------------------------


#
# CREATE TABLE  ---------------------------------------------------------------------------------------------------
# 


### CHOOSE: Define treatment period  ---------------------------------------------------------------------------------------------------
treat_period1 = 0

### CHOOSE: Define pre-treatment start year ---------------------------------------------------------------------------------------------------
year1 = 2018

### CHOOSE: Define risk level ---------------------------------------------------------------------------------------------------
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 
risk1_lab = ifelse(all(risk1 == c('Mild/moderate', 'Severe')),  'all',  ifelse(all(risk1 == c('Severe')),   'severe', 'mild'))


### Define outcomes variables --------------------------------------------------------------------------------------------------
vars = names(dta_outcomes)[which(names(dta_outcomes) == 'start_end_diff'):which(names(dta_outcomes) == 'p_delay')]


risk1_lab = 'all'

for(year1 in c(2018, 2009)){
  for(risk1_lab in c('all', 'mild', 'severe')){
    
    if(risk1_lab == 'all'){
      risk1 = c('Mild/moderate', 'Severe')
    }else if(risk1_lab == 'mild'){
      risk1 = c('Mild/moderate')
    }else if(risk1_lab == 'mild'){
      risk1 = c('Severe')
    }
    
    
  
    ### Loop across outcome variables  ---------------------------------------------------------------------------------------------------
    
    if("tidylog" %in% (.packages())){detach("package:tidylog", unload=TRUE)} # Otherwise it prints way too many messages from the loop below
    
    count = 0
    
    for(var1 in vars){
      
      print(paste0(var1, ' - ', year1, ' - ', risk1_lab))
      count = count + 1
      
      
      ### Combine....
      # ... all bits as lists
      dfs1 = list(
        
        # ... A: Pure control vs. ECM control (count/N)
        
        balance_row('count', var1, treat_period1,  'Pure control', 'Control', 0.999, year1, risk1) %>% 
          rename_with(.cols = -c(var), .fn = ~paste0(., '_a_n')),
        
        ### NOTE: Dummy version suppressed for now pending the decision on how to show annualized % values
        
        
        # ... A: Pure control vs. ECM control (dummy)
        # balance_row('dummy', var1, treat_period1,  'Pure control', 'Control', 0.999) %>%
        #   rename_with(.cols = -c(var), .fn = ~paste0(., '_a_d')),
        
        # ... B: Pure control vs. ECM treatment (count/N)
        balance_row('count', var1, treat_period1,  'Pure control', 'Treatment', 0.999, year1, risk1) %>% 
          rename_with(.cols = -c(var), .fn = ~paste0(., '_b_n')),
        
        # ... B: Pure control vs. ECM treatment (dummy)
        # balance_row('dummy', var1, treat_period1,  'Pure control', 'Treatment', 0.999) %>%
        #   rename_with(.cols = -c(var), .fn = ~paste0(., '_b_d')),
        
        # ... C: ECM control vs. ECM treatment (count/N)
        balance_row('count', var1, treat_period1,  'Control', 'Treatment', 0.999, year1, risk1) %>% 
          rename_with(.cols = -c(var), .fn = ~paste0(., '_c_n'))
        
        # ... C: ECM control vs. ECM treatment (dummy)
        # balance_row('dummy', var1, treat_period1,  'Control', 'Treatment', 0.999) %>%
        #   rename_with(.cols = -c(var), .fn = ~paste0(., '_c_d'))
      )
      
      # ... merge together
      
      
      library(plyr) # NOTE: 'plyr' messses with a lot of other functions, so we detach it right away
      temp = join_all( dfs = dfs1, by = 'var', type = 'left')
      detach("package:plyr", unload=TRUE)
      
      print(temp)
      
      if(count == 1){
        dta_table = temp
      }else{
        dta_table = rbind(dta_table, temp)
      }
    }
    
    
    ### Save object for cleaning   ---------------------------------------------------------------------------------------------------
    dta_table_save = dta_table
    
    
    if(treat_period1 == 0 & year1 == 2018){fwrite(dta_table_save, file.path(project_path,'as_csv', 'Tables', risk1_lab, 'table1_balance_patient_pre.csv'))}
    if(treat_period1 == 0 & year1 == 2009){fwrite(dta_table_save, file.path(project_path,'as_csv', 'Tables', risk1_lab, 'table1_balance_patient_pre_09.csv'))}
    if(treat_period1 == 1 & year1 == 2018){fwrite(dta_table_save, file.path(project_path, 'as_csv','Tables', risk1_lab, 'table1_balance_patient_post.csv'))}
    if(treat_period1 == 1 & year1 == 2009){fwrite(dta_table_save, file.path(project_path, 'as_csv','Tables', risk1_lab, 'table1_balance_patient_post_09.csv'))}
    
    
    
  }
}






#
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#

### CONFIRM CHOICES ----------------------------------------------------------------------------------------------------
treat_period1 = 0
year1 =  2018
risk1 = c('Mild/moderate', 'Severe') # Risk group to focus on -> c('Severe') / c('Mild/moderate') / c('Mild/moderate', 'Severe') [everyone] 
risk1_lab = ifelse(all(risk1 == c('Mild/moderate', 'Severe')),  'all',  ifelse(all(risk1 == c('Severe')),   'severe', 'mild'))

### Re-read the table --------------------------------------------------------------------------------------------------
if(treat_period1 == 0 & year1 == 2018){dta_table_save = fread(file.path(project_path,'as_csv', 'Tables', risk1_lab, 'table1_balance_patient_pre.csv'))}
if(treat_period1 == 0 & year1 == 2009){dta_table_save = fread(file.path(project_path,'as_csv', 'Tables', risk1_lab, 'table1_balance_patient_pre_09.csv'))}
if(treat_period1 == 1 & year1 == 2018){dta_table_save = fread(file.path(project_path,'as_csv', 'Tables', risk1_lab,'table1_balance_patient_post.csv'))}
if(treat_period1 == 1 & year1 == 2009){dta_table_save = fread(file.path(project_path,'as_csv', 'Tables', risk1_lab,'table1_balance_patient_post_09.csv'))}


### Extract and remove outcomes that are empty - those will mess with F-statistics --------------------------------------------------------------------------------------------------
vars_zero = dta_table_save$var[is.na(dta_table_save$p_c_n)]

dta_table = dta_table_save[!is.na(dta_table_save$p_c_n),]

names(dta_outcomes)

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
  dta_table$cell1[min1] = paste0('\\multicolumn{13}{r{.99\\textwidth}}{\\textbf{', group1, '}}\\')
}


### Replace $-$ with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)


### Print to copy to Overleaf  ---------------------------------------------------------------------------------------------------
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}


### Print definitions (optional, for those notes) -------------------------------------------------------------------------
### can also ask ChatGPT nicely and it should do it
dict_outcomes = dict_outcomes[order(dict_outcomes$order),]
temp = dict_outcomes[dict_outcomes$var %in% vars, c('name', 'description')]

for(i in 1:nrow(temp)){
  cat(paste0('\\textbf{',temp$name[i], '} - ', temp$description[i]),', ')
}


### F-statistics joint significance ----

# vars_zero2 = c(vars_zero, vars[grepl('consult', vars)])
# 
# temp = dta_outcomes %>% filter(ecm_include_patient %in% c('Pure control', 'Treatment')) %>% 
#   mutate(across(vars, ~Winsorize(., minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7))) %>%  # Winsorize at pre-defined level
#   mutate(across(vars, ~as.numeric(.) * coef)) # Annualize
# 
# temp = manova(do.call(cbind, temp[vars[-which(vars %in% vars_zero)]]) ~ temp$ecm_include_patient) %>% summary()
# 
# 
# temp2 = dta_outcomes %>% filter(ecm_include_patient %in% c('Control', 'Treatment')) %>% 
#   mutate(across(vars, ~Winsorize(., minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7))) %>%  # Winsorize at pre-defined level
#   mutate(across(vars, ~as.numeric(.) * coef)) # Annualize
# 
# temp2 = manova(do.call(cbind, temp2[vars[-which(vars %in% vars_zero)]]) ~ temp2$ecm_include_patient) %>% summary()
# 
# # Print to LaTeX
# f_row = c('\\textbf{F-test}','', '', '', sig_stars(temp$stats[1, 'Pr(>F)']),
#           '', '', '', sig_stars(temp2$stats[1, 'Pr(>F)']),
#           '\\textbf{Sample size (N)}',
#           sum(dta_outcomes$ecm_include_patient[dta_outcomes$treat_period == treat_period1] == 'Pure control'), 
#           sum(dta_outcomes$ecm_include_patient[dta_outcomes$treat_period == treat_period1] == 'Treatment'), '','',
#           sum(dta_outcomes$ecm_include_patient[dta_outcomes$treat_period == treat_period1] == 'Control'), 
#           sum(dta_outcomes$ecm_include_patient[dta_outcomes$treat_period == treat_period1] == 'Treatment'), '','')
# 
# cat(paste0(paste0(f_row, collapse = '&'), '\\'))



#
# SCRAPBOOK -----------------------------------------------------------------------------
# 
### Extract pages from the draft report -----------------------------------------------------------

# input_pdf <- file.path(project_path, 'Output', 'ECM_Evaluation_Paper_30102023.pdf')
# output_pdf <- file.path(project_path, 'Output', 'ECM_Evaluation_Paper_30102023 (selected tables).pdf')
# 
# pages_to_keep = c(10:17, 24:28)
# 
# pdf_subset(input_pdf, output_pdf, pages = pages_to_keep)



#
# END OF CODE ----
#