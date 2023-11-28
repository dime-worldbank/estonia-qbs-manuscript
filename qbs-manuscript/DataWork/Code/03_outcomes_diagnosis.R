

#
# SET-UP ---------------------------------------------------------------------------------------
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



### Make copy of the file
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)



### NEW VARIABLES? ---------------------------------------------------------------------------------------

# -> (DONE) total no. of bills
# -> (DONE DIAGNOSIS) no. of diagnosed conditions / procedures (average / total?)
# -> (DONE) admission type - referral by family doctor (E-T0011?)
# -> (DONE) length of hospitalization (average + total)
# -> (DONE) obesity (some dynamics, e.g. end of code for obesity = patient lost weight?)


### -> 30-day survival https://www.cms.gov/medicare/quality/initiatives/hospital-quality-initiative/outcome-and-payment-measures
### -> For exact ICD codes used in the field see 'Literature/ICD codes of acute conditions.pdf'
### -> But those are ICD-9... to convert to ICD-10, either 
# find an updated report (all webpages seem unaccessible)
# a conversion .csv - downloadable doesn't contain dots, so hard to match (+ imperfect matching betwen ICD-9 and ICD-10 anyway)
# use online tool to convert, eg: https://www.aapc.com/icd-10/codes/ (slow + imperfect)

##  Outcomes: 
# (DONE) AMI: Acute Myocardial Infarction
# (DONE) COPD: Coronary Obstructive Pulmonary Diseases,
# (DONE) HF: Heart Failure 
# (DONE) PN: Pneumonia
# (DONE) Stroke


## Measures: 
# 30-day risk-standardized mortality measures
# (DONE) 30-day risk-standardized readmission measures 
# 90-day risk-standardized complications measure
# 30-day excess days in acute care measures	



### -> Run negative binomial?


### NOTE: To check the mismatched number of outpatient and primary bills between 2009-2019 and 2019-2023 (see below) ---------
### 'ECM list of ID vars.xlsx' on DropBox

### For diagnosis codes check: http://icd9.chrisendres.com/ OR https://www.icd10data.com/search?s=T51.0
# diagnosis = fread(file.path(project_path, 'Data/Raw', 'Diagnosis_code_desc.csv'))


start1 = Sys.time() # To control the runtime

### Diagnosis data ---------------------------------------------------------------------------------------
dta_diagnosis = read_parquet(file.path(project_path, 'Data', 'Clean', 'Diagnoses_all.parquet')) %>% mutate(patientidencrypted  = as.character(patientidencrypted ))



# dim(dta_diagnosis) # ~ 33 million x 12 columns
# n_distinct(dta_diagnosis$billnr) # ~ 19.2 million
# n_distinct(dta_diagnosis$patientidencrypted) # ~ 86,492
# summary(dta_diagnosis$startoftreatment) # 2009-2023


### For diagnosis codes check: http://icd9.chrisendres.com/ OR https://www.icd10data.com/search?s=T51.0
diagnosis_codes = fread(file.path(project_path, 'Data', 'Clean', 'Other', 'Diagnosis_code_desc.csv'))
severe_codes = xlsx::read.xlsx(file.path(project_path, 'Data', 'Clean', 'Other', 'ICD codes of acute conditions.xlsx'), sheetIndex = 1)


# paste(unique(severe_codes$idc10[severe_codes$variable == 'Acute Myocardial Infarction (AMI)']), collapse = ', ')
# paste(unique(severe_codes$idc10[severe_codes$variable == 'Heart failure']), collapse = ', ')
# paste(unique(severe_codes$idc10[severe_codes$variable == 'Pneumonia']), collapse = ', ')
# paste(unique(severe_codes$idc10[severe_codes$variable == 'Chronic Obstructive Pulmonary Disease (COPD)']), collapse = ', ')
# paste(unique(severe_codes$idc10[severe_codes$variable == 'Stroke']), collapse = ', ')




### NOTE: LEAVE ONLY PURE CONTROL, CONTROL AND TREATMENT PATIENTS (FOR NOW) ----
patient_ecm_eligible = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible.csv')) %>% mutate(id = as.character(id))
patient_ecm_eligible_demo = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible_demo.csv')) %>% mutate(id = as.character(id))

dta_diagnosis = dta_diagnosis %>% filter(patientidencrypted %in% patient_ecm_eligible$id)


# dim(dta_diagnosis) # ~ 19.7 million x 15 columns
# n_distinct(dta_diagnosis$billnr) # ~ 10.5 million



#
# TYPE + NUMBER OF DIAGNOSIS ---------------------------------------------------------------------------------------
#

### -> Check if 'typeofdiagnos' can be used for anything  -> 
###  whether primary (Põhidiagnoos) or secondary diagnosis (Kaasuv) with + (primary ilness); - (recurrrent disease); 0 (diagnosis unconfirmed) 
### -> What is 'numberofdiagnosis'? -> severity? matches last part of typeofdiagnos
#### (e.g. all typeofdiagnos = 'Kaasuv diagnoos 2+' has 21 on 'numberofdiagnosis' -> do a table to check) -> 
#### but some typeofdiagnos do not have a number associated with them

### see: § 57 https://www.riigiteataja.ee/akt/125112011004


table(dta_diagnosis$typeofdiagnos)
table(dta_diagnosis$numberofdiagnosis)
table(dta_diagnosis$typeofdiagnos, dta_diagnosis$numberofdiagnosis)


# typeofdiagnos
# no missings ->  table(is.na(dta_diagnosis$typeofdiagnos))
# table(grepl('Kaasuv', dta_diagnosis$typeofdiagnos)) # 53% secondary (Kassuv), 47% primary
# pr(grepl('\\d', dta_diagnosis$typeofdiagnos)) # 68% of typeofdiagnos don't have severity in the name itself, so need to use numberofdiagnosis


# numberofdiagnosis
pr(substr(dta_diagnosis$numberofdiagnosis, 1,1)) # 27% = 0   |   40% = 1   |  17% = 2

### XXX ----------------

#
# DATE OF DEATH ---------------------------------------------------------------------------------------
#

### Quick check if mortality varies between ECM treatment groups -----------------------------------------------------
# dead_id = dta_diagnosis %>% filter(dateofdeath != 0)
# dead_id = dead_id$patientidencrypted %>% unique()
# 
# temp = patient_ecm_eligible %>% mutate(deceased = ifelse(id %in% dead_id, 1, 0))
# tapply(temp$deceased, temp$ecm_include_patient, pr)
# t.test(temp$deceased[temp$ecm_include_patient %in% c('Control', 'Treatment')] ~
#          temp$ecm_include_patient[temp$ecm_include_patient %in% c('Control', 'Treatment')])
# 

# Discard all data before May 2021 (min. death date in the date) -> all ECM patients should be alive until then 
dta_deaths = dta_diagnosis %>% filter(startoftreatment >= min(dta_diagnosis$dateofdeath[dta_diagnosis$dateofdeath!=0]))


### Check if date of death unique (max of 2 unique values - 0 + actual date of death)
# summary(dta_deaths$dateofdeath[dta_deaths$dateofdeath != 0])
# dta_deaths = dta_deaths %>% group_by(patientidencrypted) %>% mutate(N=n_distinct(dateofdeath))
# pr(dta_deaths$N)
# dta_deaths = dta_deaths %>% group_by(patientidencrypted) %>% filter(dateofdeath!=0) %>% mutate(N=n_distinct(dateofdeath))
# pr(dta_deaths$N)
# R: Yes


### Assign death date to a patient (rather than individual bill). This can be done in 2 WAYS:
dta_deaths = dta_deaths %>%
  group_by(patientidencrypted) %>%
  mutate(dateofdeath = max(dateofdeath)) %>%  # ... dateofdeath column - max value
  mutate(deathofdeath_discharge = (endoftreatment), # ... but patients can also be discharged as 'deceased' (codeofdischargetype == 10) - classify those as alternative death dates
         deathofdeath_discharge = ifelse(codeofdischargetype == 10, deathofdeath_discharge, 0),
         deathofdeath_discharge = max(deathofdeath_discharge),
         startoftreatment_max = max(startoftreatment) # ... also, check the latest treatment start day per patient
         )


### Select all patients that are deceased on EITHER of our two measures ----------------------------------------
dta_deaths = dta_deaths  %>%  ungroup() %>% 
          filter(dateofdeath != 0 | deathofdeath_discharge != 0)%>% 
          dplyr::select(c(patientidencrypted, dateofdeath, deathofdeath_discharge, startoftreatment_max)) %>% 
          distinct()

temp_include = left_join(dta_deaths %>% rename('id'='patientidencrypted'), patient_ecm_eligible_demo)

sf(dta_deaths$dateofdeath == 0)
sf(dta_deaths$startoftreatment_max == 0)
table(dta_deaths$dateofdeath != 0, dta_deaths$dateofdeath != 0)

### Now, there are couple of options, as the death dates are not consistent: -------------------------------------

# 1) dateofdeath non-missing +  no later treatment =  deathofdeath stays as final -> IDEAL SCENARIO (~85% of observation)
temp1 = dta_deaths  %>% 
          filter(dateofdeath != 0)  %>% 
          filter(startoftreatment_max <= dateofdeath)

temp1$dateofdeath_final = temp1$dateofdeath


# 2) dateofdeath non-missing + some later treatment + later discharge as 10 (deceased)  = deathofdeath_discharge stays as final is latest treatment <= 7 days afterwards; 0 otherwise
temp2 = dta_deaths  %>% 
          filter(dateofdeath != 0)  %>%
          filter(startoftreatment_max > dateofdeath) %>% 
          filter(deathofdeath_discharge != 0) %>% 
          mutate(diff_start = as.numeric(ymd(startoftreatment_max) - ymd(deathofdeath_discharge)))


temp2$dateofdeath_final = ifelse(temp2$diff_start > 7, 0, temp2$deathofdeath_discharge)


# 3) dateofdeath non-missing + some later treatment + NO later discharge as 10 (deceased) = dateofdeath stays as final is latest treatment <= 7 days afterwards; 0 otherwise
temp3 = dta_deaths  %>% 
  filter(dateofdeath != 0)  %>%
  filter(startoftreatment_max > dateofdeath) %>% 
  filter(deathofdeath_discharge == 0) %>% 
  mutate(diff_start = as.numeric(ymd(startoftreatment_max) - ymd(dateofdeath)))

temp3$dateofdeath_final = ifelse(temp3$diff_start > 7, 0, temp3$dateofdeath)


# 4) dateofdeath missing + discharged as 10 (deceased)  = deathofdeath_discharge stays as final is latest treatment <= 7 days afterwards; 0 otherwise
temp4 = dta_deaths  %>% 
  filter(dateofdeath == 0)  %>%
  filter(deathofdeath_discharge != 0)%>% 
  mutate(diff_start = as.numeric(ymd(startoftreatment_max) - ymd(deathofdeath_discharge)))

temp4$dateofdeath_final = ifelse(temp4$diff_start > 7, 0, temp4$deathofdeath_discharge)


### Checks
# (nrow(temp1) + nrow(temp2) + nrow(temp3) + nrow(temp4)) ==  nrow(dta_deaths)
### R: Should give T
# temp = rbindlist(list(temp1, temp2, temp3, temp4), fill = T)
# temp$diff = as.numeric(ymd(temp$startoftreatment_max) - ymd(temp$dateofdeath_final))
# summary(temp$diff[temp$dateofdeath_final != 0])
### R: Should not be >7

### Check if sum of rows of all possible scenarios equals total number of rows in the dataset with the deceased patiets, and reasign if so
if((nrow(temp1) + nrow(temp2) + nrow(temp3) + nrow(temp4)) ==  nrow(dta_deaths)){
  dta_deaths = rbindlist(list(temp1, temp2, temp3, temp4), fill = T) %>% 
      dplyr::select(c(patientidencrypted, dateofdeath_final)) %>% 
      rename('dateofdeath' = 'dateofdeath_final')
      
}



### Re-assign to dta_diagnosis (check if all patient ID's unique) ---------------------------------------
nrow(dta_deaths) == n_distinct(dta_deaths$patientidencrypted)

dta_diagnosis = left_join(dta_diagnosis %>% dplyr::select(-c(dateofdeath)), dta_deaths) %>% 
                    mutate(dateofdeath = ifelse(is.na(dateofdeath), 0, dateofdeath),
                           deceased    = ifelse(dateofdeath == 0, 0, 1))

# Save temporary version to avoid re-runnig whole script
write_parquet(dta_diagnosis, file.path(project_path, 'Data', 'dta_diagnosis_temp.parquet'))


### Check if now all bills start no later than 7 days after the date of death
# dta_diagnosis$diff = as.numeric(ymd(dta_diagnosis$startoftreatment) - ymd(dta_diagnosis$dateofdeath))
# dta_diagnosis$diff %>% summary()
### R: Yes, max value is 7


### Check overall mortality rate
# temp = dta_diagnosis %>% dplyr::select(c(patientidencrypted, deceased)) %>% distinct()
# sf(temp$deceased) # 6,845 out of 87,062 unique patients
# pr(temp$deceased) # 7.86%



### Add to patient_ecm_eligible ----------------------------------------------
dta_deaths_save = dta_deaths

dta_deaths = left_join(patient_ecm_eligible_demo,
                       dta_deaths_save %>%  rename('id' = 'patientidencrypted')) %>% 
      mutate(  # ... clear dates
      dateofdeath = ymd(dateofdeath), #... convert to date format
      dateofdeath = if_else(dateofdeath > ymd(20230331), NA, dateofdeath), # ... NOTE: if death date AFTER the end of the intervention evaluation period (31/03/2023, treat patient as alive)
      death_year = year(dateofdeath), # ... specify year
      death_year_month = ifelse(!is.na(death_year), paste0( year(dateofdeath), '-',  month(dateofdeath)), NA), # ... specify year and month
      death_month = month(dateofdeath), # ... specify just the month
      death_year_rel = time_length(difftime(dateofdeath, as.Date("2021-06-01")), "years") %>% sign() * # ... specify how many years before or after ECM onset
                      time_length(difftime(dateofdeath, as.Date("2021-06-01")), "years") %>% abs() %>% ceiling(),
      death_year_rel = ifelse(death_year_rel == 0, 1, death_year_rel), # ... change 0 to 1 in year_rel (observations on the day of ECM onset)
      death_treat_period = ifelse(dateofdeath < ymd(20210601), 0, 1) # ... specify treatment period
    )


sf(dta_deaths$ecm_include_patient)


### Check date range
# summary(dta_deaths$dateofdeath)
# sf(dta_deaths$death_treat_period)
# tapply(dta_deaths$dateofdeath,     dta_deaths$death_treat_period, summary)
# tapply(dta_deaths$death_year_rel, dta_deaths$death_treat_period, summary)
# R: All within the range of January 2018 - March 2023 and year relative to the ECM onset (stricly negative before and strictly positive after)


### Save -----
write_parquet(dta_deaths, file.path(project_path, 'Data/Clean', 'Deaths_all.parquet'))




### Update IDs to keep only patients alive at the start of the intervention -------------------------------------------------------------
dta_deaths = read_parquet(file.path(project_path, 'Data/Clean', 'Deaths_all.parquet'))

id_deaths = unique(dta_deaths$id[dta_deaths$death_treat_period == 0])

files1 = list.files(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion'), pattern = '^id_|patient_ecm_eligible')
file1 = 'id_treatment.csv'

for(file1 in files1){
  if(file1 %in% c('id_codes.csv')){next}
  
  fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', paste0(file1))) %>% 
    filter(!(id %in% id_deaths)) %>% 
    fwrite(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', paste0(file1)), na=NA, row.names = F)
}


### Filter death dates outside the treatment period
# dta_deaths %>%
#   filter(dateofdeath >= ymd(20210601) | is.na(dateofdeath)) %>%
#   filter(dateofdeath <= ymd(20230331) | is.na(dateofdeath))



### Distributions control v. treatment
# patient_ecm  =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_ecm.csv")) %>% mutate(id = as.character(id))
# dta_deaths = left_join(dta_deaths, patient_ecm %>% dplyr::select(c(id, ecm_include_patient)))
# tapply(dta_deaths$dateofdeath %>% is.na(), dta_deaths$ecm_include_patient, pr) # 'Survival rate': 96.4% control patients and 97.09% treatment patients
# tapply(dta_deaths$dateofdeath, dta_deaths$ecm_include_patient, summary) # Roughly similar distributions for treatment and control


# patient_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible.csv")) %>% mutate(id = as.character(id))
# patient_eligible = patient_eligible %>% filter(eligible_code %in% c('', 'JVP92'))
# 
# dta_deaths = left_join(patient_eligible, dta_deaths)
# tapply(dta_deaths$dateofdeath %>% is.na(), dta_deaths$eligible_patient, pr) # 'Survival rate': 96.4% control patients and 97.09% treatment patients
# tapply(dta_deaths$dateofdeath, dta_deaths$eligible_patient, summary) # Roughly similar distributions for treatment and control



### Plot density
# g1=ggplot(dta_deaths,
#           aes(x = dateofdeath,
#               color = ecm_include_patient, group = ecm_include_patient)) +
#   #geom_vline(aes(xintercept = ymd(20200315)), linewidth = 1.6, linetype= 'dashed', color = 'red')
#   geom_density()
#   
# ggsave(file.path(project_path, 'Figures', 'Deaths', paste0('Death dates - density (post-treatment, grouped)', ".png")),
#        plot=g1, width = 60, height = 47, units = 'cm')




# 
# CREATE OUTCOMES -------------------------------------------------------------------------------------------------
#






### Remove unnecessary columns
### NOTE: Also remove 'billnr' column. Now, this results in ~2% fewer rows 
### after collapsing by diagnosis code below, BUT we do ensure we have
### the same patient, on the same day, with the same dataset and admission type,
### so by all accounts we should treat it as a single healthcare interaction
### even if there are sometimes >1 bill numbers. 

dta_diagnosis = read_parquet(file.path(project_path, 'Data', 'dta_diagnosis_temp.parquet'))



dta_diagnosis = dta_diagnosis %>% ungroup() %>%
  dplyr::select(c(patientidencrypted, dateofdeath, deceased, dataset, startoftreatment, endoftreatment, codeofdiagnos, codeofadmissiontypeoradmitt))


### Collapse diagnosis codes first ---------------------------------------------------------------------------------
dta_diagnosis <- dta_diagnosis %>%
  group_by(across(setdiff(names(dta_diagnosis), c('codeofdiagnos')))) %>%
  summarize(codeofdiagnos = paste(codeofdiagnos, collapse = ", ")) %>% ungroup()




# 4437311 - with start, but no end and no billnr
# 4464733 - with start and end, but no billnr
# 4519969 - with all start, end, and billnr

# temp = dta_diagnosis %>% 
#   group_by(across(setdiff(names(dta_diagnosis), c('endoftreatment', 'codeofdiagnos')))) %>% 
#   mutate(N=n())
# 
# dim(temp)
# sf(temp$N)
# temp %>% filter(N>1) %>% View()


### Checks
# dta_diagnosis$startoftreatment %>% summary()
# class(dta_diagnosis$codeofdiagnos)
# R: Dates agree; code of diagnoses is of class character


### Check for outliers

### Find how many observations per patient and dataset
# temp2 = dta_diagnosis %>% 
#   group_by(patientidencrypted, dataset) %>%
#   mutate(N=n()) %>% ungroup() %>% 
#   group_by(dataset) %>% 
#   mutate(quantile99 = quantile(N, probs = .99))
# 
# ### Summarize all...
# dim(temp2)
# summary(temp2$N)
# 
# ### ... and by dataset
# tapply(temp2$N, temp2$dataset, summary)
# tapply(temp2$N, temp2$dataset, function(x) quantile(x, probs = seq(0.99,1,.001)))
# tapply(temp2$N, temp2$dataset, function(x) pr(quantile(x, probs = .99) < x))
# 
# ### Plot (remove filtering to see how much more skewed is the full distribution)
# ggplot(temp2 %>% filter(N < quantile99),
#        aes(x = N))+
#   geom_histogram()+
#   facet_wrap(~dataset, scales = 'free')+
#   theme_bw()
# 
# ggsave(file.path(project_path, 'Figures', 'Checks', 'Bills per patient by dataset (99th).png'),
#        width = 30, height = 30, unit = 'cm')





start1 = Sys.time()


### Identify relevant diagnosis codes etc. -------------------------------------------------------------------------------------

table(grepl('I50',dta_diagnosis$codeofdiagnos))
table(grepl('^I50',dta_diagnosis$codeofdiagnos))

dta_diagnosis = dta_diagnosis %>% 
  mutate(
    
    # Type of visit
    n_all = T,
    n_inpatient = (dataset == 'inpatient'),
    n_outpatient = (dataset == 'outpatient'),
    n_primary = (dataset == 'primaryhealth'),
    n_daycare = (dataset == 'day'),
    n_inpatient_post = (dataset %in% c('inpatientnursing', 'inpatientrehabilitation')),
    n_outpatient_post = (dataset %in% c('outpatientnursing', 'outpatientrehabilitation')),
    n_diag = 1 + str_count(codeofdiagnos, ','),
    
    # Severe (hospitaliztion)
    myocardial_infarction = (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Acute Myocardial Infarction (AMI)']), collapse='|'), codeofdiagnos) & dataset == 'inpatient'),
    stroke = (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Stroke']), collapse='|'), codeofdiagnos) & dataset == 'inpatient'),
    copd   =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Chronic Obstructive Pulmonary Disease (COPD)']), collapse='|'), codeofdiagnos) & dataset == 'inpatient'),
    heart_failure  =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Heart failure']), collapse='|'), codeofdiagnos) & dataset == 'inpatient'),
    pneumonia    =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Pneumonia']), collapse='|'), codeofdiagnos) & dataset == 'inpatient'),
    
    n_sever_diag    =  (myocardial_infarction | stroke | copd | heart_failure | pneumonia),
    
    # Severe (any)
    myocardial_infarction_any = (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Acute Myocardial Infarction (AMI)']), collapse='|'), codeofdiagnos)),
    stroke_any = (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Stroke']), collapse='|'), codeofdiagnos)),
    copd_any   =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Chronic Obstructive Pulmonary Disease (COPD)']), collapse='|'), codeofdiagnos)),
    heart_failure_any  =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Heart failure']), collapse='|'), codeofdiagnos)),
    pneumonia_any    =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Pneumonia']), collapse='|'), codeofdiagnos)),
    
    n_sever_diag_any    =  (myocardial_infarction_any | stroke_any | copd_any | heart_failure_any | pneumonia_any),

    asthma_any      =  (grepl('J45', codeofdiagnos) & dataset == 'inpatient'),
    diabetes_2_any  =  (grepl('E11.0|E11.1|E11.2|E11.3|E11.4|E11.5|E11.6|E11.7|E11.8|E11.9', codeofdiagnos)  & dataset == 'inpatient'),
    hypertension_any    =  (grepl('I10|I11|I12|I13|I15', codeofdiagnos) & dataset == 'inpatient'),
    
        
    # Additional original 'avoidable' hospitalizations
    asthma      =  (grepl('J45', codeofdiagnos) & dataset == 'inpatient'),
    diabetes_2  =  (grepl('E11.0|E11.1|E11.2|E11.3|E11.4|E11.5|E11.6|E11.7|E11.8|E11.9', codeofdiagnos)  & dataset == 'inpatient'),
    hypertension    =  (grepl('I10|I11|I12|I13|I15', codeofdiagnos) & dataset == 'inpatient'),
    
    n_inpatient_avoid    =  (asthma | diabetes_2 | copd | hypertension | heart_failure), # Works - checked manually
    
    
    admit_ambulance    =  (grepl('E-T0001', codeofadmissiontypeoradmitt) & dataset == 'inpatient'), # see explanation of different admission codes here: https://www.riigiteataja.ee/akt/125112011004
    admit_referral    =  (grepl('E-T0011', codeofadmissiontypeoradmitt) & dataset == 'inpatient'), # see explanation of different admission codes here: https://www.riigiteataja.ee/akt/125112011004
    
    
    
    alcohol_abuse  =  (grepl('F10|Z71.4', codeofdiagnos)),
    arthritis  =  (grepl('M05|M06|M15|M16|M17|M18|M19', codeofdiagnos)),
    atrial_fibrillation  =  (grepl('I48', codeofdiagnos)),
    cancer  =  (grepl('C18|C34|C50|C61', codeofdiagnos)),
    chronic_kidney_disease  =  (grepl('N18', codeofdiagnos)),
    depression  =  (grepl('F32', codeofdiagnos)),
    substance_use  =  (grepl('F11|F12|F13|F14|F15|F16|F17|F18|F19', codeofdiagnos)),
    hepatitis_b_c  =  (grepl('B16|B17', codeofdiagnos)),
    hyperlipidemia  =  (grepl('E78', codeofdiagnos)),
    hypothyroidism = (grepl('E01|E02|E03|E89.0', codeofdiagnos)),
    
    
    # hypertensive_heart  =  (grepl('I11', codeofdiagnos)),
    # ischemic_heart_disease  =  (grepl('I21|I22|I23|I24|I25', codeofdiagnos)),
    osteoporosis  =  (grepl('M80|M81', codeofdiagnos)),
    covid_incidence  =  (grepl('U07.1', codeofdiagnos)),
    
    weight_high = (grepl('E66|R63.5', codeofdiagnos)),
    weight_low = (grepl('R63.4|R63.6|^T75.82|^X52', codeofdiagnos))
    
    )
  # group_by(patientidencrypted, startoftreatment_month) %>%
  # summarise_all(., sum)


### Checks
start1-Sys.time()

# View(dta_diagnosis %>% filter(weight_low | weight_high) %>% dplyr::select(c(patientidencrypted, codeofdiagnos, weight_low,weight_high)))
# View(dta_diagnosis %>% filter(stroke)%>% dplyr::select(c(patientidencrypted, codeofdiagnos,stroke)))
# View(dta_diagnosis %>% filter(n_inpatient_avoid) %>% dplyr::select(c(patientidencrypted, codeofdiagnos, n_inpatient_avoid, asthma, diabetes_2, copd, hypertension, heart_failure)))






### Time difference -------------------------------------------------------------------------------------

### Calculate difference between start date of different bills by...
dta_diagnosis = dta_diagnosis %>%
  mutate(startoftreatment = ymd(startoftreatment), # ... treating dates as dates
         endoftreatment = ymd(endoftreatment)) %>%
  group_by(patientidencrypted) %>%  # ...grouping by patient
  arrange(startoftreatment,  .by_group = TRUE) %>%  # ... arranging by date within the groups
  mutate(start_diff = as.numeric(startoftreatment - dplyr::lag(startoftreatment, default = first(startoftreatment))), # ... calculating difference with lagged date
         start_diff = ifelse(start_diff == 0, NA, start_diff)) %>% # ... changing 0 to NA's (mostly first values, otherwise they would decrease the average difference)
  ungroup() %>% relocate('start_diff', .after = 'startoftreatment') # ... relocate to be next to startoftreatment


### Repeat by dataset (primary, outpatient, inpatient etc.)
dta_diagnosis = dta_diagnosis %>%
  group_by(patientidencrypted, dataset) %>%  # ...grouping by patient
  arrange(startoftreatment,  .by_group = TRUE) %>%  # ... arranging by date within the groups
  mutate(start_diff_dataset = as.numeric(startoftreatment - dplyr::lag(startoftreatment, default = first(startoftreatment))), # ... calculating difference with lagged date
         start_diff_dataset = ifelse(start_diff_dataset == 0, NA, start_diff_dataset)) %>% # ... changing 0 to NA's (mostly first values, otherwise they would decrease the average difference)
  ungroup() %>% relocate('start_diff_dataset', .after = 'start_diff') # ... relocate to be next to startoftreatment



### Add difference between start and end of treatment
dta_diagnosis = dta_diagnosis %>% 
  add_column(.after = 'endoftreatment', 'start_end_diff' = as.numeric(dta_diagnosis$endoftreatment - dta_diagnosis$startoftreatment))

dta_diagnosis = dta_diagnosis %>% 
  add_column(.after = 'start_end_diff', 'start_end_diff_inpatient' = ifelse(dta_diagnosis$dataset == 'inpatient', dta_diagnosis$start_end_diff, NA))



### Re-admission and death after hospitalization -------------------------------------------------------------------------------------------------

dta_diagnosis = dta_diagnosis %>% ungroup() %>% 
                  mutate(
                    'readmit_30_any' = ifelse(dataset == 'inpatient' & start_diff_dataset <= 30 & !is.na(start_diff_dataset), T, F),
                    'readmit_90_any' = ifelse(dataset == 'inpatient' & start_diff_dataset <= 90 & !is.na(start_diff_dataset), T, F),
                    'readmit_30_severe' = ifelse(dataset == 'inpatient' & n_sever_diag == T & start_diff_dataset <= 30 & !is.na(start_diff_dataset), T, F),
                    'readmit_90_severe' = ifelse(dataset == 'inpatient' & n_sever_diag == T & start_diff_dataset <= 90 & !is.na(start_diff_dataset), T, F),
                    
                    # 'deceased_30_any' = ifelse(start_diff_dataset <= 30 & !is.na(start_diff_dataset), T, F),
                    
                  ) %>% 
              relocate(c('readmit_30_any', 'readmit_90_any', 'readmit_30_severe', 'readmit_90_severe'), .after = 'start_end_diff')




### Final clean -------------------------------------------------------------------------------------------------


dta_diagnosis = dta_diagnosis %>%
  ### Treatment date
  mutate(   
         year = substr(startoftreatment, 1, 4), # ... specify year
         year_month = paste0(substr(startoftreatment, 1,4), '-', substr(startoftreatment, 6,7)), # ... specify year and month
         year_month_day = ymd(startoftreatment), # ... specify year-month-day (full date)
         month = month(year_month_day), # ... specify just the month
         year_rel = time_length(difftime(year_month_day, as.Date("2021-06-01")), "years") %>% sign() * # ... specify how many years before or after ECM onset
                    time_length(difftime(year_month_day, as.Date("2021-06-01")), "years") %>% abs() %>% ceiling(),
         year_rel = ifelse(year_rel == 0, 1, year_rel), # ... change 0 to 1 in year_rel (observations on the day of ECM onset)
         treat_period = ifelse(startoftreatment < ymd(20210601), 0, 1) # ... specify treatment period
         ) %>% 
  relocate(.after = 'startoftreatment', c('treat_period','year', 'year_month','month', 'year_month_day', 'year_rel')) %>% 
  dplyr::select(-c(start_diff, start_diff_dataset, startoftreatment, endoftreatment, # remove as we won't need those columns as outcomes
                   deceased, dateofdeath, codeofdiagnos, dataset, codeofadmissiontypeoradmitt)) 


### Year_rel creation above fails to account for leap years and results in a tiny number of borderline observations, that is
### those occuring on the onset date (01/06/2021) to be classified into a wrong year. Applying mode of year_rel to all
### observations by month seems to solve the issue
dta_diagnosis = dta_diagnosis %>% group_by(year_month) %>% mutate(year_rel = Mode(year_rel)) %>% ungroup()


### Clean ID's
dta_diagnosis = dta_diagnosis %>% rename('id' = 'patientidencrypted') %>% mutate(id=as.character(id))


### Save  -------------------------------------------------------------------------------------------------
write_parquet(dta_diagnosis, file.path(project_path, 'Data/Clean', 'dta_diagnosis.parquet'))


### Check date range
# tapply(dta_diagnosis$year_month_day, dta_diagnosis$treat_period, summary)
# tapply(dta_diagnosis$year_rel, dta_diagnosis$treat_period, summary)
### R: All within the range of January 2009 - March 2023 and year relative to the ECM onset (stricly negative before and strictly positive after)









#
# GROUP... -----------------------------------------------------------------------------------------------------------------
#

# group = F
# 
# if(group){}
  

### NOTE: We can summarise using group_by() and summarise_all() [as commented out], but takes up to x20 (sic!)
### longer than fsum(), while producing EXACTLY THE SAME RESULTS (CHECKED!)

dta_diagnosis = read_parquet(file.path(project_path, 'Data/Clean', 'dta_diagnosis.parquet'))
patient_ecm_eligible = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible.csv')) %>% mutate(id = as.character(id))


# View(dta_diagnosis %>% slice(1:10^5))
# dta_diagnosis = dta_diagnosis %>% slice(1:10^5)



# ...by month -----------------------------------------------------------------------------------------------------------------
dta_diagnosis_month = fsum(dta_diagnosis %>% 
                            filter(year >= 2018) %>%  # NOTE: We don't want any pre-2018 on a monthly basis as of now
                             dplyr::select(-c(year_month_day)) %>% # remove variabls not needed here
                              group_by(id, treat_period, year, year_rel, year_month, month)) %>% # define grouping levels
                              mutate( #  if both averages and totals needed, add averages as new columns
                                start_end_diff_av = start_end_diff / n_all,
                                start_end_diff_inpatient_av = start_end_diff_inpatient / n_inpatient,
                                n_diag_av = n_diag / n_all
                              ) 
                              # dplyr::select(-c()) # Remove those variables not needed as total

# Make sure all patients are there for all months
dta_diagnosis_month = left_join(expand_grid(id = patient_ecm_eligible$id,
                            dta_diagnosis_month %>% dplyr::select(treat_period, year, year_rel, year_month, month) %>% distinct() %>% as.data.frame()) %>%
                            group_by(id) %>% distinct() %>% ungroup(),
          dta_diagnosis_month)%>% 
  mutate(across(-c(id, treat_period, year, year_rel, year_month, month), ~replace_na(.,0))) # 0's instead of NA's


# ...by year -----------------------------------------------------------------------------------------------------------------

dta_diagnosis_year  = fsum(dta_diagnosis %>%
                              dplyr::select(-c(contains('month'), year_rel, treat_period)) %>%
                              group_by(id, year)) %>% 
                              mutate( #  if both averages and totals needed, add averages as new columns
                                start_end_diff_av = start_end_diff / n_all,
                                start_end_diff_inpatient_av = start_end_diff_inpatient / n_inpatient,
                                n_diag_av = n_diag / n_all
                              ) 

# Make sure all patients are there for all years
dta_diagnosis_year = left_join(expand_grid(id = patient_ecm_eligible$id,
                                           dta_diagnosis_year %>% dplyr::select(year) %>% distinct() %>% as.data.frame()) %>%
                                              group_by(id) %>% distinct() %>% ungroup(),
                              dta_diagnosis_year)%>% 
  mutate(across(-c(id, year), ~replace_na(.,0))) # 0's instead of NA's

# ...by year_rel -----------------------------------------------------------------------------------------------------------------
dta_diagnosis_year_rel = fsum(dta_diagnosis %>%
                             dplyr::select(-c(contains('month'), year, treat_period)) %>%
                             group_by(id, year_rel)) %>% 
                            mutate( #  if both averages and totals needed, add averages as new columns
                              start_end_diff_av = start_end_diff / n_all,
                              start_end_diff_inpatient_av = start_end_diff_inpatient / n_inpatient,
                              n_diag_av = n_diag / n_all
                            ) 

# Make sure all patients are there for all years
dta_diagnosis_year_rel = left_join(expand_grid(id = patient_ecm_eligible$id,
                                               dta_diagnosis_year_rel %>% dplyr::select(year_rel) %>% distinct() %>% as.data.frame()) %>%
                                 group_by(id) %>% distinct() %>% ungroup(),
                                 dta_diagnosis_year_rel)%>% 
  mutate(across(-c(id, year_rel), ~replace_na(.,0))) # 0's instead of NA's

# ...by period (2018-2023)  -----------------------------------------------------------------------------------------------------------------
dta_diagnosis_period_18_23  = fsum(dta_diagnosis %>%
                                     filter(year >= 2018) %>%
                                     dplyr::select(-c(contains('month'), contains('year'))) %>%
                                group_by(id, treat_period)) %>% 
                                mutate( #  if both averages and totals needed, add averages as new columns
                                  start_end_diff_av = start_end_diff / n_all,
                                  start_end_diff_inpatient_av = start_end_diff_inpatient / n_inpatient,
                                  n_diag_av = n_diag / n_all
                                ) 

# Make sure all patients are there for all periods
dta_diagnosis_period_18_23 = left_join(expand_grid(id = patient_ecm_eligible$id,
                                                   dta_diagnosis_period_18_23 %>% dplyr::select(treat_period) %>% distinct() %>% as.data.frame()) %>%
                   group_by(id) %>% distinct() %>% ungroup(),
                   dta_diagnosis_period_18_23) %>% 
  mutate(across(-c(id, treat_period), ~replace_na(.,0))) # 0's instead of NA's


# ...by period (2009-2023) -----------------------------------------------------------------------------------------------------------------
dta_diagnosis_period_09_23  = fsum(dta_diagnosis %>%
                                     dplyr::select(-c(contains('month'), contains('year'))) %>%
                                     group_by(id, treat_period)) %>% 
                                      mutate( #  if both averages and totals needed, add averages as new columns
                                        start_end_diff_av = start_end_diff / n_all,
                                        start_end_diff_inpatient_av = start_end_diff_inpatient / n_inpatient,
                                        n_diag_av = n_diag / n_all
                                      ) 

# Make sure all patients are there for all periods
dta_diagnosis_period_09_23 = left_join(expand_grid(id = patient_ecm_eligible$id,
                                                   dta_diagnosis_period_09_23 %>% dplyr::select(treat_period) %>% distinct() %>% as.data.frame()) %>%
                                         group_by(id) %>% distinct() %>% ungroup(),
                                       dta_diagnosis_period_09_23) %>% 
  mutate(across(-c(id, treat_period), ~replace_na(.,0))) # 0's instead of NA's



### Checks
dim(dta_diagnosis_month)
dim(dta_diagnosis_year)
dim(dta_diagnosis_year_rel)
dim(dta_diagnosis_period_18_23)
dim(dta_diagnosis_period_09_23)

### Save ----
write_parquet(dta_diagnosis_month, file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_month_18_23.parquet')) # NOTE: On purpose, we don't need pre-2018 on a monthly basis for now
write_parquet(dta_diagnosis_year,  file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_year_09_23.parquet'))
write_parquet(dta_diagnosis_year_rel,  file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_year_rel_09_23.parquet'))
write_parquet(dta_diagnosis_period_18_23,  file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_period_18_23.parquet'))
write_parquet(dta_diagnosis_period_09_23,  file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_period_09_23.parquet'))


end1 = Sys.time() # To control the runtime
start1-end1


dta_diagnosis_year = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_year_rel_09_23.parquet'))
dta_diagnosis_period_18_23 = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_period_18_23.parquet'))
dta_diagnosis_period_09_23 = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_period_09_23.parquet'))


dim(dta_diagnosis_year)
dim(dta_diagnosis_period_18_23)/2
dim(dta_diagnosis_period_09_23)
n_distinct(dta_diagnosis_period_09_23$id)
n_distinct(dta_diagnosis_period_18_23$id)
sf(table(dta_diagnosis_period_18_23$id))
sf(table(dta_diagnosis_period_09_23$id))



1305930/87062

### Checks
# dim(dta_diagnosis_month)
# dim(dta_diagnosis_year)
# sf(dta_diagnosis_month$asthma)
# sf(dta_diagnosis_year$asthma)
# sf(dta_diagnosis_month$n_inpatient)
# sf(dta_diagnosis_year$n_inpatient)



# ' ----
# REST ----
#

### Dummy dataset ----


# n = 10^6

# ID and date columns
# temp = data.frame(id = sample(1:n, n, replace = T),
#                  month = sample(as.Date(paste0(2020, "-01-01")) + 0:11, n, replace = TRUE))

# Value column corresponding to the combined diagnosis codes cells
# temp = temp %>% mutate(value = substr(paste(sample(LETTERS, nrow(temp), replace = T), sample(LETTERS, nrow(temp), replace = T),
#             sample(LETTERS, nrow(temp), replace = T),sample(LETTERS, nrow(temp), replace = T), sep = ','), 1, sample(c(1,3,5,7), nrow(temp), replace=T)))

# Spare columns to compare run times with and without them
# temp$var1 =  sample(10^8:9*10^8, n, replace = T)
# temp$var2 =  sample(10^8:9*10^8, n, replace = T)
# temp$var3 =  sample(10^8:9*10^8, n, replace = T)
# temp$var4 =  sample(10^8:9*10^8, n, replace = T)
# temp$var5 =  sample(10^8:9*10^8, n, replace = T)
# temp$var6 =  sample(10^8:9*10^8, n, replace = T)
# temp$var7 =  sample(10^8:9*10^8, n, replace = T)


# Group
# start1=Sys.time()
# 
# temp2 = temp %>% mutate(asthma = (grepl('A', value)),
#                         broken = (grepl('B', value)),
#                         c = (grepl('C', value)),
#                         d = (grepl('D', value)),
#                         e = (grepl('E', value)),
#                         f = (grepl('F', value)),
#                         g = (grepl('G', value)),
#                         h = (grepl('H', value)),
#                         i = (grepl('I', value)),
#                         j = (grepl('J', value)),
#                         k = (grepl('K', value)),
#                         l = (grepl('L', value)),
#                         m = (grepl('M', value)),
#                         n = (grepl('N', value)),
#                         o = (grepl('O', value)),
#                         p = (grepl('P', value)),
#                         r = (grepl('R', value)),
#                         s = (grepl('S', value)),
#                         t = (grepl('T', value)),
#                         w = (grepl('W', value)),
#                         x = (grepl('X', value)),
#                         y = (grepl('Y', value)),
#                         z = (grepl('Z', value))) %>% 
#        dplyr::select(-c(value, starts_with('var'))) %>%
#         group_by(id, month) %>%
#         summarise_all(., sum)
#   
# end1 = Sys.time()
# end1-start1

### NOTE: Adding new columns to create in mutate() does increase the time, roughly linearly (i.e. 2x more columns = 2x longer run time)
### NOTE: Increasing number of rows to work with does increase the time, roughly linearly  (i.e. 10x more columns = 10x longer run time)
### NOTE: Mutating without grouping first (why did I do that in the first place???) does decrease the time, but only around 20% on 10^7 rows
### NOTE: Adding 'spare' columns to the dataframe doesn't seem to affect the run time


# start2=Sys.time()

# temp4 = temp[temp$id == 6,]
# temp4 = temp4 %>% ungroup()
# temp4$id = temp4$id %>% as.character()

# temp3 = temp %>% mutate(asthma = (grepl('A', value)),
#                         broken = (grepl('B', value)),
#                         c = (grepl('C', value)),
#                         d = (grepl('D', value)),
#                         e = (grepl('E', value)),
#                         f = (grepl('F', value)),
#                         g = (grepl('G', value)),
#                         h = (grepl('H', value)),
#                         i = (grepl('I', value)),
#                         j = (grepl('J', value)),
#                         k = (grepl('K', value)),
#                         l = (grepl('L', value)),
#                         m = (grepl('M', value)),
#                         n = (grepl('N', value)),
#                         o = (grepl('O', value)),
#                         p = (grepl('P', value)),
#                         r = (grepl('R', value)),
#                         s = (grepl('S', value)),
#                         t = (grepl('T', value)),
#                         w = (grepl('W', value)),
#                         x = (grepl('X', value)),
#                         y = (grepl('Y', value)),
#                         z = (grepl('Z', value))) %>%
#   dplyr::select(-c(value, starts_with('var')))

# temp3 = fsum(temp3 %>% group_by(id,month))


# end1 = Sys.time()
# end1-start2
# 
# 
# 
# temp2 = temp2[order(temp2$month),]
# temp2 = temp2[order(temp2$id),]
# temp2 = as.data.frame(temp2)
# rownames(temp2) = 1:nrow(temp2)
# 
# temp3 = temp3[order(temp3$month),]
# temp3 = temp3[order(temp3$id),]
# rownames(temp3) = 1:nrow(temp3)
# temp3 = as.data.frame(temp3)
# 
# identical(temp2, temp3)

# '-----
# SCRAPBOOK -----
#



### NOTE: Patient deceased (in some cases code of discharge indicates death, but 
### there is no corresponding date of death; in those cases assume that 
### the end of treatment date on a bill ending with a discharge code 
### 10 (deceased) is the respective death date)


### If date of death missing, but type of discharge as 'deceased' the assign the end of treatment as death date
# dta_diagnosis$dateofdeath[dta_diagnosis$dateofdeath == 0 & dta_diagnosis$codeofdischargetype == 10 & !is.na(dta_diagnosis$codeofdischargetype)]  <-  dta_diagnosis$endoftreatment[dta_diagnosis$dateofdeath == 0 & dta_diagnosis$codeofdischargetype == 10 & !is.na(dta_diagnosis$codeofdischargetype)]
# 
# 
# temp = dta_diagnosis$patientidencrypted[dta_diagnosis$dateofdeath == 0 & dta_diagnosis$codeofdischargetype == 10 & !is.na(dta_diagnosis$codeofdischargetype)] 
# 
# dta_diagnosis %>% filter(patientidencrypted %in% unique(temp)) %>% View()
# 
# sf( patient_ecm_eligible$ecm_include_patient[patient_ecm_eligible$id  %in% temp])
# 
# ### Check
# # summary(dta_diagnosis$dateofdeath[dta_diagnosis$codeofdischargetype == 10 & !is.na(dta_diagnosis$codeofdischargetype)])
# # R: All non-missing and ranged between 01/2018 and 05/2023
# 
# ### If there are bills after the date of death, then assign 0 for those discharged without 10 and end of treatment as death date otherwise
# dta_diagnosis$dateofdeath[dta_diagnosis$dateofdeath != 0 & dta_diagnosis$startoftreatment > dta_diagnosis$dateofdeath ] = ifelse(
#   dta_diagnosis$codeofdischargetype[dta_diagnosis$dateofdeath != 0 & dta_diagnosis$startoftreatment > dta_diagnosis$dateofdeath] == 10,
#   dta_diagnosis$endoftreatment[dta_diagnosis$dateofdeath != 0 & dta_diagnosis$startoftreatment > dta_diagnosis$dateofdeath],
#   0
# )



#
# END OF CODE ----
#