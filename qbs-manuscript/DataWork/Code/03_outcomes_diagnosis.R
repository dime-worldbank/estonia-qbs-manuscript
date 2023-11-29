

#
# SET-UP ---------------------------------------------------------------------------------------
# 

### Source the '00_global.R' script with required packages and functions
if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "jonas"){
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}

### Make copy of the file
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)


### Diagnosis data ---------------------------------------------------------------------------------------
dta_diagnosis = read_parquet(file.path(project_path, 'Data', 'Clean', 'Diagnoses_all.parquet')) %>% mutate(patientidencrypted  = as.character(patientidencrypted ))

### For diagnosis codes check: http://icd9.chrisendres.com/ OR https://www.icd10data.com/search?s=T51.0
diagnosis_codes = fread(file.path(project_path, 'Data', 'Clean', 'Other', 'Diagnosis_code_desc.csv'))
severe_codes = fread(file.path(project_path, 'Data', 'Clean', 'Other', 'ICD codes of acute conditions.csv'))


### NOTE: ENSURE ONLY PURE CONTROL, CONTROL AND TREATMENT PATIENTS ARE KEPT (FOR NOW) ----
# patient_ecm_eligible = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible.csv')) %>% mutate(id = as.character(id))
patient_ecm_eligible_demo = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible_demo.csv')) %>% mutate(id = as.character(id))

dim(patient_ecm_eligible_demo)
dta_diagnosis = dta_diagnosis %>% filter(patientidencrypted %in% patient_ecm_eligible_demo$id)



#
# EXTRACT DATE OF DEATH ---------------------------------------------------------------------------------------
#

# Discard all data before May 2021 (min. death date in the date) -> all ECM patients should be alive until then 
dta_deaths = dta_diagnosis %>% filter(startoftreatment >= min(dta_diagnosis$dateofdeath[dta_diagnosis$dateofdeath!=0]))

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

# Identify common columns
common_columns = intersect(intersect(names(temp1), names(temp2)),
                           intersect(names(temp3), names(temp4)))

# Add missing columns to each temp data frame (if any)
temp1[, setdiff(common_columns, names(temp1))] <- NA
temp2[, setdiff(common_columns, names(temp2))] <- NA
temp3[, setdiff(common_columns, names(temp3))] <- NA
temp4[, setdiff(common_columns, names(temp4))] <- NA

# Add the scenario identifier to each temporary data frame
temp1$scenario <- 'Scenario 1'
temp2$scenario <- 'Scenario 2'
temp3$scenario <- 'Scenario 3'
temp4$scenario <- 'Scenario 4'

# Combine all scenarios using bind_rows from dplyr
all_scenarios <- bind_rows(temp1, temp2, temp3, temp4)

# Count deaths in each scenario
death_counts = all_scenarios %>%
  group_by(scenario) %>%
  summarize(deaths = sum(dateofdeath_final != 0))

# Count deaths in raw and final datasets
raw_deaths_count = sum(dta_deaths$dateofdeath != 0)
final_deaths_count = sum(dta_deaths$dateofdeath != 0)

# Combine counts into a summary dataset
summary_data = rbind(death_counts,
                     data.frame(scenario = 'Raw Data', deaths = raw_deaths_count),
                     data.frame(scenario = 'Final Saved Data', deaths = final_deaths_count))

# Export to CSV
write.csv(summary_data, 'death_summary.csv', row.names = FALSE)



# 
# CREATE OUTCOMES -------------------------------------------------------------------------------------------------
#
### Remove unnecessary columns
### NOTE: Also remove 'billnr' column. Now, this results in ~2% fewer rows 
### after collapsing by diagnosis code below, BUT we do ensure we have
### the same patient, on the same day, with the same dataset and admission type,
### so by all accounts we should treat it as a single healthcare interaction
### even if there are sometimes >1 bill numbers. 

# Reading the data
dta_diagnosis = read_parquet(file.path(project_path, 'Data', 'dta_diagnosis_temp.parquet'))

# Initial data processing
dta_diagnosis = dta_diagnosis %>% ungroup() %>%
  dplyr::select(c(patientidencrypted, dateofdeath, deceased, dataset, startoftreatment, endoftreatment, codeofdiagnos, codeofadmissiontypeoradmitt))

# Collapse diagnosis codes
dta_diagnosis <- dta_diagnosis %>%
  group_by(across(setdiff(names(dta_diagnosis), c('codeofdiagnos')))) %>%
  summarize(codeofdiagnos = paste(codeofdiagnos, collapse = ", ")) %>% ungroup()

# Identify relevant diagnosis codes etc.
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
    
    osteoporosis  =  (grepl('M80|M81', codeofdiagnos)),
    covid_incidence  =  (grepl('U07.1', codeofdiagnos)),
    
    weight_high = (grepl('E66|R63.5', codeofdiagnos)),
    weight_low = (grepl('R63.4|R63.6|^T75.82|^X52', codeofdiagnos))
    
    )
# Exporting the processed data to a CSV file
write.csv(dta_diagnosis, file = "dta_diagnosis_outcomes.csv", row.names = FALSE)

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



#
# GROUP... -----------------------------------------------------------------------------------------------------------------
#

dta_diagnosis = read_parquet(file.path(project_path, 'Data/Clean', 'dta_diagnosis.parquet'))
patient_ecm_eligible_demo = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible_demo.csv')) %>% mutate(id = as.character(id))


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
dta_diagnosis_month = left_join(expand_grid(id = patient_ecm_eligible_demo$id,
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
dta_diagnosis_year = left_join(expand_grid(id = patient_ecm_eligible_demo$id,
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
dta_diagnosis_year_rel = left_join(expand_grid(id = patient_ecm_eligible_demo$id,
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
dta_diagnosis_period_18_23 = left_join(expand_grid(id = patient_ecm_eligible_demo$id,
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
dta_diagnosis_period_09_23 = left_join(expand_grid(id = patient_ecm_eligible_demo$id,
                                                   dta_diagnosis_period_09_23 %>% dplyr::select(treat_period) %>% distinct() %>% as.data.frame()) %>%
                                         group_by(id) %>% distinct() %>% ungroup(),
                                       dta_diagnosis_period_09_23) %>% 
  mutate(across(-c(id, treat_period), ~replace_na(.,0))) # 0's instead of NA's


### Save ----
write_parquet(dta_diagnosis_month, file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_month_18_23.parquet')) # NOTE: On purpose, we don't need pre-2018 on a monthly basis for now
write_parquet(dta_diagnosis_year,  file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_year_09_23.parquet'))
write_parquet(dta_diagnosis_year_rel,  file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_year_rel_09_23.parquet'))
write_parquet(dta_diagnosis_period_18_23,  file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_period_18_23.parquet'))
write_parquet(dta_diagnosis_period_09_23,  file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_period_09_23.parquet'))


#
# END OF CODE ----
#