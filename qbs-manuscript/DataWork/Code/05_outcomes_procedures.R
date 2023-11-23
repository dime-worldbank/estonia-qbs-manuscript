

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



### Make copy of the file
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)

### NEW VARIABLES? ----

# ->  (NOT DONE) no. of diagnosed procedures (average / total?) -> 1) how to summarize  2) procedures sometimes are like - lying in bed; would need something more substantinve
# -> (DONE) CABG: Coronary Artery Bypass Graft -> T82 (ICD-10) -> 1F2101 is the code used by
# -> (DONE) THA/TKA:  Total Hip Arthroplasty and/or Total Knee Arthroplasty



### Costs data  --------------------------------------------------------------


# cost_procedures_raw = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'Other', "costs_procedures.csv"), encoding = 'UTF-8'))
# cost_procedures_raw %>% dplyr::select(c(hkkood, sap_hkkood, nimetus)) %>% distinct() %>% View


cost_procedures = clean_names(fread(file.path(project_path, 'Data', 'Clean', 'Other', "costs_procedures.csv"), encoding = 'UTF-8'))


### Procedures data --------------------------------------------------------------

dta_procedures = read_parquet(file.path(project_path, 'Data', 'Clean', 'Procedures_all.parquet'))

### NOTE: Subset --------------------------------------------------------------------------------------------------------
# dta_procedures = dta_procedures %>% filter(startoftreatment > 20210101)

### In 02_billing_clean.R the data was subset by startoftreatment, but some dateofprocedure are still wrong -----------
# Set wrong dateofprocedure to startoftreatment
# MOVE TO PREVIOUS SCRIPTS?
dta_procedures = dta_procedures %>% 
  mutate(dateofprocedure = ifelse(dateofprocedure < 20090101, startoftreatment, dateofprocedure),
         dateofprocedure = ifelse(dateofprocedure > 20230331, startoftreatment, dateofprocedure))


# dta_procedures %>% slice(1:10^5) %>% View


# ### Any overlap between procedure and diagnosis codes?
# sf(unique(dta_procedures$procedure) %in% unique(dta_diagnosis$procedure))
# R: No


### Remove unnecessary columns
dta_procedures = dta_procedures %>% 
  ungroup() %>%
  dplyr::select(c(patientidencrypted, dateofprocedure, dataset, procedure, nroftimes))



### Number of times --------------------------------------------------------------------------------------------------------

# Assign 1 if values negative
dta_procedures$nroftimes[dta_procedures$nroftimes <= 0] = 1

# Winsorize by group
min1 = min(dta_procedures$nroftimes, na.rm=T)

dta_procedures <- dta_procedures %>% 
  group_by(procedure) %>%  
  mutate(nroftimes_win =  Winsorize(nroftimes, minval = min1, maxval = NULL, probs = c(0.001, 0.99),
                                    na.rm = FALSE, type = 7))



###  Procedure codes --------------------------------------------------------------------------------------------------------

# Change o/O to 0
dta_procedures = dta_procedures %>% mutate(procedure = gsub('o|O', '0', procedure))

# Remove leading 0's from the procedure as they tend to vary in length making matching more difficult
# This will make codes in match 'hkkood' column in 'cost_procedures'
dta_procedures = dta_procedures %>% mutate(procedure = ifelse(test = grepl('^000',procedure,),
                                                                  yes  = gsub('^0+', '', procedure),
                                                                  no   = procedure)) %>% 
                                    ungroup()



# tapply(dta_procedures$intensive_care_11, dta_procedures$dataset, table)
# tapply(dta_procedures$intensive_care_12, dta_procedures$dataset, table)


# 
# CREATE OUTCOMES -------------------------------------------------------------------------------------
#




### NOTE: All Covid-19 codes double-checked to be correct
### NOTES: Check 

dta_procedures = dta_procedures %>% mutate(
  
  n_procedure = T,
  
  covid_test = grepl("3183$|66634|66645|9519$", procedure),
  covid_vaccine = grepl("3197$|3199$|9590$|9591$|9592$|9593$|9594$|9595$|9596$|9597$|9598$|9599$", procedure),
  covid_vaccine_refuse = grepl("9589$", procedure),
  consult_include = grepl("9092", procedure),
  #consult_include2 = grepl("9091", procedure),
  #consult_include3 = grepl("9093", procedure),
  consult_refuse = grepl("9090", procedure),
  consult_care_plan = grepl("9095", procedure),
  consult_gp = grepl("9044", procedure),
  consult_gp_phone = grepl("9018", procedure),
  #consult_gp_email = grepl("9019", procedure),
  consult_nurse = grepl("9061", procedure),
  consult_nurse_phone = grepl("9064", procedure),
  #consult_nurse_email = grepl("9065", procedure)
  consult_any = grepl('9044|9018|9061|9064', procedure),
  
  coronoary_bypass = grepl('1F2101', procedure),
  hip_knee_arthroplasty = grepl('0N2139|0N2140|0N2205|2650L|2651L|2652L|2655L|2656L|2674L|2951L|0N2141|2662L|2661L|2672L|0N2143', procedure), #  40214 -> no, burn deformities; knee from 2674L onwards
  

  intensive_care_1 =  grepl('2044|2070', procedure),
  intensive_care_2 =  grepl('2045|2071', procedure),
  intensive_care_3 =  grepl('2046|2072', procedure),
  intensive_care_3A =  grepl('2059|2073', procedure),
  
  intensive_care_any =  grepl('2044|2070|2045|2071|2046|2072|2059|2073', procedure),
  
  
  ### RIIGI TEATAJA (in top->down order)
  
  m_glicihem_riigi = grepl('66118', procedure),
  m_glicihem_all = grepl('66118|6506A|9118|9050', procedure),
  
  m_creatinine_riigi = grepl('66102', procedure),
  m_creatinine_all = grepl('66102|9102|6500D', procedure),
  
  m_chol_tri_riigi = grepl('66104', procedure),
  m_chol_tri_all   = grepl('6503F|6501F|66104|6501G|66105|9105|6503G|9104|9040|9042|6502L', procedure),
  
  m_glucose_riigi = grepl('66101', procedure),
  m_glucose_all = grepl('66101|9050|9101|9131|9118|9011|6500B|9067Z', procedure),
  
  m_ecg_riigi = grepl('6320|6322|6323', procedure),
  
  m_thc_riigi = grepl('66706', procedure),
  
  m_any_riigi = if_any(c(m_glicihem_riigi, m_creatinine_riigi, m_chol_tri_riigi, m_glucose_riigi, m_ecg_riigi, m_thc_riigi), ~.x == TRUE)

  )



# dta_procedures[sample(1:nrow(dta_procedures), 50, replace = F),]


### Nr of times -------------------------------------------------------------------------------------


dta_procedures = dta_procedures %>% mutate(across(-c(patientidencrypted, dateofprocedure, dataset,
                                                     procedure, nroftimes, nroftimes_win, n_procedure), ~. * nroftimes_win))


### Save
write_parquet(dta_procedures, file.path(project_path, 'Data/Clean', 'dta_procedures_temp.parquet'))



dta_procedures_save = dta_procedures

  

#
# ECM inclusion dates -------------------------------------------------------------------------------------
#

dta_procedures = read_parquet(file.path(project_path, 'Data/Clean', 'dta_procedures_temp.parquet'))


# dta_procedures$consult_include %>% sf()

### NOTE: Define as 'accepted' only those who have both 'consult_include' and 'consult_care_plan' 

patient_ecm_accept = dta_procedures %>% 
  filter(consult_care_plan > 0) %>% # Leave only observations care plan codes
  dplyr::select(any_of(c('patientidencrypted', 'dateofprocedure', 'consult_care_plan'))) %>% # Only required columns
  group_by(patientidencrypted) %>% # For each patient....
  mutate(ecm_status_patient = any(consult_care_plan>0)) %>% # ... code if they had ECM care plan codes...
  filter(ecm_status_patient) %>% # ..leaving only those that did... 
  mutate(ecm_status_patient_date = min(dateofprocedure)) %>% #  .. find first date at which those codes occur
  ungroup() %>% 
  dplyr::select(c(patientidencrypted, ecm_status_patient, ecm_status_patient_date)) %>% # Leave only unique observations
  distinct() %>% 
  mutate(ecm_status_patient = ifelse(ecm_status_patient, 'ECM accept', 'Not ECM Accept')) %>% 
  mutate(ecm_status_patient_date = ymd(ecm_status_patient_date)) %>% 
  rename('id' = 'patientidencrypted')
  # Rename columns

### Checks
#sf(patient_ecm_accept$ecm_status_patient %>% is.na()) # No NA's
#hist(patient_ecm_accept$ecm_status_patient_date,
#     breaks = 'month') # First bump in care plans = pilot; Second bump = clearly post-June 2021, ECM intervention
#sf(ym(paste0(year(patient_ecm_accept$ecm_status_patient_date),'-',
#             month(patient_ecm_accept$ecm_status_patient_date)))) # Same information as above, but numerical with numbers by month


table(patient_ecm_accept$ecm_status_patient_date > ymd(20210601))

### Save IDs of patients with relevant codes from BEFORE ECM rollout (i.e. pilot patients)
id_pilot = patient_ecm_accept$id[patient_ecm_accept$ecm_status_patient_date < ymd(20210601)] %>% unique


### Remove those with relevant codes BEFORE ECM rollout (i.e. pilot patients)
patient_ecm_accept = patient_ecm_accept %>% filter(ecm_status_patient_date > ymd(20210601))
summary(patient_ecm_accept$ecm_status_patient_date)


### Plot  timeline of inclusion by ECM treatment assignment -> in XX_figuers.R 

### Save 
fwrite(patient_ecm_accept, na=NA, row.names = F, file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_ecm_accept.csv"))


### Remove pilot patients across ID datasets
files1 = list.files(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion'), pattern = '^id_|patient_ecm_eligible')
file1 = 'id_treatment.csv'

for(file1 in files1){
  if(file1 %in% c('id_codes.csv')){next}
  
  fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', paste0(file1))) %>% 
    filter(!(id %in% id_pilot))  %>%
    fwrite(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', paste0(file1)), na=NA, row.names = F)
}


### Save
# fwrite(patients, na=NA, row.names = F,
#        file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_ecm_eligible_all_accept.csv"))


#
# CLEAN PROCEDURES ------------------------------------------------------------------------------------------------------
#

### Dates  ------------------------------------------------------------------------------------------------------
dta_procedures = read_parquet(file.path(project_path, 'Data/Clean', 'dta_procedures_temp.parquet'))

# dta_procedures = dta_procedures_save
gc()
dta_procedures = dta_procedures %>%
  ### Treatment date
  mutate(   
    year = substr(dateofprocedure, 1, 4), # ... specify year
    year_month = paste0(substr(dateofprocedure, 1,4), '-', substr(dateofprocedure, 5,6)), # ... specify year and month
    year_month_day = ymd(dateofprocedure), # ... specify year-month-day (full daye)
    month = month(year_month_day), # ... specify just the month
    
    year_rel = time_length(difftime(year_month_day, as.Date("2021-06-01")), "years") %>% sign() * # ... specify how many years before or after ECM onset
      time_length(difftime(year_month_day, as.Date("2021-06-01")), "years") %>% abs() %>% ceiling(),
    year_rel = ifelse(year_rel == 0, 1, year_rel), # ... change 0 to 1 in year_rel (observations on the day of ECM onset)
    
    treat_period = ifelse(dateofprocedure < 20210601, 0, 1) # ... specify treatment period
  ) %>% 
  # filter(dateofprocedure >= 20090101) %>% filter(dateofprocedure <= 20230331) %>%  # ...remove any observations outside the intervention range 
  relocate(.after = 'dateofprocedure', c('treat_period','year', 'year_month','month', 'year_month_day')) # ... order columns

### Year_rel creation above fails to account for leap years and results in a tiny number of borderline observations, that is
### those occuring on the onset date (01/06/2021) to be classified into a wrong year. Applying mode of year_rel to all
### observations by month seems to solve the issue
dta_procedures = dta_procedures %>% group_by(year_month) %>% mutate(year_rel = Mode(year_rel)) %>% ungroup()


### Remove columns if not needed
dta_procedures = dta_procedures %>% dplyr::select(-c(dateofprocedure))


gc()
### Check date range
# dta_procedures$year_month[sample(1:nrow(dta_procedures), 20, replace=F)]
# summary(dta_procedures$dateofprocedure)
# tapply(dta_procedures$year_month_day, dta_procedures$treat_period, summary)
# R: All within the range of January 2018 - March 2023 and in the assigned range of the treatment period


write_parquet(dta_procedures, file.path(project_path, 'Data/Clean', 'dta_procedures_temp.parquet'))





### Prices -------------------------------------------------------------------------------------

### Merge bill information with procedure costs

# Create a month-by-month grid to know the relevant price for every month and procedure
temp = expand.grid(unique(cost_procedures$hkkood), 
            seq.Date(from = dmy(01012009), to = dmy(01042023), by = 'months')) %>% 
        rename('hkkood' = 'Var1', 'date' = 'Var2')


# Match with costs data and leave only the months that fit between 'from' and 'until' dates for which a given price was valid
temp = left_join(cost_procedures, temp) %>% 
  filter(date <= until | is.na(until)) %>% filter(date >= from) %>% 
  mutate(year_month = paste0(year(date),
                                        '-',
                                        ifelse(nchar(month(date))==1, paste0('0', month(date)), month(date))))





temp = temp %>% 
  dplyr::select(-c(procedure, procedure_class, from, until, date)) %>% 
  rename('procedure' = 'hkkood') %>% as.data.frame()

write_parquet(temp, file.path(project_path, 'Data', 'costs_temp.parquet'))



gc()
temp = read_parquet(file.path(project_path, 'Data', 'costs_temp.parquet'))
dta_procedures = read_parquet(file.path(project_path, 'Data/Clean', 'dta_procedures_temp.parquet'))
gc()



# Match with 'dta_procedures'
dta_procedures <- left_join(dta_procedures,
                             temp,
                             by = c('procedure', 'year_month'))


# temp2 <- left_join(dta_procedures %>% slice(1:(25*10^6)), 
#                    temp,
#                    by = c('procedure', 'year_month'))
# 
# 
# temp3 <- left_join(dta_procedures %>% slice(((25*10^6)+1):((50*10^6)+0)), 
#                    temp,
#                    by = c('procedure', 'year_month'))
# 
# 
# temp4 <- left_join(dta_procedures %>% slice(((50*10^6)+1):(nrow(dta_procedures))), 
#                    temp,
#                    by = c('procedure', 'year_month'))
# 
# 
# dta_procedures = rbindlist(list(temp2,temp3,temp4))

n_distinct(dta_procedures$patientidencrypted)

### Checks (dataset)
# dim(dta_procedures) # 34,015,263 x 24
# pr(dta_procedures$price %>% is.na()) # 0.69% missing
# summary(dta_procedures$price) # Range = 0-122,714.89; mean = 16.49; 235,544 missing


# Price * Nr. of times
dta_procedures = dta_procedures %>% mutate(price_total = price * nroftimes_win) %>% 
  relocate('price_total', .after = 'price')



# Price x Dataset
dta_procedures = dta_procedures %>% 
  mutate(
    price_inpatient = ifelse(dataset == 'inpatient', price_total, NA),
    price_outpatient = ifelse(dataset == 'outpatient', price_total, NA),
    price_primary = ifelse(dataset == 'primaryhealth', price_total, NA)
    )

# tapply(dta_procedures$price_outpatient, dta_procedures$dataset, summary)


### Checks (random manual by procedure code)
# na1=unique(temp2$hkkood[is.na(temp2$until)])
# 
# temp4 = dta_procedures %>% filter(procedure %in% na1) %>% dplyr::select(c( patientidencrypted, dateofprocedure_month, procedure, price))
# 
# temp = cost_procedures %>% filter(hkkood %in% unique(cost_procedures$hkkood[!is.na(cost_procedures$until)])) %>% 
#           group_by(hkkood) %>% mutate(N=n())
# 
# dim(temp)
# n_distinct(temp$hkkood)
# R: All matches!!


### Clean ID's before saving
dta_procedures = dta_procedures %>% rename('id' = 'patientidencrypted') %>% mutate(id=as.character(id))


### Save ------------------------------------------------------------------------------
write_parquet(dta_procedures, file.path(project_path, 'Data/Clean', 'dta_procedures.parquet'))



### GROUP.... -------------------------------------------------------------------------------------
### NOTE: We can summarise using group_by() and summarise_all() [as commented out], but takes up to x20 (sic!)
### longer than fsum(), while producing EXACTLY THE SAME RESULTS (CHECKED!)

gc()

dta_procedures = read_parquet(file.path(project_path, 'Data/Clean', 'dta_procedures.parquet'))
patient_ecm_eligible = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible.csv')) %>% mutate(id = as.character(id))

# table(dta_procedures_clean$dataset, dta_procedures_clean$consult_any>0, dta_procedures_clean$year)

###  ...by month -------------------------------------------------------------------------------------
dta_procedures_month = fsum(dta_procedures %>% 
                              filter(year >= 2018) %>%  # NOTE: We don't want any pre-2018 on a monthly basis as of now
                              dplyr::select(-c(price, contains('nroftimes'), year_month_day, dataset, procedure)) %>%
                              group_by(id, treat_period, year, year_month, month))

# Make sure all patients are there for all years
dta_procedures_month = left_join(expand_grid(id = patient_ecm_eligible$id,
                                             dta_procedures_month %>% dplyr::select(treat_period, year, year_month, month) %>% distinct() %>% as.data.frame()) %>%
                                  group_by(id) %>% distinct() %>% ungroup(),
                                 dta_procedures_month) %>% 
  mutate(across(-c(id, treat_period, year, year_month, month), ~replace_na(.,0))) # 0's instead of NA's


### ...by year -------------------------------------------------------------------------------------
dta_procedures_year  = fsum(dta_procedures %>%
                              dplyr::select(-c(price, contains('nroftimes'), contains('month'), year_rel, treat_period, dataset, procedure)) %>%
                              group_by(id, year))

# Make sure all patients are there for all years
dta_procedures_year = left_join(expand_grid(id = patient_ecm_eligible$id,
                                            dta_procedures_year %>% dplyr::select(year) %>% distinct() %>% as.data.frame()) %>%
                                    group_by(id) %>% distinct() %>% ungroup(),
                                dta_procedures_year) %>% 
  mutate(across(-c(id, year), ~replace_na(.,0))) # 0's instead of NA's


### ...by year_rel -------------------------------------------------------------------------------------
dta_procedures_year_rel  = fsum(dta_procedures %>%
                              dplyr::select(-c(price, contains('nroftimes'), contains('month'), year, treat_period, dataset, procedure)) %>%
                              group_by(id, year_rel))

# Make sure all patients are there for all years
dta_procedures_year_rel = left_join(expand_grid(id = patient_ecm_eligible$id,
                                            dta_procedures_year_rel %>% dplyr::select(year_rel) %>% distinct() %>% as.data.frame()) %>%
                                  group_by(id) %>% distinct() %>% ungroup(),
                                dta_procedures_year_rel) %>% 
  mutate(across(-c(id, year_rel), ~replace_na(.,0))) # 0's instead of NA's


### ...by period (2018-2023)  -----------------------------------------------------------------------------------------------------------------
dta_procedures_period_18_23  = fsum(dta_procedures %>%
                                  filter(year >= 2018) %>%
                                  dplyr::select(-c(price, contains('nroftimes'), contains('month'), contains('year'), dataset, procedure)) %>%
                                  group_by(id, treat_period))


# Make sure all patients are there for all periods
dta_procedures_period_18_23 = left_join(expand_grid(id = patient_ecm_eligible$id,
                                        dta_procedures_period_18_23 %>% dplyr::select(treat_period) %>% distinct() %>% as.data.frame()) %>%
                                             group_by(id) %>% distinct() %>% ungroup(),
                                  dta_procedures_period_18_23) %>% 
  mutate(across(-c(id, treat_period), ~replace_na(.,0))) # 0's instead of NA's


###  ...by period (2009-2023)  -----------------------------------------------------------------------------------------------------------------
dta_procedures_period_09_23  = fsum(dta_procedures %>%
                                      dplyr::select(-c(price, contains('nroftimes'), contains('month'), contains('year'), dataset, procedure)) %>%
                                      group_by(id, treat_period))


# Make sure all patients are there for all periods
dta_procedures_period_09_23 = left_join(expand_grid(id = patient_ecm_eligible$id,
                                                    dta_procedures_period_09_23 %>% dplyr::select(treat_period) %>% distinct() %>% as.data.frame()) %>%
                                          group_by(id) %>% distinct() %>% ungroup(),
                                        dta_procedures_period_09_23) %>% 
  mutate(across(-c(id, treat_period), ~replace_na(.,0))) # 0's instead of NA's



# For year- and period-level datasets  we need to annualize values (for year-level one, for 2023 alone). 

### Checks
# dim(dta_procedures_month)
# 20000 / (65/1.3) * (87000/1300000) # Around 20,000 coronary artery bypass grafts are carried out in England every year.
# tapply(dta_procedures_year$coronoary_bypass, dta_procedures_year$year, function(x) sf(x>0))
# 
# tapply(dta_procedures_year$hip_knee_arthroplasty, dta_procedures_year$year, function(x) sf(x>0))
# tapply(dta_procedures_year$consult_gp, dta_procedures_year$year, function(x) sf(x>0))
# 
# 
# tapply(dta_procedures_period_09_23$coronoary_bypass, dta_procedures_period_09_23$treat_period, function(x) sf(x>0))
# tapply(dta_procedures_period_09_23$hip_knee_arthroplasty, dta_procedures_period_09_23$treat_period, function(x) sf(x>0))




### Save ------------------------------------------------------------------------------
write_parquet(dta_procedures_month, file.path(project_path, 'Data/Clean', 'Procedures_outcomes_month_18_23.parquet'))
write_parquet(dta_procedures_year,  file.path(project_path, 'Data/Clean', 'Procedures_outcomes_year_09_23.parquet'))
write_parquet(dta_procedures_year_rel,  file.path(project_path, 'Data/Clean', 'Procedures_outcomes_year_rel_09_23.parquet'))
write_parquet(dta_procedures_period_18_23,  file.path(project_path, 'Data/Clean', 'Procedures_outcomes_period_18_23.parquet'))
write_parquet(dta_procedures_period_09_23,  file.path(project_path, 'Data/Clean', 'Procedures_outcomes_period_09_23.parquet'))





### Combine with diagnoses ------------------------------------------------------------------------------
dta_procedures_month = read_parquet(file.path(project_path, 'Data/Clean', 'Procedures_outcomes_month_18_23.parquet'))
dta_procedures_year = read_parquet(file.path(project_path, 'Data/Clean', 'Procedures_outcomes_year_09_23.parquet'))
dta_procedures_year_rel = read_parquet(file.path(project_path, 'Data/Clean', 'Procedures_outcomes_year_rel_09_23.parquet'))
dta_procedures_period_18_23 = read_parquet(file.path(project_path, 'Data/Clean', 'Procedures_outcomes_period_18_23.parquet'))
dta_procedures_period_09_23 = read_parquet(file.path(project_path, 'Data/Clean', 'Procedures_outcomes_period_09_23.parquet'))


dta_diagnosis_month = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_month_18_23.parquet'))
dta_diagnosis_year = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_year_09_23.parquet'))
dta_diagnosis_year_rel = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_year_rel_09_23.parquet'))
dta_diagnosis_period_18_23 = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_period_18_23.parquet'))
dta_diagnosis_period_09_23 = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses_outcomes_period_09_23.parquet'))


### Combine with prescriptions ------------------------------------------------------------------------------

dta_prescriptions_month = read_parquet(file.path(project_path, 'Data/Clean', 'Prescriptions_outcomes_month_18_23.parquet')) %>% rename_all(~sub("_prescription$", "", .)) %>% mutate(across(c(id, year), as.character))
dta_prescriptions_year = read_parquet(file.path(project_path, 'Data/Clean', 'Prescriptions_outcomes_year_09_23.parquet')) %>% rename_all(~sub("_prescription$", "", .))%>% mutate(across(c(id, year), as.character))
dta_prescriptions_year_rel = read_parquet(file.path(project_path, 'Data/Clean', 'Prescriptions_outcomes_year_rel_09_23.parquet')) %>% rename_all(~sub("_prescription$", "", .))%>% mutate(across(c(id), as.character))
dta_prescriptions_period_18_23 = read_parquet(file.path(project_path, 'Data/Clean', 'Prescriptions_outcomes_period_18_23.parquet')) %>% rename_all(~sub("_prescription$", "", .))%>% mutate(across(c(id), as.character))
dta_prescriptions_period_09_23 = read_parquet(file.path(project_path, 'Data/Clean', 'Prescriptions_outcomes_period_09_23.parquet')) %>% rename_all(~sub("_prescription$", "", .))%>% mutate(across(c(id), as.character))


nrow(dta_diagnosis_month)
nrow(dta_procedures_month)
nrow(dta_prescriptions_month)

# temp = left_join(dta_diagnosis_period_18_23, dta_procedures_period_18_23)
# temp = left_join(temp, dta_prescriptions_period_18_23 %>% rename_all(~sub("_prescription$", "", .)))


# Read dummy dataset with correct column order
temp = fread(file.path(project_path, 'Data/Clean/Other', 'name_order_dummy.csv'))



# write_parquet(left_join(dta_diagnosis_month, dta_procedures_month) %>% setcolorder(names(temp)),  
write_parquet(left_join(dta_diagnosis_month, left_join(dta_procedures_month, dta_prescriptions_month)) ,  
              file.path(project_path, 'Data/Clean', 'All_outcomes_month_18_23.parquet'))
write_parquet(left_join(dta_diagnosis_year,  left_join(dta_procedures_year, dta_prescriptions_year)) ,  
              file.path(project_path, 'Data/Clean', 'All_outcomes_year_09_23.parquet'))
write_parquet(left_join(dta_diagnosis_year_rel,  left_join(dta_procedures_year_rel, dta_prescriptions_year_rel)) ,  
               file.path(project_path, 'Data/Clean', 'All_outcomes_year_rel_09_23.parquet'))
write_parquet(left_join(dta_diagnosis_period_18_23,  left_join(dta_procedures_period_18_23, dta_prescriptions_period_18_23)) ,  
              file.path(project_path, 'Data/Clean', 'All_outcomes_period_18_23.parquet'))
write_parquet(left_join(dta_diagnosis_period_09_23, left_join(dta_procedures_period_09_23, dta_prescriptions_period_09_23)) ,  
              file.path(project_path, 'Data/Clean', 'All_outcomes_period_09_23.parquet'))




end1 = Sys.time()
print(end1-start1)


#
# SCRAPBOOK ------------------------------------------------------------------------------
#


610.5/33
87*18.5


### Column order ----

# names(dta_diagnosis_month)
# 
# temp %>% dplyr::select(c(starts_with('n_'), starts_with('readmit_'), starts_with('consult'), starts_with('price'), starts_with('covid'))) %>% names
# 
# temp = read_parquet(file.path(project_path, 'Data/Clean', 'All_outcomes_month_18_23.parquet'))
# temp = temp %>% dplyr::select(any_of(c(
#   'id', 
#   'treat_period', 'year', 'year_rel', 'year_month', 'month',
#   "n_all", "n_inpatient", "n_outpatient", "n_primary", "n_daycare", "n_inpatient_post", "n_outpatient_post", "n_diag", "n_sever_diag", "n_inpatient_avoid", "n_diag_av", 
#   "readmit_30_any", "readmit_90_any", "readmit_30_severe", "readmit_90_severe",
#   'myocardial_infarction', 'copd', 'heart_failure', 'pneumonia', 'stroke', 'coronoary_bypass', 'hip_knee_arthroplasty',
#   'asthma', 'diabetes_2', 'hypertension',
#   'admit_ambulance', 'admit_referral',
#   'alcohol_abuse', 'arthritis', 'atrial_fibrillation', 'cancer', 'chronic_kidney_disease', 'depression', 'substance_use', 'hyperlipidemia', 'osteoporosis',
#   'weight_low', 'weight_high',
#   "consult_include", "consult_refuse", "consult_care_plan", "consult_gp", "consult_gp_phone", "consult_nurse", "consult_nurse_phone", "consult_any", 
#   "price_total", "price_inpatient", "price_outpatient", "price_primary", 
#   "covid_incidence", "covid_test", "covid_vaccine", "covid_vaccine_refuse"
# )))
# 
# paste(names(temp), collapse = ', ')
# 
# fwrite(temp[1:10, ],  file.path(project_path, 'Data/Clean/Other', 'name_order_dummy.csv'), row.names = F, na = NA)


### Test ----

# start1 = Sys.time()
# 
# temp2 = (grepl("3183$|66634|66645|9519$", dta_procedures$procedure))
# end1 = Sys.time()
# end1-start1
# 
# 
# start2 = Sys.time()
# dta_procedures = dta_procedures %>% mutate(
#   covid_test = grepl("3183$|66634|66645|9519$", procedure)
# )
# end2 = Sys.time()
# end2-start2

# sf(temp2)
# sf(dta_procedures$covid_test)



#
# END OF CODE ----
#