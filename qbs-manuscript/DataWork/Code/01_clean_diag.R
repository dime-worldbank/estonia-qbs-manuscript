
### Source the '00_global.R' script with required packages and functions

if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path('~/World_Bank/Locker/Estonia/Health/Billing/R Scripts/Clean/00_global.R'))
}



#
#  THIS CODE CONSTRUCTS THE FOLLOWING INDICATORS: ----
#
#   Incidence of new diagnosis - TO DO
#   Avoidable hospital admission for COPD, Diabetes, Congestive heart failure, hypertension - DONE
#   Emergency department visit - DONE
#   Inpatient readmission within 90 days after previous inpatient readmission - DONE
#   Inadequate follow up for acute inpatient care or surgery - TO DO

### For diagnosis codes check: http://icd9.chrisendres.com/ OR https://www.icd10data.com/search?s=T51.0

# diagnosis = fread(file.path(project_path, 'Data/Raw', 'Diagnosis_code_desc.csv'))
# 
# inpatient_merge_diag$CodeOfDiagnos2 <-gsub("\\.0.*", '', inpatient_merge_diag$CodeOfDiagnos)
# hospitalization$CodeOfDiagnos2 <-gsub("\\.0.*", '', hospitalization$CodeOfDiagnos)
# pr(hospitalization$CodeOfDiagnos2[!is.na(hospitalization$CodeOfDiagnos2)]  %in% diagnosis$ICD9Code)
# sf(inpatient_merge_diag$CodeOfDiagnos[which((inpatient_merge_diag$CodeOfDiagnos2  %in% diagnosis$ICD9Code)==F)])



#
# AVOIDABLE HOSPITALIZATION ---- 
#

### Load inpatient dataset
inpatient1 <- fread(file.path(raw_2021, "inpatient_care_2019_2020_2021.csv"), encoding = 'UTF-8')
inpatient2 <- fread(file.path(raw_2022, "inpatient_care_2022.csv"), encoding = 'UTF-8')
inpatient3 <- fread(file.path(raw_2022_2, "inpatient_care_2021_2022.csv"), encoding = 'UTF-8')
inpatient4 <- fread(file.path(raw_2022_nov, "inpatient_care_2022.csv"), encoding = 'UTF-8')

inpatient <- rbind(inpatient1, inpatient2, inpatient3, inpatient4)


### Load all the diagnosis datasets

inpatient_diag2 <- fread(file.path(raw_2022, "inpatient_care_diag_2022.csv"), encoding = 'Latin-1')
inpatient_diag3 <- fread(file.path(raw_2022_2, "inpatient_care_diag_2021_2022.csv"), encoding = 'UTF-8')
inpatient_diag4 <- fread(file.path(raw_2022_nov, "inpatient_care_diag_2022.csv"), encoding = 'UTF-8')

inpatient_diag <- rbind(inpatient_diag2, inpatient_diag3, inpatient_diag4)


### Merge bills to diag
inpatient_merge_diag <- left_join(inpatient, inpatient_diag, by = "BillNr")


### Restrict to patients from August 2021 onwards
inpatient_merge_diag$year <- substring(inpatient_merge_diag$StartOfTreatment,1,4)
table(inpatient_merge_diag$year, useNA = "ifany")/1000

inpatient_merge_diag$month <- substring(inpatient_merge_diag$StartOfTreatment,5,6)
table(inpatient_merge_diag$month, useNA = "ifany")/1000

#inpatient_merge_diag <- subset(inpatient_merge_diag, inpatient_merge_diag$StartOfTreatment >= 20210731 )
inpatient_merge_diag$treat_period <- ifelse(inpatient_merge_diag$StartOfTreatment > 20210731, '1', '0')

#inpatient_merge_diag = inpatient_merge_diag[inpatient_merge_diag$treat_period == 1,]

# Treat as date
inpatient_merge_diag$StartOfTreatment <- ymd(inpatient_merge_diag$StartOfTreatment)
inpatient_merge_diag$EndOfTreatment <- ymd(inpatient_merge_diag$EndOfTreatment)


# Total hospitalization
inpatient_merge_diag$n <- 1

### Code preventable hospitalization as 0-1 dummies based on diagnosis codes

# Treat code of diagnoses as character
inpatient_merge_diag$CodeOfDiagnos <- as.character(inpatient_merge_diag$CodeOfDiagnos)

# Asthma
inpatient_merge_diag$asthma <- ifelse(str_detect(inpatient_merge_diag$CodeOfDiagnos, "^J45"), 1, 0)

# COPD 
inpatient_merge_diag$copd   <- ifelse(str_detect(inpatient_merge_diag$CodeOfDiagnos, "^J44"), 1, 0)

# Diabetes
inpatient_merge_diag$e11_diag   <- ifelse(str_detect(inpatient_merge_diag$CodeOfDiagnos, "^E11"), 1, 0)

# Congestive heart failure I50.9
inpatient_merge_diag$i50_diag  <- ifelse(str_detect(inpatient_merge_diag$CodeOfDiagnos, "I50.9"), 1, 0)

# Hypertension
inpatient_merge_diag$i10_diag <- ifelse(str_detect(inpatient_merge_diag$CodeOfDiagnos, "I10|I11|I12|I13|I15"), 1, 0)

### Emergency visits
inpatient_merge_diag$ambulance_emergency <- ifelse(str_detect(inpatient_merge_diag$CodeofAdmissionTypeorAdmitt,"E-T0001"),1,0)


### Merge by anonymized id with treatment status data
treatment_status <- read.csv(file.path(data, "ecm_patient_randomization.csv"))
hospitalization <- left_join(treatment_status, inpatient_merge_diag, by = "PatientIDencrypted")

# Treat those with no inpatient visits in this period as '-1' for 'treat_period' variable
hospitalization$treat_period[is.na(hospitalization$treat_period)]= '-1'


### Now group by bill-patient-period and then aggregate to patient-period only.

# First define grouping variables
grouping_var = c('BillNr', 'PatientIDencrypted', 'ecm_patient_id', 'list_id', 'doctor_code', 'class_code',
                 # 'Gender', 'DateOfBirth','ResidenceCodeSettlementLevel',
                 'patient_treatment', 'treat_ecm', 'treat_period')

# Group
hospitalization = hospitalization %>% group_by(across(grouping_var)) %>% 
  dplyr::select(c(asthma, copd, e11_diag, i50_diag, i10_diag, ambulance_emergency, n)) %>%
  summarize_all(.,sum_miss) %>% # Sum all outcome variables
  mutate(across(c(asthma, copd, e11_diag, i50_diag, i10_diag, ambulance_emergency, n),  ~ ifelse(. > 0, 1, 0))) %>% # But we want to work with dummies only
  ungroup() %>% group_by(across(grouping_var[-which(grouping_var == "BillNr")]))   %>% # Summarize bills for each patient
  dplyr::select(c(asthma, copd, e11_diag, i50_diag, i10_diag, ambulance_emergency, n)) %>% summarise_all(., sum_miss) %>%
  mutate(across(c(asthma, copd, e11_diag, i50_diag, i10_diag, ambulance_emergency, n),  ~ ifelse(. > 0, 1, 0))) 
  

### Add data summarized costs of all procedures per patient-period    
### (see '01_clean_procedures.R' script)
hospitalization_procedures = read.csv(file.path(data, "patient_procedures_costs.csv")) %>% mutate(treat_period = as.character(treat_period))
hospitalization = left_join(hospitalization, hospitalization_procedures)


### NOTE: In the present set-up we have patients who are either entirely missing from
### the inpatient dataset (meaning they weren't hospitalized at all), those who are present
### only for the pre-treatment period, post-treatment period, OR BOTH. We want to highlight
### those in both pre- and post-treatment. For now we don't want pre-treatment, so we 
### mark those observations for removal

### Create empty vector to store row indices to remove
remove1 = c()

### For each ID occurring in pre-treatment period, check if it also occurs in the post-treatment period. If so, mark for removal
for(i in unique(hospitalization$PatientIDencrypted[hospitalization$treat_period=='0'])){
  if(i %in% hospitalization$PatientIDencrypted[hospitalization$treat_period=='1']){
    remove1 = c(remove1, i)
  }
}
### Remove ID's occurring in both periods
hospitalization = hospitalization[which((hospitalization$PatientIDencrypted %in% remove1 & hospitalization$treat_period == '0')==F),]


### Those patients who are only in the dataset in the pre-treatment period and not in post-treatment period
### are then assigned NA's on all the outcome variables
hospitalization[hospitalization$treat_period == 0, c('asthma', 'copd', 'e11_diag', 'i50_diag', 'i10_diag',
                                                     'ambulance_emergency', 'n', 
                                                     'price_procedure_times_sum', 'price_procedure_times_mean')] <- NA

### Assign 0's to NA's
hospitalization[is.na(hospitalization)] <- 0

### Remove ID colulmn
hospitalization <- hospitalization %>% dplyr::select(-PatientIDencrypted)

### Merge with the final list of participating GPs
gp_list = fread(file.path(data, "participating_gp_final.csv"))
hospitalization = merge(hospitalization, gp_list)


#
# INPATIENT RE-ADMISSION (ADD)----
#

# Group by patient to calculate duration between the start of two different bills
inpatient <- subset(inpatient, inpatient$StartOfTreatment >= 20210731 )

# Leave only required columns...
inpatient_stays <- inpatient %>% dplyr::select(BillNr, PatientIDencrypted, StartOfTreatment, EndOfTreatment)

# ...and patients in the intervention to make the code work faster, removing those patients not admitted at all during this period
inpatient_stays <- left_join(treatment_status, inpatient_stays, by = "PatientIDencrypted") %>% filter(!is.na(StartOfTreatment))

# Calculate time (in days) since last bill
inpatient_stays <- inpatient_stays %>% # we take our data frame
  # group_by(PatientIDencrypted) %>% mutate(N = n()) %>% ungroup() %>% # If we want to find how many times patients re-admitted
  mutate(admit_date = as.Date(as.character(StartOfTreatment), format="%Y%m%d")) %>% # turn the dates into date format
  arrange(PatientIDencrypted, admit_date) %>% # sort them first by mrn and then by admit_date
  ungroup() %>% 
  group_by(PatientIDencrypted) %>% # group them by mrn so we can for each patient...
  mutate(daysSinceLastAdmit = admit_date - dplyr::lag(admit_date, default = first(admit_date))) %>%  # ...get the days since last admit
  mutate(daysSinceLastAdmit = as.integer(daysSinceLastAdmit)) %>% # turn this into an integer
  ungroup() # ungroup it (must be done - don't ask)

# Now re-join to have full data frame of patients included in the intervention
inpatient_stays <- left_join(treatment_status %>% dplyr::select(c(PatientIDencrypted)),
                             inpatient_stays %>%  dplyr::select(c(PatientIDencrypted, daysSinceLastAdmit)))

# Calculate which re-admissions were 'short', i.e. <90 days and treat patients with at least
# one short readmission as '1' and others as '0'
inpatient_stays = inpatient_stays %>% mutate(readmission_short = ifelse(daysSinceLastAdmit<90 & daysSinceLastAdmit>0, 1, 0)) %>%
  group_by(PatientIDencrypted) %>% 
  dplyr::select(-c(daysSinceLastAdmit)) %>% summarise_all(.,sum_miss) %>%
  mutate(across(c(readmission_short),  ~ ifelse(. > 0, 1, 0))) 

# Merge with the hospitalization file (avoidable and emergency hospitalizations) created above
hospitalization = merge(hospitalization, inpatient_stays)
hospitalization = relocate(hospitalization, .after = 'n', 'readmission_short')

### Save
write.csv(hospitalization, file.path(data, "patient_dta_reg.csv"), row.names = F)


#
# END OF CODE ----
#