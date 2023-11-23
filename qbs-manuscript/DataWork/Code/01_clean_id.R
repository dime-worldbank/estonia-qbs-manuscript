
#
# SET-UP ----
#

### Source the '00_global.R' script with required packages and functions
if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}


# 
# PATIENT ID'S--------------------------------------------------------------------------------------------------
#
### CREATE: Dataset listing all ID codes (encrypted + ECM-specific) for all patients (1,605,595 rows)

### Read files
id1 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "Patient_ID_1.csv"), encoding = 'UTF-8'))
id2 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "Patient_ID_2.csv"), encoding = 'UTF-8'))

### Combine
id = rbind(id1, id2)

### Clean...
id = id %>% 
  dplyr::select(-c(v1)) %>%  # ... no need for 'v1' variable 
  rename('id' = 'patient_i_dencrypted', # ... rename columns 
         'id_ecm' = 'ecm_patient_id') %>%
  mutate(id = as.character(id), id_ecm = as.character(id_ecm)) #... treat as character

### Save
fwrite(id, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'id_codes.csv'), na = NA, row.names = F)



# '--------------------------------------------------------------------------------------------------
# CLINICS--------------------------------------------------------------------------------------------------
#
### CREATE: Clinic-level file with ECM inclusion status, average QBS and management scores, number of lists (410 rows)

# Note: QBS and management scores are provider/GP level variables, so here the variables measure clinic-level averages

### Read file
clinic_all = fread(file.path(project_path,  'Data', 'Raw', 'ECM Inclusion', "Clinics.csv"), encoding = 'UTF-8') %>% clean_names

### Clean...
clinic_all = clinic_all %>% rename('ecm_include_clinic' = 'ecm_status_clinic_categorical')

### Rename grouping column(s), specifying whether a given clinic was or wasn't participating in ECM
clinic_all = clinic_all %>% 
      mutate(ecm_include_clinic = ifelse(is.na(ecm_include_clinic), 'Not ECM', ecm_include_clinic),
             ecm_include_clinic = dplyr::recode(ecm_include_clinic,
                                    '1' = 'ECM', '2' = 'Not ECM', '3' = 'Not ECM'))

### Save
fwrite(clinic_all, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'clinic_all.csv'), na = NA, row.names = F)


#
# LISTS/GPs (ALL) --------------------------------------------------------------------------------------------------
#

### CREATE: List-level file with ECM status, GP code, provider-level QBS scores, and clinic data (766 row)

### Read file
list_all = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', 'GP_all.csv'), encoding = 'UTF-8'))


### Clean...
list_all = list_all %>% rename('clinic_registration_code' = 'clinic_registration_number', #... rename columns 
                               'ecm_include_list' = 'ecm_treatment_status',
                               'ecm_refuse_list' = 'refused') %>% 
  dplyr::select(clinic_registration_code, # ...drop and re-order columns
                list_id, gp_id, ecm_include_list, ecm_refuse_list)


### Add clinic-level data

# Merge
list_all = full_join(list_all, clinic_all)


### Rename grouping column(s)
list_all = list_all %>% 
  mutate(
    ecm_include_list = ifelse(is.na(ecm_include_list), 'Not ECM', ecm_include_list),
    ecm_include_list = dplyr::recode(ecm_include_list,
                                     '0' = 'Not ECM','1' = 'ECM'))


### Check if all list_id unique
list_all = list_all %>% group_by(list_id) %>% mutate(N=n()) %>% ungroup()
unique(list_all$list_id[list_all$N > 1])
# R: No -> the ones not matched to clinic are NA (9), plus N0835 and N0712 are found on the list twice

# N0835 -> leave the entry with clinic_registration_code = 12693863 (this is the right clinic code 
# if we consult 'treat_control_clinic_gp.csv' (OneDrive))

# N0712 -> leave the entry with clinic_registration_code = 14557344 (this is the right clinic code 
# if we consult 'treat_control_clinic_gp.csv' (OneDrive)) 

list_all = list_all %>% 
  filter(!(list_id == 'N0835' & clinic_registration_code == '14663359')) %>% 
  filter(!(list_id == 'N0712' & clinic_registration_code == '14557344')) %>% 
  dplyr::select(-c(N)) # Remove N column


### NOTE: Not saving right now, will do that when added ECM status (whether GP actually agreed to participate)

### Add participation status ----
### NOTE: Some providers initially agreed to participate in ECM but then, for various reasons, didn't end up doing so by the end of the study period

### Read file
list_ecm = clean_names(fread(file.path(project_path,  'Data', 'Raw', 'ECM Inclusion', "GP_final_participation_list.csv"), encoding = 'UTF-8'))


### NOTE: However, N0083 and N0184 are NOT CODED AS ECM anywhere else (see e.g. 'ecm_clinic_all_patients_nopii.csv' right below and in
### 'PATIENTS (ECM CLINICS)' section). For now I leave, them but shouldn't they be removed?

# patient_all_ecm = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', 'ecm_clinic_all_patients_nopii.csv'))) %>%
#   rename('list_id' = 'gp_id') %>% dplyr::select(c(list_id, ecm_patient_id)) %>% distinct()
# 
# sf(list_ecm$list_id[!(list_ecm$list_id %in% patient_all_ecm$list_id)])


### Create new column saying which lists actually included
list_all$ecm_status_list = ifelse(test = list_all$list_id %in% list_ecm$list_id, 
                                  yes = 'Participating', 
                                  no  = ifelse(is.na(list_all$ecm_include_list) | list_all$ecm_include_list == 'Not ECM', 'Not ECM', 'Not participating'))

list_all$ecm_include_list = ifelse(list_all$ecm_status_list == 'Participating', 'ECM', list_all$ecm_include_list)


### Add QBS (domain II) scores ----
qbs = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', 'QBS Need Adjustment Calculations.csv'), encoding = 'UTF-8'))
  
qbs[9, 'v76'] = 'qbs_II' # change cell value in row 9th of QBS score column (currently this rows holds proper column names, apart from this column)

qbs = qbs %>% 
  row_to_names(9) %>% # move selected row to column names
  rename( # rename columns
    'list_id' = 'Nimistu\nList',
    'clinic_registration_code' = 'Registrikood\nRegistration code'
  ) %>% 
  dplyr::select(c('clinic_registration_code', 'list_id', 'qbs_II')) %>% # select only necessary columns
  filter(grepl('^N', list_id)) %>%  # select only rows that start with 'N' on 'list_id'
  mutate(qbs_II = as.numeric(qbs_II)) # treat QBS scores as numeric


list_all = left_join(list_all, qbs %>% dplyr::select(c(list_id, qbs_II)))


### Save
fwrite(list_all, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'list_all.csv'), na = NA, row.names = F)


#
# PATIENTS (ECM EVALUATION) ----
#
### CREATE: Patient-level dataset with ECM inclusion (enrollment) status, classification, class code, list (6,865 rows)
### NOTE: This includes only patients in selected and accepting providers that were actually randomized into treatment and control groups

### Read files
patient_ecm1 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "ECM_randomization_results_1_bothID.csv"), encoding = 'UTF-8')) # ... main randomization outomce
patient_ecm2 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "ECM_randomization_results_2_bothID.csv"), encoding = 'UTF-8')) # ... additional patients randomized


### Rename and delete columns as necessary before joining
patient_ecm1 = patient_ecm1 %>%  dplyr::select(-c(v1, x, treatment_final, treatment, list_onhold)) # 'v1' and 'x' spare (as below); 'treatment' and 'treatment_final' all 1 and 'list_onhold' all NA, but all 3 missing from 'patient_ecm2'
patient_ecm2 = patient_ecm2 %>% dplyr::select(-c(v1, x))

patient_ecm = rbind(patient_ecm1, patient_ecm2)
rm(list = c('patient_ecm1', 'patient_ecm2'))


### Clean...
patient_ecm = patient_ecm %>% rename('id' = 'patient_i_dencrypted',  # ... rename columns
                                       'id_ecm' = 'ecm_patient_id',
                                       'gp_id' = 'doctor_code',
                                       'clinic_registration_code' = 'registration_clinic',
                                       'ecm_include_patient' = 'treat_ecm') %>% 
                    mutate( id = as.character(id), id_ecm = as.character(id_ecm)) %>%  #... treat as character
                    dplyr::select(c(list_id, id, id_ecm, # ...drop and re-order columns
                                    class_code, ecm_include_patient)) 


## Rename grouping column(s)
patient_ecm = patient_ecm %>% 
  mutate(ecm_include_patient = ifelse(is.na(ecm_include_patient), 'Not ECM', ecm_include_patient), 
         ecm_include_patient = dplyr::recode(ecm_include_patient,
                                             'Not Enrolled' = 'Control', 'Enrolled' = 'Treatment'))



### Save
fwrite(patient_ecm, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm.csv'), na = NA, row.names = F)



#
# PATIENTS (ECM ELIGIBLE) ----
#

### CREATE: Patient-level dataset listing all ECM-eligible patients, i.e. pure control, by clinic, list_id + with exclusion status (87,396 rows)

### Read data (from both May 2021 and November 2022)
### NOTE: Those are files listing all patients meeting ECM inclusion criteria at two different periods
patient_eligible_may21  = clean_names(fread(file.path(project_path, 'Data','Raw',  'ECM Inclusion', "ECM_eligible_may21.csv"), encoding = 'UTF-8'))
patient_eligible_nov22  = clean_names(fread(file.path(project_path, 'Data','Raw',  'ECM Inclusion', "ECM_eligible_nov22.csv"), encoding = 'UTF-8'))

### Cleaning... 
patient_eligible_may21 = patient_eligible_may21 %>%  
                    rename('list_id_may21' = 'nimistu', # ... rename columns
                          'list_id_new_may21' = 'uus_nimistu',
                          'id' = 'patsient',
                          'eligible_code_may21' = 'valjaarv_p_kood',
                          'inclusion_date_may21' = 'lisamise_kp',
                          'exclusion_date_may21' = 'valjaarv_kp'
                    ) %>% 
                    dplyr::select(c(id, list_id_may21, list_id_new_may21,  # ...drop and re-order columns
                                    eligible_code_may21, inclusion_date_may21, exclusion_date_may21)) %>%
                    mutate(eligible_patient = 'Eligible', #... create new 'eligible_patient' column
                           inclusion_date_may21 = dmy(inclusion_date_may21), # ...treat date column as dates
                           exclusion_date_may21 = dmy(exclusion_date_may21),
                           id = as.character(id)) #... treat as character
              
patient_eligible_nov22 = patient_eligible_nov22 %>%  
                    rename('list_id_nov22' = 'nimistu', # ... rename columns
                           'list_id_new_nov22' = 'uus_nimistu',
                           'id' = 'patsient',
                           'eligible_code_nov22' = 'valjaarv_p_kood',
                           'inclusion_date_nov22' = 'lisamise_kp',
                           'exclusion_date_nov22' = 'valjaarv_kp',
                           'comorbidities' = 'kaasuvaid_haiguseid'
                    ) %>% 
                    dplyr::select(c(id, list_id_nov22, list_id_new_nov22, eligible_code_nov22,  # ...drop and re-order columns
                                    inclusion_date_nov22, exclusion_date_nov22, comorbidities)) %>%
                    mutate(eligible_patient = 'Eligible', #... create new 'eligible_patient' column
                           inclusion_date_nov22 = dmy(inclusion_date_nov22),
                           exclusion_date_nov22 = dmy(exclusion_date_nov22),
                           id = as.character(id)) #... treat as character


### Update lists - if a patient has more than two lists, they always have entry on new list (as checked above)
### Ensure only newest list is kept by keeping only those rows that are empty on 'list_id_new' column
patient_eligible_may21 = patient_eligible_may21  %>% filter(list_id_new_may21 == '') %>% dplyr::select(-c(list_id_new_may21)) %>% distinct()
patient_eligible_nov22 = patient_eligible_nov22  %>% filter(list_id_new_nov22 == '') %>% dplyr::select(-c(list_id_new_nov22)) %>% distinct()


### Combine May 2021 and November 2022 by patient id
patient_eligible = inner_join(patient_eligible_nov22, 
                                patient_eligible_may21 %>% dplyr::select(c(id, contains('may21'))),
                              by = 'id') %>% 
                    dplyr::select(c(id, list_id_nov22, list_id_may21,  # ...drop and re-order columns
                                   eligible_code_nov22, eligible_code_may21, 
                                   inclusion_date_may21, inclusion_date_nov22,
                                   exclusion_date_may21, exclusion_date_nov22,
                                   eligible_patient, comorbidities))


### NOTE:  REMOVE exclusion code 'JVP92' appears to be assigned to ECM control patients and 'JVP99' to deceased patients -> nullify them? ----
### Deceased ones ONLY FOR MAY 2021 as we want their post-treatment outcomes
patient_eligible = patient_eligible %>% 
  mutate(eligible_code_may21 = ifelse(eligible_code_may21 %in% c('JVP92'), '', eligible_code_may21),
         eligible_code_nov22 = ifelse(eligible_code_nov22 %in% c('JVP92','JVP99'), '', eligible_code_nov22))

### NOTE: We will be guided by May 2021 list_id and exclusion codes
### However, we might also  want to know whether patient excluded at any date -> then check for each patient if they have any exclusion code 
### for EITHER May 2021 or November 2022 and if they do code them as '1' on new 'excluded' column, summarizing by 'id' and 'list_id_may21'

patient_eligible = patient_eligible %>% 
  mutate(excluded = ifelse(eligible_code_may21 != '' | eligible_code_nov22 != '', 1, 0)) %>% 
  dplyr::select(c(list_id_may21, id, comorbidities, excluded)) %>% 
  group_by(list_id_may21, id, comorbidities) %>% 
  fsum() %>% 
  ungroup() %>% 
  distinct() %>% 
  as.data.frame() %>% 
  mutate(excluded = ifelse(excluded > 0, 1, 0))



### Add 'id_ecm' column
patient_eligible = left_join(patient_eligible, id)

### Combine with 'patient_ecm', coding ECM inclusion at the patient level
patient_ecm_eligible = left_join(patient_eligible,
                                 patient_ecm %>% dplyr::select(-c(id_ecm)),
                                 by = 'id') %>% 
  mutate(ecm_include_patient = ifelse(is.na(ecm_include_patient), 'Pure control', ecm_include_patient))


# Assign May 2021 list_id to all rows currently missing it, keeping 'patient_ecm' coding where it is present
patient_ecm_eligible$list_id[is.na(patient_ecm_eligible$list_id)] = patient_ecm_eligible$list_id_may21[is.na(patient_ecm_eligible$list_id)]

### NOTE: REMOVE EXCLUDED ONES ----
patient_ecm_eligible = patient_ecm_eligible %>% filter(excluded == 0)

### NOTE: Recode ECM control and treatment to pure control for not participating providers ----
patient_ecm_eligible = patient_ecm_eligible %>% 
                          mutate(ecm_include_patient = ifelse(test = ecm_include_patient == 'Pure control',
                                                              yes  = ecm_include_patient,
                                                              no   = ifelse(list_id %in% list_ecm$list_id, 
                                                                            ecm_include_patient,
                                                                            'Pure control')))


### Add clinic randomization number (for fixed effects in regression)

# Combine, adding only block_categorical
patient_ecm_eligible = left_join(patient_ecm_eligible,
                 list_all %>% dplyr::select(c(list_id, block_categorical)))

### Not all list_id matched - check how many
table(is.na(patient_ecm_eligible$block_categorical))
### R: 4,334 patients without block id
tapply(patient_ecm_eligible$ecm_include_patient, is.na(patient_ecm_eligible$block_categorical), sf)
tapply(patient_ecm_eligible$list_id[is.na(patient_ecm_eligible$block_categorical)], patient_ecm_eligible$ecm_include_patient[is.na(patient_ecm_eligible$block_categorical)], unique)
### R: for ECM control and treatment, that only concerns one list (NO712)

### NOTE: Assign new block ID to all those missing it? -------------------------------------------------------------------------
patient_ecm_eligible$block_categorical[is.na(patient_ecm_eligible$block_categorical)] = max(patient_ecm_eligible$block_categorical, na.rm=T)+1


### Re-order columns
patient_ecm_eligible = patient_ecm_eligible %>% dplyr::select(c(list_id, id, id_ecm, ecm_include_patient, comorbidities, class_code, block_categorical))


### Save...
# ...whole dataset
fwrite(patient_ecm_eligible, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible.csv'), na = NA, row.names = F)

#
# END OF CODE -------------------------------------------------------------------
# 