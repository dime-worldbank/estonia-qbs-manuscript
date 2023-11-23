
#
# SET-UP ----
#

### Source the '00_global.R' script with required packages and functions
if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}


# Make a copy of the file
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)



# 
# PATIENT ID'S--------------------------------------------------------------------------------------------------
#
### CREATE: Dataset listing ID codes (encrypted + ECM) for all patients (1,605,595 rows)

### Read files
id1 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "Patient_ID_1.csv"), encoding = 'UTF-8'))
id2 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "Patient_ID_2.csv"), encoding = 'UTF-8'))

### Checks
# glimpse(id1)
# glimpse(id2)
# sf(unique(id1$patient_i_dencrypted) %in% unique(id2$patient_i_dencrypted)) # No overlap

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
### CREATE: Clinic-level file with ECM inclusion status, QBS and management scores, number of lists (410 rows)

### Read file
clinic_all = fread(file.path(project_path,  'Data', 'Raw', 'ECM Inclusion', "Clinics.csv"), encoding = 'UTF-8') %>% clean_names



### Clean...
clinic_all = clinic_all %>% rename('ecm_include_clinic' = 'ecm_status_clinic_categorical')

plot(clinic_all$average_clinic_qbs_score,
     clinic_all$management_score)
cor(clinic_all$average_clinic_qbs_score, 
    clinic_all$management_score)


### Checks
# sf(clinic_all$ecm_include_clinic) # 1 (included) - 93   |  2 (excluded)  -  282  | 3 (pilot)  -  13


### NOTE: Keeping all clinics (i.e. included and excluded from ECM) for now

### Rename grouping column(s)
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

# Check if clinic codes match between 'list_all' and 'clinic_all'
sf(unique(list_all$clinic_registration_code) %in% clinic_all$clinic_registration_code)
sf(unique(clinic_all$clinic_registration_code) %in% list_all$clinic_registration_code)
# Result (hereafter 'R:'): Only 401 out of 410 matches -> NOTE: Do we have larger clinic dataset?

# Merge
list_all = full_join(list_all, clinic_all)


# Check if ECM GPs and clinics overlap fully
table(list_all$ecm_include_clinic, list_all$ecm_include_list, useNA = 'ifany')
# R: Yes


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

### Read file
list_ecm = clean_names(fread(file.path(project_path,  'Data', 'Raw', 'ECM Inclusion', "GP_final_participation_list.csv"), encoding = 'UTF-8'))


### Check if participating lists match 'list_all' dataset
sf(list_ecm$list_id %in% unique(list_all$list_id))
# R: Yes, all 74 match

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

### Checks

sf(list_all$ecm_include_clinic)
sf(list_all$ecm_include_list)
sf(list_all$ecm_status_list)
table(list_all$ecm_include_list, list_all$ecm_status_list, useNA = 'ifany') # 0-0 = 402   0-1 = 0   1-0 = 72    1-1 = 72
list_all[list_all$ecm_status_list == 'Participating' & list_all$ecm_include_list != 'ECM',]
# R: Out of 74 lists coded as participating, 3 were not originally coded as ECM
# NOTE: Why might that be? FOR NOW re-code those participating as ECM

list_all$ecm_include_list = ifelse(list_all$ecm_status_list == 'Participating', 'ECM', list_all$ecm_include_list)


### Checks
tapply(list_all$clinic_registration_code, list_all$ecm_include_list, n_distinct)
tapply(list_all$clinic_registration_code, list_all$ecm_status_list, n_distinct)
tapply(list_all$clinic_registration_code,list_all$treatment_clinics, n_distinct)
tapply(clinic_all$clinic_registration_code,clinic_all$treatment_clinics, n_distinct)


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



# R: Two of the  lists participating in ECM according to 'list_ecm' aren't coded as such in 'list_all' (those are: "N0184" "N0083")
# Of those N0083 is missing clinic information, So, far I managed to find QBS score
# for those in 'Clinics_all.csv', but it doesn't seem to correspond to other QBS scores
# E.g. 'Clinics_all.csv' has different QBS for clinic code '10697098' (list_id = 'N0595' -
# the one that needed to be manually adjusted from 'NO597' in 'GP_final_participation_list.csv')

# Those lists ("N0184" "N0083") are also not coded as ECM in 'clinic_track.xlsx' (DropBox) or in 
# the patient classification in 'patient_classification_0517.xlsx  (DropBox) and are listed 
# as 'ECM treatment status' = 0 in 'ecm_gp_randomization.xlsx' (OneDrive; 'GP_all.csv' in 'Data/Raw/ECM Inclusion)
# and in 'ecm_gp_list.xlsx; (OneDrive); 'NO184' listed as not ECM and 'N0083' not listed at all in 'treat_control_gp.xlsx' (OneDrive)

# For now, although note to the 'GP_final_participation_list.csv',  says that this is the list of ECM GP's from October 2022 
# (so most up-to-date), I decided to remove both of those lists because they don't occur for ECM patients dataset ('patient_ecm' below) anyway

# However, alternatively we can keep N0184 and also add a row for N0083

### Add N0083 (from 'ecm_gp_randomization.csv')
# temp = clean_names(fread(file.path(project_path,  'Data', 'Raw', 'ECM Inclusion', "ecm_gp_randomization.csv"), encoding = 'UTF-8')) %>% 
#         filter(list_id == 'N0083') %>% 
#         dplyr::select(c(clinic_registration_number, list_id, gp_id)) %>% 
#         rename('clinic_registration_code' = 'clinic_registration_number')
# 
# list_all = rbindlist(list(list_all, temp), fill=T)
# list_all = as.data.frame(list_all)
# list_all[list_all$list_id == 'N0083', c('ecm_include_list', 'treatment_clinics', 'ecm_include_clinic')] = c(1,1,1)


### NOTE: REMOVE LISTS THAT ARE NOT RECODED AS ECM ANYWHERE ELSE
# list_ecm  = list_ecm %>% filter(!(list_id %in% c('N0083', 'N0184')))



#
#
# PATIENTS (ECM EVALUATION) ----
#
### CREATE: Patient-level dataset with ECM inclusion (enrollment) status, classification, class code, list (6,865 rows)

### Read files
patient_ecm1 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "ECM_randomization_results_1_bothID.csv"), encoding = 'UTF-8'))
patient_ecm2 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "ECM_randomization_results_2_bothID.csv"), encoding = 'UTF-8'))


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
                    # dplyr::select(-c(clinic_name, clinic_registration_code, gp_id,
                    #                  number_related_illness, miss_share, pid, count_strata, 
                    #                  total_eval_patients, patients_needed))


### NOTE: N00835 is missing in 'patient_ecm'
### N0836 is present in 'patient_ecm' (gp_id = D05351 [gp_name = Marianna Šikova], clinic_registration_number = 14663359 
### -> also coded with a 2nd clinic_registration_number = 11314753 in 'list_all') -> but e.g. in 'sampling-clinincs.xlsx' (OneDrive),
### and in 'treat_control_gp.xlsx' (OneDrive) N0836=14663359 only

### N0836 is also coded with patient classification in 'patient_classification_0517.xlsx' on DropBox


## Rename grouping column(s)
patient_ecm = patient_ecm %>% 
  mutate(ecm_include_patient = ifelse(is.na(ecm_include_patient), 'Not ECM', ecm_include_patient), 
         ecm_include_patient = dplyr::recode(ecm_include_patient,
                                             'Not Enrolled' = 'Control', 'Enrolled' = 'Treatment'))



### Save
fwrite(patient_ecm, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm.csv'), na = NA, row.names = F)


### Checks
# sf(patient_ecm$ecm_include_patient) # Enrolled - 2,414   Not Enrolled - 4,451
# tapply(patient_ecm$ecm_include_patient, patient_ecm$ecm_status_list, sf) # Enrolled - 1,784   Not Enrolled - 3,275 those with ecm_status_list == 1
# tapply(patient_ecm$list_id, patient_ecm$ecm_status_list, n_distinct) # 0 - 26    1 - 72
# list_all$list_id[list_all$ecm_status_list == 1] %in% patient_ecm$list_id %>% sf() # 72
# list_all$list_id[list_all$ecm_status_list == 1][!(list_all$list_id[list_all$ecm_status_list == 1] %in% patient_ecm$list_id)] # empty





#
# PATIENTS (ECM ELIGIBLE) ----
#

### CREATE: Patient-level dataset listing all ECM-eligible patients by clinic, list_id + with exclusion status (87,396 rows)

### Read data (from both May 2021 and November 2022)
patient_eligible_may21  = clean_names(fread(file.path(project_path, 'Data','Raw',  'ECM Inclusion', "ECM_eligible_may21.csv"), encoding = 'UTF-8'))
patient_eligible_nov22  = clean_names(fread(file.path(project_path, 'Data','Raw',  'ECM Inclusion', "ECM_eligible_nov22.csv"), encoding = 'UTF-8'))



### NOTE: Comorbidities are constant, so can take them from only one dataset (checked to be correct)


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


### Checks - those with new list entries - do they always have at least a second entry?
## with new code in 'list_id' and empty entry on the corresponding 'list_id_new'?
# id1 = unique(patient_eligible_may21$id[patient_eligible_may21$list_id_new_may21!=''])
# temp = patient_eligible_may21  %>% filter(id %in% id1) %>% group_by(id) %>% mutate(N=n())
# sf(temp$N)
# R: Yes


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


### Check which exclusion codes corresponds to deceases patients

# dta_deaths = read_parquet(file.path(project_path, 'Data', 'Clean', 'Deaths_all.parquet')) # Mind that this is a dataset created in further scripts
# temp = left_join(patient_eligible, dta_deaths %>% dplyr::select(c(id, death_date, death_treat_period)), by = 'id')
# table(temp$death_treat_period, temp$eligible_code_may21, useNA = 'ifany')
# table(temp$death_treat_period, temp$eligible_code_nov22, useNA = 'ifany')
# tapply(temp$death_date %>% is.na(), temp$eligible_code_may21, summary)
# tapply(temp$death_date %>% is.na(), temp$eligible_code_nov22, summary)


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


### Checks
# list_id match between 'patient_ecm' and 'patient_eligible'
sf(patient_ecm_eligible$list_id_may21 == patient_ecm_eligible$list_id)
which(patient_ecm_eligible$list_id_may21 != patient_ecm_eligible$list_id)
patient_ecm_eligible$id[34741]

# All match apart from one (id = '10642623') -> assign May 2021 list_id to all rows currently missing it,
# keeping 'patient_ecm' coding where it is present
patient_ecm_eligible$list_id[is.na(patient_ecm_eligible$list_id)] = patient_ecm_eligible$list_id_may21[is.na(patient_ecm_eligible$list_id)]



### Checks
# tapply(patient_ecm_eligible$excluded, patient_ecm_eligible$ecm_include_patient, sf)
# n_distinct(patient_ecm_eligible$id)
# sf(patient_ecm_eligible$ecm_include_patient)

# Any patients duplicated?
# temp = patient_ecm_eligible %>% ungroup() %>% group_by(id) %>% mutate(N=n())
# sf(temp$N)
# R: No



### NOTE: REMOVE EXCLUDED ONES ----
patient_ecm_eligible = patient_ecm_eligible %>% filter(excluded == 0)


### NOTE: Recode ECM control and treatment to pure control for not participating providers ----
patient_ecm_eligible = patient_ecm_eligible %>% 
                          mutate(ecm_include_patient = ifelse(test = ecm_include_patient == 'Pure control',
                                                              yes  = ecm_include_patient,
                                                              no   = ifelse(list_id %in% list_ecm$list_id, 
                                                                            ecm_include_patient,
                                                                            'Pure control')))


### Add clinic randomization number (for FE; Ben's suggestion on WhatsApp from 24/10/2023)

# Check if all lists from patient_ecm_eligible match list_all which has list_id and block_categorical
sf(patient_ecm_eligible$list_id %in% list_all$list_id)

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

# ... only ID's of relevant groups
fwrite(patient_ecm_eligible %>% filter(ecm_include_patient == 'Treatment') %>% dplyr::select(c(id)),
       file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'id_treatment.csv'), na = NA, row.names = F)

fwrite(patient_ecm_eligible %>% filter(ecm_include_patient == 'Control') %>% dplyr::select(c(id)),
       file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'id_control.csv'), na = NA, row.names = F)

fwrite(patient_ecm_eligible %>% filter(ecm_include_patient == 'Pure control') %>% dplyr::select(c(id)),
       file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'id_pure_control.csv'), na = NA, row.names = F)




#
# PATIENTS (ECM CLINICS) ----
#
### CREATE: Patient-level dataset listing all patients from ECM clinics by ID (136,924 rows)


### NOTE: ecm_patient_id.csv identical on all relevant counts to ecm_provider_patients.csv and to ecm_clinic_patient_list.csv, but fewer patients
### than ecm_clinic_patient_patients_nopii.csv (which also has the non-ECM version under non_ecm_list_patients_nopii.csv)

# patient_all_ecm = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'Other', 'ecm_patient_id.csv'))) %>% dplyr::select(c(list_id, ecm_patient_id)) %>% distinct()
### ecm_patient_id.csv -> fewer rows (171,315) against 176,736) + older (31/08/2021 against 19/12/2022)


patient_all_ecm = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', 'ecm_clinic_all_patients_nopii.csv'))) %>%
  rename('list_id' = 'gp_id') %>% dplyr::select(c(list_id, ecm_patient_id)) %>% distinct()

patient_all_non_ecm = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'Other', 'non_ecm_list_patients_nopii.csv')))  %>% dplyr::select(c(list_id, ecm_patient_id)) %>% distinct()


### Check if there is any overlap
sf(unique(patient_all_ecm$ecm_patient_id) %in% unique(patient_all_non_ecm$ecm_patient_id))
# R: Yes, 1,729 IDs overlap


### Check if IDs unique

# For ECM ones
# dim(patient_all_ecm)
# n_distinct(patient_all_ecm$ecm_patient_id)
# R: Yes for ECM patients


# For non-ECM ones
# dim(patient_all_non_ecm)
# n_distinct(patient_all_non_ecm$ecm_patient_id)
# temp = patient_all_non_ecm %>% group_by(ecm_patient_id) %>% mutate(N = n())
# sf(temp$N)
# n_distinct(temp$ecm_patient_id[temp$N>1])

# R: Not for non-ECM ones -> 409 patients have >1 list)_id


### Get all patients in participating GPs...
patient_all_ecm = full_join(patient_all_ecm %>% mutate(ecm_list = 'ECM'), # .. combine ECM and non-ECM datasets
                            patient_all_non_ecm %>% mutate(non_ecm_list = 'Not ECM'),
                            by = c('list_id', 'ecm_patient_id')) %>% 
  rename('id_ecm' = 'ecm_patient_id') %>%  # ... rename columns
  mutate(id_ecm = as.character(id_ecm)) %>%  # ... treat as character
  filter(list_id %in% unique(list_all$list_id[list_all$ecm_status_list == 'Participating'])) # ...NOTE: Leaving only those list_id that are in 'GP_final_participation_list.csv'


### NOTE: FOR NOW, assign ECM status to those currently NA's on 'ecm_list' column (those are  N0083 and N0184 lists of uncertain ECM status)
patient_all_ecm$ecm_list[patient_all_ecm$list_id %in% c('N0083', 'N0184')] = 'ECM'


### Add 'id' column
patient_all_ecm = left_join(patient_all_ecm, id)


### Remove duplicates 
patient_all_ecm = patient_all_ecm %>% group_by(id_ecm) %>% mutate(N = n())


### NOTE: For now, if there are duplicates (i.e. ID's coming from both ECM and Non-ECM lists, assume patient is from ECM list)
patient_all_ecm = patient_all_ecm %>% filter(N == 1 | N > 1 & ecm_list == 'ECM') %>% dplyr::select(-c(non_ecm_list, N))



### Checks
n_distinct(patient_all_ecm$list_id) # 74
n_distinct(patient_all_ecm$id_ecm) # 136,924  # NOTE: Double check that value
nrow(patient_all_ecm) == n_distinct(patient_all_ecm$id_ecm) # TRUE


### Save
fwrite(patient_all_ecm, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_all_ecm.csv'), na = NA, row.names = F)



#
# MERGE ALL ----
#
### CREATE: Patient-level dataset of IDs combining all the datasets and information above (220,717 rows)

### Checks
# sf(patient_ecm_eligible$id_ecm[patient_ecm_eligible$ecm_include_list == 1] %in% patient_all_ecm$id_ecm) # All 5,059 ECM patients found
# sf(patient_ecm_eligible$id_ecm[patient_ecm_eligible$ecm_status_list == 1] %in% patient_all_ecm$id_ecm) # All 5,059 ECM patients found

# All ECM patients ID's in 'patient_eligible'?
sf(patient_ecm$id %in% patient_eligible$id)
# R: Yes, all 6,865 present


### Combine - ID's only
patient_ecm_eligible_all = rbind(
  patient_eligible %>% dplyr::select(c(id, id_ecm)),
  patient_all_ecm %>% dplyr::select(c(id, id_ecm))
) %>% distinct()


### Save ----
fwrite(patient_ecm_eligible_all, na=NA, row.names = F,
       file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_ecm_eligible_all.csv"))




#
# END OF CODE ----
# 