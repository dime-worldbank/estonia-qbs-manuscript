


#
# SET-UP ----
# 

### Clean
rm(list=ls())
gc()
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


### NOTES ----------------------------------------------


### Move files --------------------------------------------------
### Move files to 'diagnoses' and 'procedures' folder automatically

# source_folder = 'OneDrive_1_15-8-2023 (2)'
# files1 = list.files(file.path(project_path, 'Data', 'Raw', source_folder))
# 
# 
# for(file1 in files1){
#   
#   if(grepl('_diag',file1)){
#     file.copy(file.path(project_path, 'Data', 'Raw', source_folder, file1),
#               file.path(project_path, 'Data', 'Raw', 'Diagnoses', file1))
#     file.remove(file.path(project_path, 'Data', 'Raw', source_folder, file1))
#   }
#   
#   if(grepl('_proced',file1)){
#     file.copy(file.path(project_path, 'Data', 'Raw', source_folder, file1),
#               file.path(project_path, 'Data', 'Raw', 'Procedures', file1))
#     file.remove(file.path(project_path, 'Data', 'Raw', source_folder, file1))
#   }
#   
# }


### For diagnosis codes check: http://icd9.chrisendres.com/ OR https://www.icd10data.com/search?s=T51.0
# diagnosis = fread(file.path(project_path, 'Data/Raw', 'Diagnosis_code_desc.csv'))


### Read ID's of ECM patients to keep  ---------------------------------------------------------
### NOTE: Only ECM eligible for now --------------------------------------------------------
patients =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_ecm_eligible.csv"))

patients_id = patients$id

patients_id

### CHOOSE: start date from which we want to see the outcomes  --------------------------------------------------
start_date1 = 20090101
end_date1   = 20230331



# billing = read_parquet(file.path(project_path, 'Data', 'Raw', 'Billing', 'outpatient_2009_2019_10.parquet'))
# table(billing$TypeOfTreatment)

#
# DATA CLEANING  ---------------------------------------------------------
# 


folder='Billing'



for(folder in c('Billing', 'Diagnoses', 'Procedures')){

    
  # Control loop 
  print(folder)
  Sys.sleep(1)
  start1 = Sys.time()

  # if(folder %in% c('Billing')){next}
  
    
  ### List all files in the relevant folder ---------------------------------------------------------------------------------------------
  files_list = list.files(file.path(project_path, 'Data', 'Raw', folder), full.names = T, pattern = '.csv')
  
  
  
  # Read and add all the files together in a loop ---------------------------------------------------------------------------------------------
  # file = 'C:/Users/ASUS/Documents/World_Bank/Locker/Estonia/Health/ECM (PAP)/Data/Raw/Diagnoses/outpatient_rehabilitation_diag_2019_10_new.csv'
  file = files_list[1]
  
  count = 0
  
  for(file in files_list){
    
    #  Control loop
    print(gsub('.*/', '', file))
    count = count+1
  
    ### Read file (.parquet if exists, if not read .csv + create .parquet) ---------------------------------------------------------------------------------------------
    if(file.exists(gsub('.csv', '.parquet', file))){
      temp = read_parquet(gsub('.csv', '.parquet', file))
    }else{
      temp = fread(file) %>% distinct()
      write_parquet(temp, gsub('.csv', '.parquet', file))
    }
    
    ### Cleaning... ---------------------------------------------------------------------------------------------
    names(temp) = names(temp) %>% tolower() # ... names to lowercase
    
    temp = temp %>% 
              dplyr::select(-any_of('v1')) %>% # ...remove spare columns (if they exist)
              mutate(billnr = as.character(billnr)) # ..treat bill id as character

    
    
    # Leave only patients and dates of interest (billing).... ---------------------------------------------------------------------------------------------
    if(folder == 'Billing'){
      
      temp = temp %>% 
        mutate(patientidencrypted = as.character(patientidencrypted)) %>% # ID as character
        filter(patientidencrypted %in% patients_id) %>%  # ID in 'patients', i.e. only leave those ECM eligible
        filter(startoftreatment >= start_date1) %>% # Treatment after pre-specified start date
        filter(startoftreatment <= end_date1) %>% # Treatment before pre-specified start date
        mutate(dataset = file %>%   # Determine which dataset that is
                          gsub("\\d.*", "", ., perl = TRUE) %>% 
                          gsub('.*/|care|_', '', .)) %>% 
        dplyr::select(c(billnr, dataset, typeoftreatment, patientidencrypted, gender, dateofbirth, residencecodesettlementlevel, # Leave only necessary columns
                        startoftreatment, endoftreatment, 
                        codeofadmissiontypeoradmitt, codeofdischargetype, dateofdeath, doctorid))
    
    }else{
      # ....or relevant billing codes (diagnoses, procedures) ---------------------------------------------------------------------------------------------
      if(!exists('billing')){billing = read_parquet(file.path(project_path, 'Data', 'Clean', 'Billing_all.parquet'))}
      
        temp = inner_join(temp, billing, by = 'billnr') 
      
    }
    
    
    ### Write clean file version ---------------------------------------------------------------------------------------------
    write_parquet(temp, gsub('.csv', '.parquet', gsub('Raw', 'Clean', file)))
    

    # Combine to the total ---------------------------------------------------------------------------------------------
    if(count == 1){
      dta = temp
    }else{
      dta = rbindlist(list(dta, temp))
    }
    
    # print(nrow(dta)/10^6)
    
  }
  
  ### Final filtering ---------------------------------------------------------------------------------------------
  dta = dta %>% 
      filter(startoftreatment >= start_date1) %>% filter(startoftreatment <= end_date1) %>%  # Ensure dates are in the pre-specified range
      filter(!is.na(billnr)) %>% filter(!is.na(patientidencrypted)) %>%  # Ensure there are no missing bill or patient ID's
      distinct() # Ensure observations are unique
  
  ### Save  ---------------------------------------------------------------------------------------------
  write_parquet(dta, file.path(project_path, 'Data', 'Clean', paste0(folder,'_all.parquet')))
  
  
  
  ### Extract patient level-demographics from billing dataset... ---------------------------------------------------------------------------------------------
  if(folder == 'Billing'){

    # dta = read_parquet(file.path(project_path, 'Data', 'Clean', 'Billing_all.parquet')) %>% mutate(patientidencrypted  = as.character(patientidencrypted ))
    
    patient_demo = dta %>% ungroup() %>% 
                    group_by(patientidencrypted, gender, dateofbirth) %>% # ...select relevant variables
                    mutate(N = n()) %>% ungroup() %>%  # ... county how often its combination occurs (will be needed to adjudicate between clerical entry errors)
                    dplyr::select(c(patientidencrypted, gender, dateofbirth, N)) %>%  # ... leave only relevant variables
                    distinct() %>%  #... in their unique combination
                    mutate(dateofbirth = ifelse(dateofbirth == 0, NA, dateofbirth), # ... replace as 0's and '' as NA's
                           gender = ifelse(gender == '', NA, gender))%>% 
      
                    group_by(patientidencrypted) %>% mutate(N2 = n()) %>%  #... for each patient, count how many times their ID occurs
                    filter(!(N2>1 & (is.na(gender) | is.na(dateofbirth)))) %>%  # ... for those with more than one occurrence, remove the entries with missing values
                    filter(N2 == 1 | (N2 > 1 & N == max(N))) %>%  # ... for the remaining non-unique occurences, select the combination of variables occuring more frequently
                    slice(1) %>% # ... for all the remaining non-unique occurences (fewer than 5 patients), randomly select the first of the demographic variables combinations
                    ungroup() %>%
                    mutate(gender = dplyr::recode(gender, 'N' = 'Female', 'M' = 'Male')) %>% # ... recode variables as required
                    rename('id' = 'patientidencrypted') %>% dplyr::select(-c(N, N2))# ... end by renaming and removing columns
    ### Checks
    # nrow(patient_demo)
    # n_distinct(patient_demo$patientidencrypted)
    # temp = patient_demo %>% group_by(patientidencrypted) %>% filter(n()>1)
    
    ### Save on its own
    write_parquet(patient_demo, file.path(project_path, 'Data', 'Clean', 'Patient_demo.parquet'))
    
    ### Save with 'patient_ecm_eligible.csv'
    patient_ecm_eligible = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible.csv')) %>% mutate(id = as.character(id))
    # patient_demo = read_parquet(file.path(project_path, 'Data', 'Clean', 'Patient_demo.parquet')) %>% mutate(id = as.character(id))
    patient_ecm_eligible_demo = left_join(patient_ecm_eligible, patient_demo)
    fwrite(patient_ecm_eligible_demo, file.path(project_path, 'Data', 'Clean',  'ECM Inclusion', 'patient_ecm_eligible_demo.csv'), na = NA, row.names = F)
    
  }
    

  # Control loop
  end1 = Sys.time()
  print(end1-start1)
  
}




#
# OUTPATIENT - PRIMARY 'SWITCH'  --------------------------------------------------
#

run = F

if(run){
  billing = read_parquet(file.path(project_path, 'Data', 'Clean', 'Billing_all.parquet'))
  dta_diagnosis = read_parquet(file.path(project_path, 'Data', 'Clean', 'Diagnoses_all.parquet'))
  dta_procedures = read_parquet(file.path(project_path, 'Data', 'Clean', 'Procedures_all.parquet'))
  
  
  billing$year = substr(billing$startoftreatment , 1, 4)
  dta_diagnosis$year = substr(dta_diagnosis$startoftreatment , 1, 4)
  dta_procedures$year = substr(dta_procedures$startoftreatment , 1, 4)
  
  dta_diagnosis %>% slice(1:10)
  dta_procedures %>% slice(1:10)
  
  
  ### By DoctorID
  # tapply(dta_diagnosis$year, paste(dta_diagnosis$doctorid, ' - ', dta_diagnosis$dataset), table)
  ### R: For those doing both primary and outpatient, seem to confirm suddent 'switch' in visits'
  
  ### Admission type?
  # options(max.print = 2000)
  # table(billing$dataset[billing$dataset %in% c('primaryhealth', 'outpatient')], 
  #       billing$codeofadmissiontypeoradmitt[billing$dataset %in% c('primaryhealth', 'outpatient')],
  #       billing$year[billing$dataset %in% c('primaryhealth', 'outpatient')])
  ### R: All appear to be very pre-dominantly admitted on E-T0002 (came by myself), but outpatient 
  ### also has non-neglible number on a few other categories, like T0011 (family physician)
  
  
  ### Billing nr pattern?
  temp = cbind(billing %>% filter(dataset == 'outpatient') %>% slice(1:200) %>% dplyr::select(c(billnr)),
               billing %>% filter(dataset == 'primaryhealth') %>% slice(1:200) %>% dplyr::select(c(billnr)))
  
  
  billing$billnr2 = substr(billing$billnr, 1, 2)
  billing$billnr9 = substr(billing$billnr, 9, 10)
  
  table(billing$billnr9)
  
  
  
  
  ### R: Doesn't seem to work in any discernible pattern
  
  
  
  ### What if we collapse the two?
  ### R: seems like a smooth trend now...
  # billing$dataset2 = ifelse(billing$dataset == 'primaryhealth', 'outpatient', billing$dataset)
  # 
  # table(billing$dataset2, billing$typeoftreatment, billing$year)
  # 
  # temp = data.frame(year =seq(2009:2022), 
  #                   n_outpatient_and_primary = c(1023013, 1055909, 1131936, 1158101, 1201738, 1238913, 1300978,
  #                                                1326691, 1358876, 1362081, 1388164, 1346204, 1560172, 1402712))
  # 
  # ggplot(temp, aes(year, n_outpatient_and_primary))+
  #   geom_point()+
  #   geom_line()+
  #   scale_y_continuous(limits = c(0, max(temp$n_outpatient_and_primary))) +
  #   scale_x_continuous(breaks = seq(2009:2022)) +
  #   theme_bw()
  
  
  
  
  
  ### By code of diagnosis/procedure that is most common?
  
  # Define how many most common categories you want
  N = 100
  
  # Define N most common procedures by dataset and year
  temp = dta_procedures %>% 
    filter(dataset %in% c('outpatient', 'primaryhealth', 'inpatient')) %>% 
    group_by(dataset, year) %>% 
    count(procedure) %>% 
    arrange(dataset, year, desc(n)) %>%
    slice(1:N) %>% 
    ungroup()
  
  
  ### Add names to the codes of procedures
  cost_procedures_raw = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'Other', "costs_procedures.csv"), encoding = 'UTF-8'))
  # cost_procedures_raw %>% dplyr::select(c(hkkood, sap_hkkood, nimetus)) %>% distinct() %>% View
  
  temp = left_join(temp, cost_procedures_raw %>% dplyr::select(c(sap_hkkood, nimetus)) %>% rename('procedure' = 'sap_hkkood')) %>% distinct()
  
  # English translations of the names (from ChatGPT)
  names <- c(
    "Perearsti telefoni teel  toimunud ja patsiendi tervisekaardis dokumenteeritud patsiendinõustamine",
    "Pereõe telefoni teel toimunud ja patsiendi tervisekaardis dokumenteeritud patsiendinõustamine",
    "Perearsti esmane vastuvõtt",
    "Kaugvastuvõtt",
    "Pereõe nõustav vastuvõtt",
    "Perearsti profülaktiline vastuvõtt",
    "Biomarkeri (patogeeni, geneetilise või somaatilise mutatsiooni) määramine real-time-PCR-meetodil",
    "Eriarsti kaugvastuvõtt",
    "Kroonilise haige nõustamine",
    "Perearsti elektronposti teel toimunud ja patsiendi tervisekaardis dokumenteeritud patsiendinõustamine",
    "COVID-19 test",
    "Muu haiguse vastu vaktsineerimine",
    "Pereõe elektronposti teel toimunud ja patsiendi tervisekaardis dokumenteeritud patsiendinõustamine",
    "Silma eesosa optiline koherentne tomograafia",
    "Rohelise ja sinise triaažikategooriaga patsiendi käsitlus valvetoas",
    "Ambulatoorne pikaajaline hapnikravi kopsuhaigele (ööpäevahind)",
    "Kodune hapnikravi",
    "Teised/kõik ülejäänud inimesed (COVID-19 vaktsineerimine)",
    "Vaktsineerimine SARS-CoV-2 viirusest põhjustatud COVID-19 haiguse vastu",
    "Vaktsineerimine SARS-CoV-2 viirusest põhjustatud COVID-19 haiguse vastu nädalavahetusel ja riigipühadel",
    "Elektrokardiograafia, pildipangas arhiveeritud",
    "SARS koroonaviirus-2 RNA määramine",
    "SARS-koroonaviirus-2 RNA määramine",
    "Keeldumine vaktsineerimisest SARS-CoV-2 viirusest põhjustatud COVID-19 haiguse vastu",
    "Riskirühma kuuluvad inimesed (COVID-19 vaktsineerimine)",
    "Elektrokardiograafia",
    "SARS koroonaviirus-2 RNA määramine kiirmeetodil",
    "SARS-koroonaviirus-2 RNA määramine kiirmeetodil",
    "Psühhiaatri kaugvastuvõtt aktiivravi perioodis",
    "Ülejäänud ETO- d  (COVID-19 vaktsineerimine)",
    "Kloriid, liitium, laktaat, ammoonium*",
    "Psühhiaatri kaugvastuvõtt toetusravi perioodis",
    "EKG monitooring 24 tundi",
    "Prostataspetsiifiline antigeeni määramine  ",
    "Prostataspetsiifilise antigeeni määramine",
    "Vedelikupõhine günekotsütoloogiline uuring (LBC)",
    "Õe iseseisev kaugvastuvõtt",
    "Õe kaugvastuvõtt",
    "Mikroorganismi samastamine biokeemilise või immunoloogilise spetsiaalse/automatiseeritud süsteemi abil"
  )
  
  translations <- c(
    "General practitioner's patient counseling conducted by phone and documented in the patient's health record",
    "Nurse's patient counseling conducted by phone and documented in the patient's health record",
    "Primary appointment with a general practitioner",
    "Telemedicine appointment",
    "Nurse's advisory appointment",
    "General practitioner's preventive appointment",
    "Determination of a biomarker (pathogen, genetic, or somatic mutation) by real-time PCR method",
    "Specialist's telemedicine appointment",
    "Counseling for chronic patients",
    "Patient counseling conducted by the general practitioner via email and documented in the patient's health record",
    "COVID-19 test",
    "Vaccination against other diseases",
    "Patient counseling conducted by the nurse via email and documented in the patient's health record",
    "Optical coherence tomography of the anterior segment of the eye",
    "Handling of patients with green and blue triage categories in the emergency room",
    "Outpatient long-term oxygen therapy for lung patients (daily rate)",
    "Home oxygen therapy",
    "Vaccination for others/all remaining people (COVID-19 vaccination)",
    "Vaccination against COVID-19 caused by the SARS-CoV-2 virus",
    "Vaccination against COVID-19 caused by the SARS-CoV-2 virus on weekends and holidays",
    "Electrocardiography, archived in the image bank",
    "Determination of SARS coronavirus-2 RNA",
    "Determination of SARS coronavirus-2 RNA",
    "Refusal of vaccination against COVID-19 caused by the SARS-CoV-2 virus",
    "Vaccination for high-risk group individuals (COVID-19 vaccination)",
    "Electrocardiography",
    "Determination of SARS coronavirus-2 RNA by rapid method",
    "Determination of SARS coronavirus-2 RNA by rapid method",
    "Psychiatrist's telemedicine appointment during active treatment period",
    "Others ETO- d (COVID-19 vaccination)",
    "Chloride, lithium, lactate, ammonium*",
    "Psychiatrist's telemedicine appointment during support treatment period",
    "24-hour EKG monitoring",
    "Measurement of prostate-specific antigen",
    "Measurement of prostate-specific antigen",
    "Liquid-based cytology examination (LBC)",
    "Nurse's independent telemedicine appointment",
    "Nurse's telemedicine appointment",
    "Identification of microorganisms by biochemical or immunological specific/automated system"
  )
  
  
  # Create dictionary and add English translation to the dataframe
  df_dictionary <- data.frame(nimetus = names, english = translations)
  temp = left_join(temp, df_dictionary)
  
  
  
  
  ### Find those that start occuring commonly in outpatient data only after 2019
  post_2019 = temp %>%
    filter(dataset == 'outpatient') %>% 
    group_by(procedure) %>%
    filter(all(year >= 2020)) %>%
    ungroup() 
  
  
  # Leave only ids of procedures
  post_2019_codes = post_2019 %>% dplyr::select(c(procedure)) %>% distinct()
  
  # Plot
  g1=ggplot(temp %>% 
              filter(procedure %in% post_2019_codes$procedure) %>%
              filter(!is.na(english)) %>% 
              filter(dataset %in% c('primaryhealth', 'outpatient')), 
            aes(year, english,
                fill = N>0))+
    geom_tile(col = 'black')+
    geom_vline(aes(xintercept = 11.5), linetype = 'solid', color = 'black', linewidth = .5) +
    facet_wrap(~dataset)+
    scale_fill_manual(values = 'grey60') +
    coord_fixed(ratio=1)+
    guides(fill='none') +
    labs(
      title = paste0('<b>Which out of the ',N,' most common<br>outpatient procedures
                   in each year, only<br>make it to TOP100 in or after 2020?</b><br>'),
      x = '', y = '',
      caption = '<br><br><b>Notes:</b> Grey squares indicate that a given procedure
    was among 100 most common ones in a given datset and year. Only the procedures
    that meet this criterion in or after 2020 are shown. </b><br><br>',
    )+
    theme(
      axis.text.x = element_text(size = 8, face = 'plain', angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 8, face = 'bold'),
      plot.title = element_markdown(size = 20, hjust = .5)
    )
  
  ggsave(file.path(project_path, 'Figures', 'procedures_2020.png'),
         plot = g1, width = 33, height = 25, unit = 'cm')
  
  
  
  
  
  temp2 <- temp %>%
    filter(dataset == 'outpatient') %>% 
    group_by(procedure) %>%
    filter(all(year >= 2019)) %>%
    ungroup()
  
  
  temp2 = left_join(temp2, 
                    cost_procedures_raw  %>% dplyr::select(c(hkkood, sap_hkkood, nimetus)) %>% rename('procedure' = 'sap_hkkood'))
  
  for(i in 1:nrow(temp2)){
    cat(paste0(temp2$nimetus[i], '\n'))
  }
  
  
  
  temp3 = dta_procedures %>% filter(grepl('9001$|9002$|9062$', procedure))
  temp3 = dta_procedures %>% filter(procedure %in% post_2019$procedure)
  
  table(temp3$dataset, temp3$year)
  # ABOVE: SUBSET OF 100 MOST COMMON CATEGORIES THAT BECOME MOST COMMON IN OUTPATIENT DATA ONLY IN 2020 OR LATER
  # BELOW: WHOLE DATASET
  table(dta_procedures$dataset, dta_procedures$year)
  
  2
  ndis
  
  
  
  g2=rbind(data.frame(year = 2009:2023, dataset = "out", value = c(1354329, 1470700, 1594440, 1590517, 1646182, 1708816, 1817679, 1821410, 2031313, 2070985, 2152386, 2879282, 3152424, 3109370, 676900)),
           data.frame(year = 2009:2023, dataset = "primary", value = c(1590078, 1626045, 1779215, 1885681, 2139495, 2219369, 2387096, 2558269, 2587694, 2786442, 2850366, 1906265, 2453600, 2198706, 466099))) %>% 
    ggplot(aes(year, value, group = dataset, col = dataset)) +
    geom_line() +
    scale_x_continuous(breaks = seq(2009,2023,1))+
    theme(
      axis.text.x = element_text(size = 12, face = 'plain', angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 12, face = 'bold'),
    )
  
  ggsave(file.path(project_path, 'Figures', 'out_primary_trend.png'),
         plot = g2, width = 35, height = 20, unit = 'cm')
  
  
  ### Chi2 test of most common categories
  ### R: Discriminates just as well (with <<1,000 most common) or just as bad (>>1,000) between primary and outpatient like between inpatient and outpatient
  
  # View(dta_diagnosis %>% filter(codeofdiagnos == 'H52.1'))
  
  v1 = dta_diagnosis$codeofdiagnos[dta_diagnosis$dataset == 'outpatient']
  v2 = dta_diagnosis$codeofdiagnos[dta_diagnosis$dataset == 'primaryhealth']
  v3 = dta_diagnosis$codeofdiagnos[dta_diagnosis$dataset == 'inpatient']
  
  table1 <- table(v1)
  table2 <- table(v2)
  table3 <- table(v3)
  
  
  temp = full_join(data.frame(table1) %>% rename('var' = 'v1', 'out' = 'Freq'),
                   data.frame(table2) %>% rename('var' = 'v2', 'primary' = 'Freq'))
  
  temp = temp %>% arrange(desc(out)) %>% slice(1:1000)
  
  chisq.test(temp$out, temp$primary)
  
  Lambda(temp$out, temp$primary)
  
  
  # Month as date (in billing and in procedures (dateofprocedure))
  # Exclude spare columns from diagnosis and procedures
  # Treatment period
  
  
  # Sample data
  codes <- unique(billing$billnr)
  
  # Initialize a matrix to store the counts
  count_matrix <- matrix(0, nrow = 10, ncol = 10)
  
  # Loop through the codes and count the occurrences
  for (code in codes) {
    digits <- unlist(strsplit(code, ""))
    for (i in 1:10) {
      count_matrix[i, as.numeric(digits[i])] <- count_matrix[i, as.numeric(digits[i])] + 1
    }
  }
  
  # Visualize the data using ggplot2
  library(ggplot2)
  
  count_data <- data.frame(Place = 1:10, Digit = 0:9, Count = count_matrix)
  
  ggplot(count_data, aes(x = Place, y = Digit, fill = Count)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(title = "Digit Counts at Each Place")
  
}


#
# END OF CODE  --------------------------------------------------
#