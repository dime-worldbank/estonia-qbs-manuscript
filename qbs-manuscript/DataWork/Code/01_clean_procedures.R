
### Source the '00_global.R' script with required packages and functions

if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path('~/World_Bank/Locker/Estonia/Health/Billing/R Scripts/Clean/00_global.R'))
}


### This code summarized procedures cost for all patients included in the ECM evaluation


#
# COSTS PROCEDURES ----
#

### Read data
cost_procedures = clean_names(fread(file.path(project_path, 'Data/Raw', "Costs_procedures.csv"), encoding = 'Latin-1'))


### Re-name
names(cost_procedures)[c(6,7)] <- c('from', 'until')

### Leave only required columns
cost_procedures = cost_procedures[,1:11]

### Adjust type of columns where necessary
cost_procedures$price = as.numeric(gsub(',', '', cost_procedures$price))
cost_procedures = cost_procedures %>% mutate(across(c(from, until), dmy))


#
# COSTS PROCEDURES PER PATIENT ----
#

### RUN ANEW?
### If 'Yes', then the cost will be merged with inpatient data from the scratch
### The process is commented out because it takes min. 4-5 minutes to run

anew = 'No' 

if(anew == 'Yes'){

  # Load inpatient dataset
  inpatient1 <- fread(file.path(raw_2021, "inpatient_care_2019_2020_2021.csv"), encoding = 'UTF-8')
  inpatient2 <- fread(file.path(raw_2022, "inpatient_care_2022.csv"), encoding = 'UTF-8')
  inpatient3 <- fread(file.path(raw_2022_2, "inpatient_care_2021_2022.csv"), encoding = 'UTF-8')
  inpatient4 <- fread(file.path(raw_2022_nov, "inpatient_care_2022.csv"), encoding = 'UTF-8')
  
  inpatient <- rbind(inpatient1, inpatient2, inpatient3, inpatient4)
  
  
  # Load all the procedures datasets
  
  inpatient_procedures2 <- fread(file.path(raw_2022, "inpatient_care_procedures_2022.csv"), encoding = 'Latin-1')
  inpatient_procedures3 <- fread(file.path(raw_2022_2, "inpatient_care_procedures_2021_2022.csv"), encoding = 'UTF-8')
  inpatient_procedures4 <- fread(file.path(raw_2022_nov, "inpatient_care_procedures_2022.csv"), encoding = 'UTF-8')
  
  inpatient_procedures <- rbind(inpatient_procedures2, inpatient_procedures3, inpatient_procedures4)
  
  
  # Merge bills to procedures
  inpatient_merge_procedures <- left_join(inpatient, inpatient_procedures, by = "BillNr")
  
  ### Restrict to patients from August 2021 onwards
  inpatient_merge_procedures$year <- substring(inpatient_merge_procedures$StartOfTreatment,1,4)
  table(inpatient_merge_procedures$year, useNA = "ifany")/1000
  
  inpatient_merge_procedures$month <- substring(inpatient_merge_procedures$StartOfTreatment,5,6)
  table(inpatient_merge_procedures$month, useNA = "ifany")/1000
  
  #inpatient_merge_procedures <- subset(inpatient_merge_procedures, inpatient_merge_procedures$StartOfTreatment >= 20210731 )
  inpatient_merge_procedures$treat_period <- ifelse(inpatient_merge_procedures$StartOfTreatment > 20210731, '1', '0')
  
  
  ### Merge with anonymized id with treatment status
  treatment_status <- read.csv(file.path(data, "ecm_patient_randomization.csv"))
  hospitalization_procedures <- left_join(treatment_status, inpatient_merge_procedures, by = "PatientIDencrypted")
  
  ### Create empty price_procedure column
  hospitalization_procedures$price_procedure = NA
  
  #### For each patient
  for(i in 1:nrow(hospitalization_procedures)){
    #if(i %% 500 == 0){ print(i) } # Controls the status of the loop (old)
    progress(i, nrow(hospitalization)) # Controls the status of the loop
    
    #  Add price by matching code and treatment period
    price1 = cost_procedures$price[which(cost_procedures$sap_hkkood == hospitalization_procedures$Procedure[i] &
                                            cost_procedures$from < ymd(hospitalization_procedures$StartOfTreatment[i]) &
                                            cost_procedures$until > ymd(hospitalization_procedures$StartOfTreatment[i]))]
    
    # If none foundm then add the price for the most recent available period
    if(length(price1)==0){
      price1 = cost_procedures$price[which(cost_procedures$sap_hkkood == hospitalization_procedures$Procedure[i] &
                                              cost_procedures$from == max(cost_procedures$from[cost_procedures$sap_hkkood == hospitalization_procedures$Procedure[i]]))]
    }
    # If still none found, then assign NA value
    if(length(price1)==0){
      price1 = NA
    }
    
    # Assign
    hospitalization_procedures$price_procedure[i] = price1
  }
  
  ### Optionally saved file now, so that if any changes are required in the code lines below, the loop above 
  ### doesn't need to be re-run
  # write.csv(hospitalization_procedures, file.path(data, "patient_avoidable_hosp_procedures (temp).csv"), row.names = F)
  
  # hospitalization_procedures = read.csv( file.path(data, "patient_avoidable_hosp_procedures (temp).csv"))
  
  ### Some procedures than more than once on the same bill, so multiply the price accordingly
  # (strange thing is that some of the 'NrOfTimes' values are >100, whereas other fraction of 1...)
  hospitalization_procedures$price_procedure_times = hospitalization_procedures$price_procedure * hospitalization_procedures$NrOfTimes
  
  ### Leave only required columns
  hospitalization_procedures = hospitalization_procedures %>% 
    dplyr::select(c(BillNr, PatientIDencrypted, treat_period, price_procedure_times ))
  
  ### Summarize by patient (and period, in case we want to do pre/post-treatment comparison of treatment groups)
  hospitalization_procedures = hospitalization_procedures %>% group_by(PatientIDencrypted, treat_period) %>% 
    summarise(across(c(price_procedure_times), list(mean = mean_miss, sum = sum_miss),  .names = "{.col}_{.fn}"))
  
  ### Assign '-1' to the patients not admitted to hospital at all during the intervention period
  ### and therefore missing the 'treat_period' variable
  hospitalization_procedures$treat_period[is.na(hospitalization_procedures$treat_period)]= '-1'
  
  ### Save
  write.csv(hospitalization_procedures, file.path(data, "patient_procedures_costs.csv"), row.names = F)
  
  }



#################################################################################################################
### ACTA EST FABULA
#################################################################################################################
