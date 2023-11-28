# Required Libraries
library(data.table)
library(arrow)

# Function to write log messages
write_log <- function(message, log_file = "discrepancy_report.log") {
  write(paste(Sys.time(), " - ", message, sep = ""), file = log_file, append = TRUE)
}

# Initialize the log file
write_log("Starting the data cleaning process.")

# Function to write discrepancy reports to CSV
write_discrepancy_report <- function(data, step, file_name = "discrepancy_report.csv") {
  report <- data.table(
    step = step,
    observations = nrow(data),
    variables = length(names(data))
  )
  fwrite(report, file_name, row.names = FALSE, append = TRUE)
}

# SET-UP ----

### Source the '00_global.R' script with required packages and functions
if (Sys.info()[["user"]] == "wb539995") {
  # source('~/path/to/r_script/00_global.R'
} else if (Sys.info()[["user"]] == "jonas") {
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}

### Read ID's of ECM patients to keep
patients <- fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', "patient_ecm_eligible.csv"))

patients_id <- patients$id

### CHOOSE: start date from which we want to see the outcomes
start_date1 <- 20090101
end_date1 <- 20230331

# Initial data logging
write_log("Initial data read.")
write_discrepancy_report(patients, "Initial Data")

#
# DATA CLEANING  ---------------------------------------------------------

folder <- 'Diagnoses'

for (folder in c('Billing', 'Diagnoses', 'Procedures')) {
  
  # Control loop 
  print(folder)
  
  ### List all files in the relevant folder
  files_list <- list.files(file.path(project_path, 'Data', 'Raw', folder), full.names = TRUE, pattern = '\\.parquet$')
  
  temp_list <- vector("list", length(files_list))
  for (i in seq_along(files_list)) {
    file <- files_list[i]
    
    # Control loop
    print(gsub('.*/', '', file))
    
    ### Read file (.parquet if exists, if not read .csv + create .parquet)
    if (file.exists(gsub('.csv', '.parquet', file))) {
      temp <- read_parquet(gsub('.csv', '.parquet', file))
    } else {
      temp <- fread(file)
      write_parquet(temp, gsub('.csv', '.parquet', file))
    }
    setDT(temp)
    
    ### Cleaning... 
    setnames(temp, tolower(names(temp))) # ... names to lowercase
    temp[, c('v1') := NULL] # ...remove spare columns (if they exist)
    temp[, billnr := as.character(billnr)] # ..treat bill id as character
    
    # Logging after initial file read and clean
    write_log(paste("File read and initial cleaning completed for file: ", gsub('.*/', '', file)))
    write_discrepancy_report(temp, paste("File Read and Clean - ", gsub('.*/', '', file)))
    
    # Leave only patients and dates of interest (billing)....
    if (folder == 'Billing') {
      temp[, `:=`(
        patientidencrypted = as.character(patientidencrypted), # ID as character
        dataset = gsub("\\d.*", "", file, perl = TRUE) %>% gsub('.*/|care|_', '', .)
      )]
      temp <- temp[patientidencrypted %in% patients_id & startoftreatment >= start_date1 & startoftreatment <= end_date1]
      
      # Logging after filtering billing data
      write_log(paste("Billing data filtered for file: ", gsub('.*/', '', file)))
      write_discrepancy_report(temp, paste("Billing Data Filtered - ", gsub('.*/', '', file)))
    } else {
      # ....or relevant billing codes (diagnoses, procedures) 
      if (!exists('billing')) {
        billing <- read_parquet(file.path(project_path, 'Data', 'Clean', 'Billing_all.parquet'))
        
      }
      
      temp <- billing[temp, on = .(billnr = billnr)]
      
      # Logging after joining with billing data
      write_log(paste("Data joined with billing for file: ", gsub('.*/', '', file)))
      write_discrepancy_report(temp, paste("Data Joined with Billing - ", gsub('.*/', '', file)))
    }
    
    ### Write clean file version
    write_parquet(temp, gsub('.csv', '.parquet', gsub('Raw', 'Clean', file)))
    
    # Combine to the total
    temp_list[[i]] <- temp
  }
  
  # Combine all data.tables
  dta <- rbindlist(temp_list)
  
  ### Final filtering
  dta <- dta[startoftreatment >= start_date1 & startoftreatment <= end_date1 & !is.na(billnr) & !is.na(patientidencrypted), unique(.SD)]
  
  # Logging after final filtering
  write_log(paste("Final filtering completed for folder: ", folder))
  write_discrepancy_report(dta, paste("Final Filtered Data - ", folder))
  
  ### Save
  write_parquet(dta, file.path(project_path, 'Data', 'Clean', paste0(folder, '_all.parquet')))
  
  ### Extract patient level-demographics from billing dataset...
  if (folder == 'Billing') {
    patient_demo <- dta[, .(N = .N), by = .(patientidencrypted, gender, dateofbirth)]
    patient_demo <- patient_demo[!is.na(gender) & !is.na(dateofbirth)]
    patient_demo <- patient_demo[, .SD[which.max(N)], by = .(patientidencrypted)]
    setnames(patient_demo, c("patientidencrypted", "gender", "dateofbirth", "N"), c("id", "gender", "dateofbirth", "N"))
    patient_demo[, gender := fcase(
      gender == 'N', 'Female',
      gender == 'M', 'Male',
      default = NA_character_  # or use 'Unknown' or keep it as 'NA_character_' for unspecified
    )]
    
    # Logging after extracting patient demographics
    write_log("Patient demographics extracted from billing dataset.")
    write_discrepancy_report(patient_demo, "Patient Demographics")
    
    # Save with 'patient_ecm_eligible.csv'
    patient_ecm_eligible <- fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible.csv'))
    patient_ecm_eligible[, id := as.character(id)]
    patient_ecm_eligible_demo <- patient_ecm_eligible[patient_demo, on = .(id = id)]
    fwrite(patient_ecm_eligible_demo, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_eligible_demo.csv'), na = "NA", row.names = FALSE)
    rm(temp)
    gc()
    print(gc())
  }
  print(gc())
}
