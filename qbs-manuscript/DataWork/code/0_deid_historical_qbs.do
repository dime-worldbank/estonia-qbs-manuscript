 
 // Clean and de-identify QBS historical data
 
 use "${data}/qbs_historical_appended.dta", clear
 
 keep year doctor_name qbs_points
 
 bysort doctor_name : gen doc1 = 1 if _n == 1 
 replace doc1 = sum(doc1)
 replace doc1 = . if missing(doctor_name )
 
 ren doc1 doctor_id_nopii
 
 drop doctor_name
 
 label variable year "Year"
 label variable doctor_id_nopii "Doctor ID anonymized"
 label variable qbs_points "QBS score"
 
 save "${constructed}/qbs_historical_appended_nopii.dta", replace
 
 // Clean and de-identify sampling list data
 
 
 use "${data}/sampling-lists-mergedrisk.dta", clear
 
 keep Numberofrelatedillnesses list_age clinic_id
 
 ren Numberofrelatedillnesses number_chronic_illness
 
  bysort clinic_id : gen c1 = 1 if _n == 1 
 replace c1 = sum(c1)
 replace c1 = . if missing(clinic_id)
 
 ren c1 clinic_id_nopii
 
 label variable clinic_id "Clinic ID anonymized"
 label variable number_chronic_illness "Number of chronic illness"
 label variable list_age "Average age of patient"
 
 
 save "${constructed}/sampling-lists-mergedrisk-nopii.dta", clear

