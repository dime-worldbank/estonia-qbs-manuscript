// Load and clean the QBS data
use "${data}/domainii_qbs.dta", clear
  isid list, sort
  set seed 428252	// Timestamp: 2020-11-05 20:13:06 UTC
  gen rand = rnormal()
  sort rand
  gen uid = "QBS" + strofreal(_n,"%03.0f")
    
// Rename the variables
  ren targetgroup_diab_monitoring tgtgroup_diab_monitor
  ren covered_diab_monitoring     covered_diab_monitor
  ren qbs_diab_monitoring         qbs_diab_monitor
  ren qbs_diab_medication         qbs_diab_treat
  ren coveragert_diab_monitoring  coveragert_diab_monitor

// Keep variables of interest
  keep uid tgtgroup_diab_monitor          ///
    covered_diab_monitor coveragert_diab_monitor qbs_diab_monitor          ///
    tgtgroup_diab_treat covered_diab_treat coveragert_diab_treat           ///
    tgtgroup_hyp1_monitor covered_hyp1_monitor coveragert_hyp1_monitor     ///
    qbs_hyp1_monitor tgtgroup_hyp2_monitor covered_hyp2_monitor            ///
    coveragert_hyp2_monitor qbs_hyp2_monitor tgtgroup_hyp3_monitor         ///
    covered_hyp3_monitor coveragert_hyp3_monitor qbs_hyp3_monitor          ///
    tgtgroup_hyp1_treat covered_hyp1_treat coveragert_hyp1_treat           ///
    qbs_hyp1_treat tgtgroup_hyp2_treat covered_hyp2_treat                  ///
    coveragert_hyp2_treat qbs_hyp2_treat tgtgroup_infarction               ///
    covered_infarction coveragert_infarction qbs_infarction                ///
    tgtgroup_infarction_treat1 covered_infarction_treat1                   ///
    coveragert_infarction_treat1 qbs_infarction_treat1                     ///
    tgtgroup_infarction_treat2 covered_infarction_treat2                   ///
    coveragert_infarction_treat2 qbs_infarction_treat2                     ///
    tgtgroup_hypothyreosis covered_hypothyreosis                           ///
    coveragert_hypothyreosis qbs_hypothyreosis qbs_diab_treat total_points_domain

// Label all variables
  label var tgtgroup_diab_monitor         "Type-II Diabetes (monitor) - population eligible"
  label var covered_diab_monitor          "Type-II Diabetes (monitor) - patients served"
  label var coveragert_diab_monitor       "Type-II Diabetes (monitor) - coverage ratio"
  label var qbs_diab_monitor              "Type-II Diabetes (monitor) - QBS points"
  label var tgtgroup_diab_treat           "Type-II Diabetes (treat) - population eligible"
  label var covered_diab_treat            "Type-II Diabetes (treat) - patients served"
  label var coveragert_diab_treat         "Type-II Diabetes (treat) - coverage ratio"
  label var qbs_diab_treat                "Type-II Diabetes (treat) - QBS points"
  label var tgtgroup_hyp1_monitor         "Hypertension-low (monitor) - population eligible"
  label var covered_hyp1_monitor          "Hypertension-low (monitor) - patients served"
  label var coveragert_hyp1_monitor       "Hypertension-low (monitor) - coverage ratio"
  label var qbs_hyp1_monitor              "Hypertension-low (monitor) - QBS points"
  label var tgtgroup_hyp1_treat           "Hypertension-low (treat) - population eligible
  label var covered_hyp1_treat            "Hypertension-low (treat) - patients covered"
  label var coveragert_hyp1_treat         "Hypertension-low (treat) - coverage ratio"
  label var qbs_hyp1_treat                "Hypertension-low (treat) - QBS points"
  label var tgtgroup_hyp2_monitor         "Hypertension-med (monitor) - population eligible"
  label var covered_hyp2_monitor          "Hypertension-med (monitor) - patients served"
  label var coveragert_hyp2_monitor       "Hypertension-med (monitor) - coverage ratio"
  label var qbs_hyp2_monitor              "Hypertension-med (monitor) - QBS points"
  label var tgtgroup_hyp2_treat           "Hypertension-med (treat) - population eligible"
  label var covered_hyp2_treat            "Hypertension-med (treat) - patients served"
  label var coveragert_hyp2_treat         "Hypertension-med (treat) - coverage ratio"
  label var qbs_hyp2_treat                "Hypertension-med (treat) - QBS points"
  label var tgtgroup_hyp3_monitor         "Hypertension-high (monitor) - population eligible"
  label var covered_hyp3_monitor          "Hypertension-high (monitor) - patients served"
  label var coveragert_hyp3_monitor       "Hypertension-high (monitor) - coverage ratio"
  label var qbs_hyp3_monitor              "Hypertension-high (monitor) - QBS points"
  label var tgtgroup_infarction           "Myocardial infarction (monitor) - population eligible"
  label var covered_infarction            "Myocardial infarction (monitor) - patients served"
  label var coveragert_infarction         "Myocardial infarction (monitor) - coverage ratio"
  label var qbs_infarction                "Myocardial infarction (monitor) - QBS points"
  label var tgtgroup_infarction_treat1    "Myocardial infarction (treat-1) - population eligible"
  label var covered_infarction_treat1     "Myocardial infarction (treat-1) - patients served"
  label var coveragert_infarction_treat1  "Myocardial infarction (treat-1) - coverage ratio"
  label var qbs_infarction_treat1         "Myocardial infarction (treat-1) - QBS points"
  label var tgtgroup_infarction_treat2    "Myocardial infarction (treat-2) - population eligible"
  label var covered_infarction_treat2     "Myocardial infarction (treat-2) - patients served"
  label var coveragert_infarction_treat2  "Myocardial infarction (treat-2) - coverage ratio"
  label var qbs_infarction_treat2         "Myocardial infarction (treat-2) - QBS points"
  label var tgtgroup_hypothyreosis        "Hypothyroid (monitor) - population eligible"
  label var covered_hypothyreosis         "Hypothyroid (monitor) - patients served"
  label var coveragert_hypothyreosis      "Hypothyroid (monitor) - coverage ratio"
  label var qbs_hypothyreosis             "Hypothyroid (monitor) - QBS points"
  
// Cleaning

  foreach var of varlist * {
    local label : var lab `var'
    local label = subinstr(trim(proper("`label'")),"Qbs","QBS",.)
    local label = subinstr("`label'","Ii","2",.)
    lab var `var' "`label'"
  }
  
  compress
  ren *_* *[2]_*[1]
  ren *_*_* *[3]_*[1]_*[2]
  
  order * , seq
  
  ren points_domain_total qbs_score

  order uid qbs_score , first
    lab var uid "Unique ID"

// Save

  save "${constructed}/qbs-domainii_clean.dta", replace

// End of dofile
