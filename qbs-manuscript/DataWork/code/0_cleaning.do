// Load and clean the QBS data
use "${data}/domainii_qbs_2019.dta", clear
  isid list_id, sort
  set seed 428252	// Timestamp: 2020-11-05 20:13:06 UTC
  gen rand = rnormal()
  sort rand
  gen uid = "QBS" + strofreal(_n,"%03.0f")

  ren den_* tgtgroup_*
ren num_* covered_*

  ren tgtgroup_inf_monitor tgtgroup_infarction
  ren covered_inf_monitor covered_infarction
  ren coveragert_inf_monitor coveragert_infarction
  ren qbs_inf_monitor qbs_infarction
  ren tgtgroup_inf_treat tgtgroup_infarction_treat1
  ren covered_inf_treat covered_infarction_treat1
  ren coveragert_inf_treat coveragert_infarction_treat1
  ren qbs_inf_treat qbs_infarction_treat1
  ren tgtgroup_inf_treat2 tgtgroup_infarction_treat2
  ren covered_inf_treat2 covered_infarction_treat2
  ren coveragert_inf_treat2 coveragert_infarction_treat2
  ren qbs_inf_treat2 qbs_infarction_treat2
  ren tgtgroup_hypothyroid_monitor tgtgroup_hypothyreosis
  ren covered_hypothyroid_monitor covered_hypothyreosis
  ren coveragert_hypothyroid_monitor coveragert_hypothyreosis
  ren qbs_hypothyroid_monitor qbs_hypothyreosis

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

  ren points_qbs qbs_score

  order uid qbs_score , first
    lab var uid "Unique ID"

// Save

  save "${constructed}/qbs-domainii_clean_2019.dta", replace

// End of dofile
