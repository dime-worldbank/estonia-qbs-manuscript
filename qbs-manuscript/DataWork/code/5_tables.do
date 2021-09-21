

  // Load clean dataset
  use "${constructed}/qbs-domainii_clean.dta", clear

  //Table - 2 Summary table for section 3.2 -- summary of all indicators

   estpost tabstat  diab_monitor_tgtgroup  diab_monitor_covered   diab_monitor_coveragert ///
					diab_treat_tgtgroup    diab_treat_covered     diab_treat_coveragert   ///
					hyp1_monitor_tgtgroup  hyp1_monitor_covered   hyp1_monitor_coveragert  ///
					hyp1_treat_tgtgroup   hyp1_treat_covered     hyp1_treat_coveragert   ///
					hyp2_monitor_tgtgroup  hyp2_monitor_covered   hyp2_monitor_coveragert  ///
					hyp2_treat_tgtgroup    hyp2_treat_covered     hyp2_treat_coveragert   ///
					hyp3_monitor_tgtgroup  hyp3_monitor_covered   hyp3_monitor_coveragert  ///
					hypothyreosis_tgtgroup hypothyreosis_covered  hypothyreosis_coveragert  ///
					infarction_tgtgroup    infarction_covered     infarction_coveragert    ///
					infarction_treat1_tgtgroup infarction_treat1_covered infarction_treat1_coveragert  ///
					infarction_treat2_tgtgroup infarction_treat2_covered infarction_treat2_coveragert, ///
					statistics(mean p50 sd min max) columns(statistics)

  //esttab using "${output}/table_summary_indicator.csv", cells("mean p50 sd min max") nomtitle nonumber label replace
  esttab using "${output}/table_summary_indicator.tex", cells("mean(fmt(%9.0f)) p50(fmt(%9.0g)) sd(fmt(%9.0f)) min(fmt(%9.0g)) max(fmt(%9.0g))") nomtitle nonumber label replace
