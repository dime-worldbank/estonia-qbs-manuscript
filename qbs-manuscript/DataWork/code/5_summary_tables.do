

	// Load clean dataset
	use "${constructed}/qbs-domainii_clean.dta", clear

	//Table - 2 Summary table for section 3.2 -- summary of all indicators

	 estpost tabstat  tgtgroup_diab_monitor covered_diab_monitor coveragert_diab_monitor 						///
	 									tgtgroup_diab_treat covered_diab_treat coveragert_diab_treat 											///
										tgtgroup_hyp1_monitor covered_hyp1_monitor coveragert_hyp1_monitor 								///
										tgtgroup_hyp2_monitor covered_hyp2_monitor coveragert_hyp2_monitor 								///
										tgtgroup_hyp3_monitor covered_hyp3_monitor coveragert_hyp3_monitor 								///
										tgtgroup_hyp1_treat covered_hyp1_treat coveragert_hyp1_treat 											///
										tgtgroup_hyp2_treat covered_hyp2_treat coveragert_hyp2_treat 											///
										tgtgroup_infarction covered_infarction coveragert_infarction 											///
										tgtgroup_infarction_treat1 covered_infarction_treat1 coveragert_infarction_treat1 	///
										tgtgroup_infarction_treat2 covered_infarction_treat2 coveragert_infarction_treat2 	///
										tgtgroup_hypothyreosis covered_hypothyreosis coveragert_hypothyreosis, 						///
										statistics(mean p50 sd min max) columns(statistics)
	//esttab using "${output}/table_summary_indicator.csv", cells("mean p50 sd min max") nomtitle nonumber label replace
	esttab using "${output}/table_summary_indicator.tex", cells("mean(fmt(%9.0f)) p50(fmt(%9.0g)) sd(fmt(%9.0f)) min(fmt(%9.0g)) max(fmt(%9.0g))") nomtitle nonumber label replace
