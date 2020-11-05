/////////////////////////////////////////////////////////////////////////
// 				 NEED-BASED ESTIMATION
/* This do file does the following:
1. Create all variables for setting up need-based estimation using "${constructed}/qbs_shrinkage.dta"
2. Apply the need-based estimation on all the indicators
3. Output is "${constructed}/qbs_shrinkage.dta" with need-based coverage rates
4. Plot need-based and james stein estimations (move to 4_figures_section.do after finalized with BD)
5. Calculate the final scores with partial credit and need-based estimation
*/
//////////////////////////////////////////////////////////////////////////


		// LOCAL FOR INDICATORS

 // 1. SET UP NEED-BASED ADJUSTMENT

		use "${constructed}/qbs_shrinkage.dta", clear

		local indicators 		diab_monitor 	///
								diab_treat 		///
								hyp1_monitor 	///
								hyp2_monitor 	///
								hyp3_monitor 	///
								hyp1_treat 		///
								hyp2_treat 		///
								infarction 		///
								infarction_treat1 		///
								infarction_treat2 		///
								hypothyreosis

		// Need-based adjustment -- capture sensitivity -- Adjusted Coverage

 // 2. APPLY NEED-BASED ADJUSTMENT
		foreach indicator of local indicators 		{

				egen num_`indicator'_m         = mean(covered_`indicator')						 // mean(covered)
				gen num_`indicator'_2	   	   = num_`indicator'_m * .5						 // mean(covered) * 0.5
				gen num_`indicator'		   	   = covered_`indicator' + num_`indicator'_2 		 // new numerator


				egen den_`indicator'_m         = mean(tgtgroup_`indicator')						 // mean(population)
				gen den_`indicator'_2	   	   = den_`indicator'_m * .5						 // mean(popn) * 0.5
				gen den_`indicator'		   	   = tgtgroup_`indicator' + den_`indicator'_2 		 // new denominator


				gen c_`indicator'	= (num_`indicator' / den_`indicator')							// Adjusted Coverage

				// Drop mean generated variables
				drop num_`indicator'_m den_`indicator'_m

			}

			//Save dataset with James-Stein and need-based coverage rates

			save "${constructed}/qbs_shrinkage.dta", replace


			//3.  PLOT JS AND NEED-BASED

			// FIGURE 5
	 	 // JAMES STEIN V NEED-BASED

	 	 // Plot JS and need-based

	 	 // Local for indicator names

	 	 local indicators 			diab_monitor ///
	 	                        diab_treat ///
	 													hyp1_monitor 	///
	 													hyp2_monitor 	///
	 													hyp3_monitor 	///
	 													hyp1_treat 		///
	 													hyp2_treat 		///
	 													infarction 		///
	 													infarction_treat1 	///
	 													infarction_treat2 		///
	 													hypothyreosis


	 	local  diab_monitor_title  	"Type-II Diabetes (monitor)"
	 	local  diab_treat_title  "Type-II Diabetes (treat)"
	 	local  hyp1_monitor_title   "Hypertension-low (monitor)"
	 	local  hyp2_monitor_title "Hypertension-medium (monitor)"
	 	local  hyp3_monitor_title 	"Hpertension-high (monitor)"
	 	local  hyp1_treat_title 	"Hypertension-all risk (treat)"
	 	local  hyp2_treat_title 		"Hypertension-medum high risk (treat)"
	 	local  infarction_title  "Myocardial infarction (monitor)"
	 	local  infarction_treat1_title  "Myocardial infarction (treat-1)"
	 	local  infarction_treat2_title  "Myocardial infarction (treat-2)"
	 	local  hypothyreosis_title		"Hypothyroidism (monitor)"

	 	 gen start_point = 0 if _n == 1
	 	 replace start_point  = 1 if _n == 2
	 	 gen end_point = 0 if _n == 1
	 	 replace end_point = 1 if _n == 2

	 	 foreach i of local indicators {

	 	 tw ///
	 	 (scatter js_`i' coveragert_`i', mlwidth(none) msize(0.6) mcolor(emerald%60) legend(pos(6) ring(3) col(6) ///
	  lab(1 "Raw") lab(2 "Need-based") order(1 2) region(fcolor(white))) legend(subtitle("James-stein estimator vs."))) ///
	 	 (scatter js_`i' c_`i', mlwidth(none) msize(0.6) mcolor(red%60)) ///
	 	 (line end_point start_point, lcolor(black) yla(, nogrid) graphregion(color(white)) title("``i'_title'", size(med)) ///
	 	 saving(`i'.gph, replace))
	 }
	 	 grc1leg 	diab_monitor.gph ///
	 	                        diab_treat.gph ///
	 													hyp1_monitor.gph 	///
	 													hyp2_monitor.gph 	///
	 													hyp3_monitor.gph  	///
	 													hyp1_treat.gph 		///
	 													hyp2_treat.gph 		///
	 													infarction.gph 		///
	 													infarction_treat1.gph 		///
	 													infarction_treat2.gph 		///
	 													hypothyreosis.gph , col(3) xsize(6) ysize(8) graphregion(color(white))

	// 5. CALCULATE FINAL SCORES

	//-------------------------------------------------------------------//


			// 3. NEW SCORES = Need-adjusted coverage * Weight from PCA


			gen pca_diab_monitor  = c_diab_monitoring * 68
			gen pca_diab_treat  = c_diab_treat * 12
			gen pca_hyp1_m      = c_hyp1_monitor * 66
			gen pca_hyp2_m      = c_hyp2_monitor * 68
			gen pca_hyp3_m      = c_hyp3_monitor * 66
			gen pca_hyp1_t      = c_hyp1_treat * 17
			gen pca_hyp2_t      = c_hyp2_treat * 22
			gen pca_mi          = c_infarction * 68
			gen pca_mi1         = c_infarction_treat1 * 14
			gen pca_mi2         = c_infarction_treat2 * 19
			gen pca_thyroid     = c_hypothyreosis * 63


			// NEW FINAL SCORES

		gen new_pca_score = pca_diab_monitor + 	///
							pca_diab_treat + ///
							pca_hyp1_m  +  ///
							pca_hyp2_m  +  ///
							pca_hyp3_m  +  ///
							pca_hyp1_t  + ///
							pca_hyp2_t  +  ///
							pca_mi      +  ///
							pca_mi1     +  ///
							pca_mi2     +  ///
							pca_thyroid

		replace new_pca_score = round(new_pca_score)

	// 2. Simple partial credit
	
			gen p_diab_monitor = coveragert_diab_monitoring_s * 65
			gen p_diab_treat  = coveragert_diab_treat_s * 10
			gen p_hyp1_m      = coveragert_hyp1_monitor_s * 90
			gen p_hyp2_m      = coveragert_hyp2_monitor_s * 175
			gen p_hyp3_m      = coveragert_hyp3_monitor_s * 40
			gen p_hyp1_t      = coveragert_hyp1_treat_s * 5
			gen p_hyp2_t      = coveragert_hyp2_treat_s * 20
			gen p_mi          = coveragert_infarction_s * 20
			gen p_mi1         = coveragert_infarction_treat1_s * 5
			gen p_mi2         = coveragert_infarction_treat2_s * 5
			gen p_thyroid     = coveragert_hypothyreosis_s * 45

			gen total_prop_score = p_diab_monitor + ///
									p_diab_treat + ///
									p_hyp1_m + ///
									p_hyp1_t + ///
									p_hyp2_m + ///
									p_hyp2_t + ///
									p_hyp3_m + ///
									p_mi     + ///
									p_mi1    + ///
									p_mi2    + ///
									p_thyroid

		replace total_prop_score = round(total_prop_score)
