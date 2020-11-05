


		// LOCAL FOR INDICATORS
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


			// Plot JS and need-BASED

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
