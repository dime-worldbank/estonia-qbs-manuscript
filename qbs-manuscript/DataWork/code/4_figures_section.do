

	/////////////////////////////////////////////////////////////////////////
	// 					FIGURES FOR RESULTS SECTION
	// 							Section 3.1

	/* This do file makes the following figures
		1. Figure with histogram and scatter of QBS scores from 2017 and 2018
		2. Figure with line graphs on QBS score performance from 2017 to 2018
		3. Distribution of the disease burden across patient population by age
	*/
	//
	//////////////////////////////////////////////////////////////////////////


  // Load Data

  use "${data}/qbs_historical_appended.dta", clear

  destring qbs_points, replace
  destring year, replace

 ///////////////////////////////////////////////////////////////////////////
	// FIGURE 1.

	 // HISTOGRAM WITH SCORES + SCATTER

	 // HISTOGRAM
	 preserve

	 keep if year == 2017 | year == 2018
		bys doctor_name: gen n = _N
		keep if n == 2
		keep doctor_name register_code year qbs_points
		drop register_code
		reshape wide qbs_points , i(doctor_name ) j(year)

		histogram qbs_points2017,	///
					freq  bfcolor(gre%30) lwidth(none) gap(5) start(0) width(20) ///
					fxsize(100) fysize(25) ///
					 ytitle("") xtit("")	///
					 yla(none) yscale(lstyle(none))	///
					xlabel(0(200)700, angle(horizontal) nogrid) xtit("QBS scores 2017", color(black))	///
					plotregion(margin(zero)) graphregion(color(white)) 	///
					saving("hist_xvar", replace)




		histogram qbs_points2018, ///
					freq bfcolor(gre%30) lwidth(none) gap(5) start(0) width(20) horizontal ///
					xtitle("") ytit("")  xscale(rev) yscale(alt) ///
					fxsize(25) fysize(100) xla(none) ///
					ylabel(0(200)700, angle(horizontal) nogrid) xscale(lstyle(none)) ytit("QBS scores 2018", color(black))	///
					plotregion(margin(zero)) graphregion(color(white)) ///
					saving("hist_y61", replace)

		gen v = 0 if _n == 1
		replace v = 200 if _n == 2
		replace v = 700 if _n == 3
		replace v = 780 if _n == 4

		gen h = 0 if _n == 1
		replace h = 200 if _n == 2
		replace h = 700 if _n == 3
		replace h = 780 if _n == 4


		// SCATTER

		tw (scatter qbs_points2018 qbs_points2017 if qbs_points2017 < qbs_points2018,  ///
			mcolor(emerald%40) mlwidth(none) yline(512, lpattern(dash) lcolor(black)) ///
			yline(576, lpattern(dash) lcolor(black)) yla(512 "80%" 576 "90%", angle(0) ///
			labsize(small)) xla(none)  ytit("") xtit("") legend(off))	///
		(scatter qbs_points2018 qbs_points2017 if qbs_points2017 > qbs_points2018,  ///
		mcolor(maroon%40) mlwidth(none) legend(off) xline(792, lcolor(black)) 		///
		yline(796, lcolor(black))) ///
			(line v h, lcolor(black)), graphregion(color(white)) saving("graph_y6", replace)


		// COMBINE HISTOGRAM + SCATTER

		graph combine  hist_xvar.gph hist_y61.gph graph_y6.gph   ,	///
		holes(1) rows(2) 	///
		imargin(1 1 1 1) graphregion(color(white))  ///
		saving(graphwithhistograms.gph, replace)
		graph export "${output}/fig_1_qbs_2017_2018.png", width(4000) replace

	restore

	///////////////////////////////////////////////////////////////////////////

		// FIGURE 2.
		// LINE GRAPH

	// Distribution of QBS scores for each provider for 2017 and 2018 only
	use "${data}/qbs_historical_appended.dta", clear

	destring qbs_points, replace
	destring year, replace

	set scheme uncluttered

	preserve

	drop if year == 2016

	//Providers that perform consistently low
	gen score_512 = (qbs_points<512)
	bys doctor_name: egen sum_2017_2018 = total(score_512)

	// Providers that perform consistently high
	gen score_576 = (qbs_points>576)
	bys doctor_name: egen sum_2017_2018_576 = total(score_576)

	// Provides that perform consistently in the middle
	gen score_512_576 = (qbs_points>=512 & qbs_points<=576)
	bys doctor_name: egen sum_2017_2018_mid = total(score_512_576)


	bys doctor_name: gen n = _N
	line qbs_points year if n == 2 , connect(ascending)
	/*
	line qbs_points year if n == 2 , connect(ascending) lcolor(black%20) lwidth(vthin)  xlab(2017(1)2018) ///
	ytit(QBS Score per Provider) xtit(Year) ///
	tit("QBS score trend per Provider")
	*/
	tw ///
	(line qbs_points year if n == 2 & sum_2017_2018 == 2,  connect(ascending) lcolor(red%20) lwidth(vthin)  xlab(2017(1)2018) ///
	ylab(0 "0" 200 "200" 400 "400" 600 "600" 800 "800" 512 "512" 576 "576", angle(0) labgap(3))) 	///
	(line qbs_points year if n == 2 & sum_2017_2018_576 == 2, connect(ascending) lcolor(green%20) lwidth(vthin)  xlab(2017(1)2018)) ///
	(line qbs_points year if n == 2 & sum_2017_2018_mid == 2, connect(ascending) lcolor(blue%50) lwidth(vthin)  xlab(2017(1)2018))	 ///
	(line qbs_points year if n == 2 & (sum_2017_2018_mid == 1 | sum_2017_2018_576 == 1 | sum_2017_2018 == 1), connect(ascending) lcolor("105 105 105") lwidth(vvthin) xlab(2017(1)2018)  ytit("") xtit(" ") xla(none)  xscale(alt) plotregion(lcolor(black))), yline(576, lwidth(thin) lcolor(black)) yline(512, lwidth(thin)  lcolor(black)) legend(off) graphregion(color(white)) xsize(3) saving(line, replace)

	gr export "${output}/figure_2_line.png", width(4000) replace

	restore


	//////////////////////////////////////////////////////////////////////////

		// FIGURE 3.
		// SCATTER OF DISEASE BURDEN

	// Distribution of patients with related illnesses by QBS Score


	use "${data}/sampling-lists-mergedrisk.dta", replace

	preserve

	collapse (mean) Numberofrelatedillnesses (mean) list_age, by(clinic_id)
	 drop if list_age < 20
	drop if Numberofrelatedillnesses > 15000
	format Numberofrelatedillnesses %9.0f

	  local dot jitter(3)   msize(small)


  tw ///
    (scatter Numberofrelatedillnesses list_age if list_age < 40 , `dot' mc(olive_teal) mlwidth(none) ) ///
    (scatter Numberofrelatedillnesses list_age if list_age >= 40 & Numberofrelatedillnesses < 500 , `dot' mc(ltblue) mlwidth(none)) ///
    (scatter Numberofrelatedillnesses list_age if  Numberofrelatedillnesses >= 500 & list_age >= 40, `dot' mc("64 105 166") mlwidth(none) yla(, angle(0) nogrid) legend(off) xla(20(10)60)) ///
  , graphregion(color(white)) xtitle("Average age of patients in the practice") ytitle("Total number of ECM related illnesses")

   graph export "${output}/fig_3_disease_burden.png" , width(4000) replace

   restore

	 // FIGURE 4.LOWESS ON COVERAGE AND NEED BY INDICATOR

	 	use "${constructed}/qbs-domainii_clean.dta", clear

	 local indicators 		diab_monitor 	///
								diab_treat 		///
								hyp1_monitor 	///
								hyp2_monitor 	///
								hyp3_monitor 	///
								hyp1_treat 		///
								hyp2_treat 		///
								infarction 		///
								infarction_treat1 	 	///
								infarction_treat2 		///
								hypothyreosis

		local  diab_monitor_title  	"Diabetes Type II: Monitoring"
		local  diab_treat_title 		"Diabetes Type II: Treatment"
		local  hyp1_monitor_title 		"Hypertension Low Risk: Monitoring"
		local  hyp2_monitor_title		"Hypertension Med Risk: Monitoring"
		local  hyp3_monitor_title 		"Hypertension High Risk: Monitoring"
		local  hyp1_treat_title			"Hypertension All Risk: Treatment"
		local  hyp2_treat_title 		"Hypertension Med-High Risk: Treatment"
		local  infarction_title			"Myocardial Infarction: Monitoring"
		local  infarction_treat1_title 	"Myocardial Infarction Beta Blockers: Treatment"
		local  infarction_treat2_title  "Myocardial Infarction Statins: Treatment"
		local  hypothyreosis_title		"Hypothyroidism: Monitoring"


		local yline_diab_monitor  	76
		local yline_diab_treat  		70
		local yline_hyp1_monitor  		76
		local yline_hyp2_monitor  		73
		local yline_hyp3_monitor  		70
		local yline_hyp1_treat  		90
		local yline_hyp2_treat  		83
		local yline_infarction 			90
		local yline_infarction_treat1  	70
		local yline_infarction_treat2 	70
		local yline_hypothyreosis  		90


		foreach i of local indicators	{

			lowess coveragert_`i'  tgtgroup_`i' if coveragert_`i', yline(`yline_`i'', lcolor(black)) msize(0.7) mcolor(grey%30) mlwidth(none) lineopts(lcolor(red) lwidth(thick)) ylab(0(20)100, angle(0) nogrid)		///
			tit("``i'_title'", size(medsmall) color(black)) xtit("") ytit("") note("") 		///
			subtit("``i'_sub'")		///
			graphregion(color(white)) saving(`i'.gph, replace)

			}


			gr combine 			diab_monitor.gph ///
								diab_treat.gph 		///
								hyp1_monitor.gph 	///
								hyp2_monitor.gph 	///
								hyp3_monitor.gph  	///
								hyp1_treat.gph 		///
								hyp2_treat.gph 		///
								infarction.gph 		///
								infarction_treat1.gph 		///
								infarction_treat2.gph 		///
								hypothyreosis.gph, col(3) xsize(8) ysize(8) graphregion(color(white))


				gr export "${output}/fig_4_lowess_indicator.pdf", replace


	 /* FIGURE 5 -- Will move here after finalizing
	 // JAMES STEIN V NEED-BASED

	 // Plot JS and need-based

	 use "${constructed}/qbs_shrinkage.dta", clear

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
													hypothyreosis.gph , col(3) xsize(5) ysize(12) graphregion(color(white))

	*/
	//////////////// END OF DO FILE ////////////////////////////////////////////
