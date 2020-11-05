
	//PCA


	use "${constructed}/qbs-domainii_clean.dta", clear


	// In the data :

		// tgtgroup_`indicator'				= underlying population (den)
		// covered_`indicator'				= achieved population (num)
		// coveragert_ `indicator' 			= round((num/den)*100)


	// COMPONENTS FOR PCA - COVERAGE RATIO FOR EACH INDICATOR

	global pcalist 			coveragert_diab_monitor 				///
							coveragert_diab_treat 					///
							coveragert_hyp1_monitor 				///
							coveragert_hyp2_monitor 				///
							coveragert_hyp3_monitor 				///
							coveragert_hyp1_treat 					///
							coveragert_hyp2_treat 					///
							coveragert_infarction 					///
							coveragert_infarction_treat1 			///
							coveragert_infarction_treat2 			///
							coveragert_hypothyreosis


	// PCA

	pca ${pcalist}

	// GET SCORE

	predict p1, score


	// CREATE VAR WITH LOADINGS

	gen 	loading = 0.40 if _n == 1
	replace loading = 0.07 if _n == 2
	replace loading = 0.39 if _n == 3
	replace loading = 0.40 if _n == 4
	replace loading = 0.39 if _n == 5
	replace loading = 0.10 if _n == 6
	replace loading = 0.13 if _n == 7
	replace loading = 0.40 if _n == 8
	replace loading = 0.08 if _n == 9
	replace loading = 0.11 if _n == 10
	replace loading = 0.37 if _n == 11

	// CREATE VAR WITH POINT VALUE OF SCORE

		gen pv = 65 	if _n == 1
	replace pv = 10 	if _n == 2
	replace pv = 90 	if _n == 3
	replace pv = 175 	if _n == 4
	replace pv = 40 	if _n == 5
	replace pv = 5 		if _n == 6
	replace pv = 20 	if _n == 7
	replace pv = 20 	if _n == 8
	replace pv = 5 		if _n == 9
	replace pv = 5 		if _n == 10
	replace pv = 45 	if _n == 11

	//





		gen 	name	= "Diabetes Type II: Monitoring"                 if  _n == 1
		replace name 	= "Diabetes Type II: Treatment"                if _n == 2
		replace name 	= "Hypertension Low Risk: Monitoring"          if _n == 3
		replace name 	= "Hypertension Med Risk: Monitoring"          if _n == 4
		replace name 	= "Hypertension High Risk: Monitoring"         if _n == 5
		replace name	= "Hypertension All Risk Level: Treatment"     if _n == 6
		replace name 	= "Hypertension Med-High Risk Level: Treatment" if _n == 7
		replace name 	= "Myocardial Infarction: Monitoring"           if _n == 8
		replace name 	= "Myocardial Infarction Beta Blockers: Treatment" if _n == 9
		replace name 	= "Myocardial Infarction Statins: Treatment"       if _n == 10
		replace name 	= "Hypothyroidism: Monitoring"                     if _n == 11


		// RESCALE WEIGHTS

		egen loading_sum 		= total(loading)
		gen  multiplier_factor 	= (480/loading_sum)			// All indicators should sum to 480

		gen rescaled_weight		=	(multiplier_factor*loading)

		replace rescaled_weight = round(rescaled_weight,1)



		/* 		NEW POINT WEIGHTS BASED ON RAW COVERAGE DATA

					Loading pv diab_monitor 	= 68
					Loading pca_diab_treat 		= 12
					Loading pca_hyp1_m			= 66
					Loading pca_hyp2_m 		    = 68
					Loading pca_hyp3_m			= 66
					Loading pca_hyp1_t			= 17
					Loading pca_hyp2_t			= 22
					Loading pca_mi 				= 68
					Loading pca_mi1				= 14
					Loading pca_mi2				= 19
					Loading pca_thyroid			= 63

		*/




	//-----------------------------------------------------------------------//
