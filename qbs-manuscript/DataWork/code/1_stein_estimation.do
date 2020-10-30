

	// Load the QBS data

	use "$data/domainii_qbs.dta", clear


	// Drop all unnecessary variables
	drop coefficient_1 competence pregnancy_monitor competence ///
				pregnancy_monitor gynecology_exam surgical_manipulation ///
				times_pregnancy times_gyn times_surg i_DIAB_kr s_DIAB_kr ///
				t_DIAB_kr h_DIAB_kr p_DIAB_kr i_HYP_kr s_HYP_kr t_HYP_kr ///
				h_HYP_kr p_HYP_kr t_3039 co cp cq cr cs m_punktid ///
				punktidemuutus changeinpoints cw cx


	// James-Stein estimator

	/* y_js = stein estimator for coverage ratio
	   y_bar = grand average of coverage
	   c 	= stein shrinking factor
	   c	= [1 - (n-3)sd^2]
					--------
					    S
	 S = sigma[(y)^2]

	 y_js = cy
	 */

	// Step 1. Calculate the grand average, sd, sd^2 for each indicator

	local indicators  coveragert_diab_monitoring coveragert_diab_treat	///
							coveragert_hyp1_monitor coveragert_hyp2_monitor 	///
							coveragert_hyp3_monitor coveragert_hyp1_treat		///
							coveragert_hyp2_treat coveragert_infarction			///
							coveragert_infarction_treat1 						///
							coveragert_infarction_treat2 coveragert_hypothyreosis
