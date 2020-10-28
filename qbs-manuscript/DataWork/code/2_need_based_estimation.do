


		// LOCAL FOR INDICATORS

		local indicators 		diab_monitoring	///
								diab_treat		///
								hyp1_monitor	///
								hyp2_monitor	///
								hyp3_monitor 	///
								hyp1_treat		///
								hyp2_treat		///
								infarction		///
								infarction_treat1		///
								infarction_treat2		///
								hypothyreosis
		
		
		ren targetgroup_diab_monitoring tgtgroup_diab_monitoring

		// METHOD II -- capture sensitivity -- Adjusted Coverage

		foreach indicator of local indicators 		{

				egen num_`indicator'_m         = mean(covered_`indicator')						 // mean(covered)
				gen num_`indicator'_2	   	   = num_`indicator'_m * .5						 // mean(covered) * 0.5
				gen num_`indicator'		   	   = covered_`indicator' + num_`indicator'_2 		 // new numerator
			

				egen den_`indicator'_m         = mean(tgtgroup_`indicator')						 // mean(population)
				gen den_`indicator'_2	   	   = den_`indicator'_m * .5						 // mean(popn) * 0.5
				gen den_`indicator'		   	   = tgtgroup_`indicator' + den_`indicator'_2 		 // new denominator
				

				gen c_`indicator'	= (num_`indicator' / den_`indicator')							// Adjusted Coverage
				
				
			}
