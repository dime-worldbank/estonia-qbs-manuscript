
// QBS Manuscript Master

	/* This master do file sets all the globas, files paths for all users,
		runs all the pprocesses for the analysis of the QBS manuscript
		
	*/
	
	// Clear memory
		
		clear all
		set more off	
	
	
	// File paths for users

		 *Standardize settings accross users
	   ieboilstart, version(12.1)          //Set the version number to the oldest version used by anyone in the project team
	   `r(version)'                        //This line is needed to actually set the version from the command above

		/******************************************************************/

		// Set users


	  if "`c(username)'" == "meyhar"	 {			// MM
	  
		// Box
		global projectfolder	"/Users/meyhar/Box/Estonia ECM/Research outputs/qbs manuscript"	
		
		//Github
		global github			"/Users/meyhar/Documents/GitHub/estonia-qbs-manuscript/qbs-manuscript"
		
		
	  }
	  if "`c(username)'" == "bbdaniels"	 {				// BBD
		global projectfolder	"/Users/bbdaniels/Box/Estonia ECM/Research outputs/qbs manuscript"
		}
		
		// Root globals - Box
		
		global datawork 		"${projectfolder}/DataWork"
		global data				"${datawork}/raw"
		global output			"${datawork}/output"
		global constructed		"${datawork}/construct"
		
		// Root globals - Github
		
		global code 			"${github}/DataWork/code"
