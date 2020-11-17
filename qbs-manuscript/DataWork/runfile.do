// QBS Manuscript Runfile
// This runfile sets all the globals and filepaths for all users,
// and runs all the processes for the analysis of the QBS manuscript.

// Standardize settings accross users
  ieboilstart, version(12.1) // Set the version number to the oldest version
    `r(version)' // This line is needed to actually set the version

// File paths for users
  if "`c(username)'" == "meyhar"   {      // MM
    global projectfolder  "/Users/meyhar/Box/Estonia ECM/Research outputs/qbs-manuscript"
    global github      "/Users/meyhar/Documents/GitHub/estonia-qbs-manuscript/qbs-manuscript"
  }

  if "`c(username)'" == "bbdaniels"   {        // BBD
    global projectfolder  "/Users/bbdaniels/Box/Estonia ECM/Research outputs/qbs-manuscript"
    global github  "/Users/bbdaniels/GitHub/estonia-qbs-manuscript/qbs-manuscript"
  }

  // Root globals - Box
  global datawork       "${projectfolder}/DataWork"
    global data         "${datawork}/raw"
    global constructed  "${datawork}/constructed"

  // Root globals - Github
  global code       "${github}/DataWork/code"
    global output   "${github}/DataWork/output"

/******************************************************************/
// CODE TO PRODUCE ESTIMATION AND FIGURES/TABLES FOR PAPER
/******************************************************************/

  // RUN CLEANING OF QBS SCORES
  do "${code}/0_cleaning.do"

  // RUN STEIN ESTIMATION
  do "${code}/1_james-stein.do"

  // RUN NEED-BASED ESTIMATION
  do "${code}/2_need-based.do"

  // RUN PCA ANALYSIS
  do "${code}/3_pca.do"

  // RUN ALL FIGURES FOR THE PAPER
  do "${code}/4_figures_section.do"

  // RUN TABLE 2 FOR THE PAPER
  do "${code}/5_summary_tables.do"

// End of dofile
