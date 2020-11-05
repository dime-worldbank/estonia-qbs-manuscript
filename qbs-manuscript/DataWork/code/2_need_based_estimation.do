//          NEED-BASED ESTIMATION
/* This do file does the following:
1. Create all variables for setting up need-based estimation using "${constructed}/qbs_shrinkage.dta"
2. Apply the need-based estimation on all the indicators
3. Calculate the final scores with partial credit and need-based estimation
4. Output is "${constructed}/qbs_shrinkage.dta" with need-based coverage rates
*/

// 1. Create all variables for setting up need-based estimation
use "${constructed}/qbs_shrinkage.dta", clear

  local indicators diab_monitor   ///
    diab_treat     ///
    hyp1_monitor   ///
    hyp2_monitor   ///
    hyp3_monitor   ///
    hyp1_treat     ///
    hyp2_treat     ///
    infarction     ///
    infarction_treat1     ///
    infarction_treat2     ///
    hypothyreosis

// 2. Apply the need-based estimation on all the indicators
foreach indicator of local indicators {

  egen num_`indicator'_m  = mean(`indicator'_covered)       // mean(covered)
  gen num_`indicator'_2   = num_`indicator'_m * .5          // mean(covered) * 0.5
  gen num_`indicator'     = `indicator'_covered + num_`indicator'_2   // new numerator


  egen den_`indicator'_m  = mean(`indicator'_tgtgroup)    // mean(population)
  gen den_`indicator'_2   = den_`indicator'_m * .5        // mean(popn) * 0.5
  gen den_`indicator'     = `indicator'_tgtgroup + den_`indicator'_2      // new denominator

  gen `indicator'_nb  = (num_`indicator' / den_`indicator')              // Adjusted Coverage
  
  local label : var lab `indicator'_coveragert
    local label = substr("`label'",1,strpos("`label'"," - "))
    local label = "`label'" + " -  Need-Based"
    lab var `indicator'_nb "`label'"

  // Drop mean generated variables
  drop *_`indicator'*

}

// 3. Calculate the final scores with partial credit and need-based estimation

  // NEW SCORES = Need-adjusted coverage * Weight from PCA
  
    gen pca_diab_monitor = diab_monitor_nb * 68
    gen pca_diab_treat   = diab_treat_nb * 12
    gen pca_hyp1_m       = hyp1_monitor_nb * 66
    gen pca_hyp2_m       = hyp2_monitor_nb * 68
    gen pca_hyp3_m       = hyp3_monitor_nb * 66
    gen pca_hyp1_t       = hyp1_treat_nb * 17
    gen pca_hyp2_t       = hyp2_treat_nb * 22
    gen pca_mi           = infarction_nb * 68
    gen pca_mi1          = infarction_treat1_nb * 14
    gen pca_mi2          = infarction_treat2_nb * 19
    gen pca_thyroid      = hypothyreosis_nb * 63

    // NEW FINAL SCORES
    gen new_pca_score = pca_diab_monitor +   ///
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

  // Simple partial credit

    gen p_diab_monitor = diab_monitor_coveragert * 65
    gen p_diab_treat  = diab_treat_coveragert * 10
    gen p_hyp1_m      = hyp1_monitor_coveragert * 90
    gen p_hyp2_m      = hyp2_monitor_coveragert * 175
    gen p_hyp3_m      = hyp3_monitor_coveragert * 40
    gen p_hyp1_t      = hyp1_treat_coveragert * 5
    gen p_hyp2_t      = hyp2_treat_coveragert * 20
    gen p_mi          = infarction_coveragert * 20
    gen p_mi1         = infarction_treat1_coveragert * 5
    gen p_mi2         = infarction_treat2_coveragert * 5
    gen p_thyroid     = hypothyreosis_coveragert * 45

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
     
    drop pca* p_*
    
    ren new_pca_score qbs_score_nb
      lab var qbs_score_nb "Need-Based PC QBS Score"
    ren total_prop_score qbs_score_pca
      lab var qbs_score_pca "Principal-Components QBS Score"
      
   order * , seq
    
   order uid qbs_score* , first
     lab var uid "Unique ID"

//Save dataset with James-Stein and need-based coverage rates

  save "${constructed}/qbs_shrinkage.dta", replace

// End of dofile
