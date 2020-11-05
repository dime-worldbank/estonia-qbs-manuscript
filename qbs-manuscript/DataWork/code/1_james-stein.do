// JAMES-STEIN ESTIMATION

/* This do-file does the following:
  1. Write program to produce positive-part James-Stein estimators
  2. Calculate the J-S estimators for each indicator in "${constructed}/qbs-domainii_clean.dta"
  3. Output estimated performance to "${constructed}/qbs_shrinkage.dta"
*/

// 1. Write program to produce positive-part James-Stein estimators
cap prog drop js
prog def js
syntax anything

  tempvar id mu s ss cov c m dev mean_coverage indiv_deviation
  gen `id' = _n

  preserve
    // Calculate mean for unit
    gen `mu' = covered_`anything'/tgtgroup_`anything'

    // Total units
    local k = `c(N)'

    // Create fake subdata, expand provider level to patient level
    expand tgtgroup_`anything'
    
    // Patient-level coverage imputed
    bys `id' : gen `cov' = (_n <= covered_`anything')
    
    // Patient-level deviations from grand mean
    egen `mean_coverage' = mean(`cov')
      gen `indiv_deviation' = (`cov' - `mean_coverage')^2

    // Get standard deviation of the mean for each unit
    collapse (sum) `indiv_deviation' ///
      (firstnm) covered_`anything' tgtgroup_`anything' `mu' , by(`id')
      
      gen `s' = `indiv_deviation' / sqrt(tgtgroup_`anything')

    // Calculate shrinkage
    egen `m' = mean(`mu')
    gen `dev' = `mu' - `m'
    egen `ss' = sum(`indiv_deviation')
    
    gen `c' = max(0 , 1 - ((`k'-2)*`s') / `ss')
    
    // JS estimator
    gen js_`anything' = (`mu') + `c' * (`m' - `mu')

    tempfile calc
      save `calc' , replace

  restore

  qui merge 1:1 `id' using `calc' , nogen

end

// 2. Calculate the J-S estimators for each indicator in "${constructed}/qbs-domainii_clean.dta"
use "${constructed}/qbs-domainii_clean.dta", clear

  local indicators diab_monitor diab_treat hyp1_monitor hyp1_treat      ///
    hyp2_monitor hyp2_treat hyp3_monitor                ///
    infarction infarction_treat1 infarction_treat2      ///
    hypothyreosis

  qui foreach i of local indicators {

    replace coveragert_`i' = coveragert_`i'/100

    cap drop js_`i'
    js `i'
  }

// 3. Output estimated performance to "${constructed}/qbs_shrinkage.dta"

  save "${constructed}/qbs_shrinkage.dta", replace

// End of dofile
