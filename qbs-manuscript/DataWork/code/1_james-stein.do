// James-Stein estimators for coverage


// Total number of means
cap prog drop js
prog def js
syntax anything

  tempvar id mu s ss cov c m dev
  gen `id' = _n

  preserve
    // Calculate mean for unit
    gen `mu' = covered_`anything'/tgtgroup_`anything'

    // Total units
    local k = `c(N)'

    // Create fake subdata, expand provider level to patient level
    expand tgtgroup_`anything'
    bys `id' : gen `cov' = (_n <= covered_`anything')

    // Get SEM -> variance for each unit
    collapse (sem) `cov' (firstnm) covered_`anything' tgtgroup_`anything' `mu' , by(`id')
    gen `s' = `cov'^2

    // Calculate shrinkage
    egen `m' = mean(`mu')
    gen `dev' = `mu' - `m'
    egen `ss' = sum(`dev'^2)
    gen `c' = max(0 , 1 - ((`k'-2)*`s') / `ss')

    // JS estimator
    gen js_`anything' = (`m') + `c' * (`mu' - `m')

    tempfile calc
      save `calc' , replace

  restore

  qui merge 1:1 `id' using `calc' , nogen

end

// Apply on QBS scores
use "${constructed}/qbs-domainii_clean.dta", clear


local indicators diab_monitor diab_treat hyp1_monitor hyp1_treat      ///
                  hyp2_monitor hyp2_treat hyp3_monitor                ///
                  infarction infarction_treat1 infarction_treat2      ///
                  hypothyreosis

foreach i of local indicators {

  replace coveragert_`i' = coveragert_`i'/100

  cap drop js_`i'
  js `i'
}

  save "${constructed}/qbs_shrinkage.dta", replace




/*
gen check = coveragert_diab_treat/100

su check
  gen d = check - `r(mean)'

gen shrinkage = abs((check - js_diab_treat)/(check - d))

scatter  shrinkage tgtgroup_diab_treat
*/
// End of dofile
