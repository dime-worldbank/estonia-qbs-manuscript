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

    // Create fake subdata
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

// Test
use "/Users/bbdaniels/Box/Estonia ECM/Research outputs/qbs manuscript/DataWork/raw/domainii_qbs.dta", clear

cap drop js_diab_treat
js diab_treat

gen check = coveragert_diab_treat/100

su check
  gen d = check - `r(mean)'

gen shrinkage = abs((check - js_diab_treat)/(check - d))

scatter  shrinkage tgtgroup_diab_treat

// End of dofile
