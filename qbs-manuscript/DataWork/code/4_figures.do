//           FIGURES FOR RESULTS SECTION
/* This do file makes the following figures
  1a. Figure with histogram and scatter of QBS scores from 2017 and 2018
  1b. Figure with line graphs on QBS score performance from 2017 to 2018
  2. Distribution of the disease burden across patient population by age
  3. Lowess curve to highlight coverage reduces as population increases
  4. Comparison of JS and need-based
  5. Box graph with coverage rates for pca
  6. Final scores with raw, partial credit and need-based adjustment
*/

// Load Data
use "${data}/qbs_historical_appended.dta", clear

destring qbs_points, replace
destring year, replace

///////////////////////////////////////////////////////////////////////////
// FIGURE 1.

 // HISTOGRAM WITH SCORES + SCATTER

 ////////////////////////////////////////////////////////////////////////
 //FIGURE 1a.

 // HISTOGRAM
 preserve

 keep if year == 2017 | year == 2018
  bys doctor_name: gen n = _N
  keep if n == 2
  keep doctor_name register_code year qbs_points
  drop register_code
  reshape wide qbs_points , i(doctor_name ) j(year)

  histogram qbs_points2017,  ///
        freq  bfcolor(gre%30) lwidth(none) gap(5) start(0) width(20) ///
        fxsize(100) fysize(25) ///
         ytitle("") xtit("")  ///
         yla(none) yscale(lstyle(none))  ///
        xlabel(0(200)700, angle(horizontal) nogrid) xtit("QBS scores 2017", color(black))  ///
        plotregion(margin(zero)) graphregion(color(white))   ///
        saving("hist_xvar", replace)




  histogram qbs_points2018, ///
        freq bfcolor(gre%30) lwidth(none) gap(5) start(0) width(20) horizontal ///
        xtitle("") ytit("")  xscale(rev) yscale(alt) ///
        fxsize(25) fysize(100) xla(none) ///
        ylabel(0(200)700, angle(horizontal) nogrid) xscale(lstyle(none)) ytit("QBS scores 2018", color(black))  ///
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
    labsize(small)) xla(none)  ytit("") xtit("") legend(off))  ///
  (scatter qbs_points2018 qbs_points2017 if qbs_points2017 > qbs_points2018,  ///
  mcolor(maroon%40) mlwidth(none) legend(off) xline(792, lcolor(black))     ///
  yline(796, lcolor(black))) ///
    (line v h, lcolor(black)), graphregion(color(white)) saving("graph_y6", replace)


  // COMBINE HISTOGRAM + SCATTER

  graph combine  hist_xvar.gph hist_y61.gph graph_y6.gph   ,  ///
  holes(1) rows(2)   ///
  imargin(1 1 1 1) graphregion(color(white))  ///
  saving(graphwithhistograms.gph, replace)
  graph export "${output}/fig_1_qbs_2017_2018.png", width(4000) replace

restore

///////////////////////////////////////////////////////////////////////////

  // FIGURE 1b.
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
ylab(0 "0" 200 "200" 400 "400" 600 "600" 800 "800" 512 "512" 576 "576", angle(0) labgap(3)))   ///
(line qbs_points year if n == 2 & sum_2017_2018_576 == 2, connect(ascending) lcolor(green%20) lwidth(vthin)  xlab(2017(1)2018)) ///
(line qbs_points year if n == 2 & sum_2017_2018_mid == 2, connect(ascending) lcolor(blue%50) lwidth(vthin)  xlab(2017(1)2018))   ///
(line qbs_points year if n == 2 & (sum_2017_2018_mid == 1 | sum_2017_2018_576 == 1 | sum_2017_2018 == 1), connect(ascending) lcolor("105 105 105") lwidth(vvthin) xlab(2017(1)2018)  ytit("") xtit(" ") xla(none)  xscale(alt) plotregion(lcolor(black))), yline(576, lwidth(thin) lcolor(black)) yline(512, lwidth(thin)  lcolor(black)) legend(off) graphregion(color(white)) xsize(3) saving(line, replace)

gr export "${output}/figure_2_line.png", width(4000) replace

restore


//////////////////////////////////////////////////////////////////////////

  // FIGURE 2.
  // SCATTER OF DISEASE BURDEN
/////////////////////////////////////////////////////////////////////////
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

/////////////////////////////////////////////////////////////////////////////
 // FIGURE 3.

 // LOWESS ON COVERAGE AND NEED BY INDICATOR
/////////////////////////////////////////////////////////////////////////////

   use "${constructed}/qbs-domainii_clean.dta", clear

 local indicators     diab_monitor   ///
              diab_treat     ///
              hyp1_monitor   ///
              hyp2_monitor   ///
              hyp3_monitor   ///
              hyp1_treat     ///
              hyp2_treat     ///
              infarction     ///
              infarction_treat1      ///
              infarction_treat2     ///
              hypothyreosis

  local  diab_monitor_title    "Diabetes Type II: Monitoring"
  local  diab_treat_title     "Diabetes Type II: Treatment"
  local  hyp1_monitor_title     "Hypertension Low Risk: Monitoring"
  local  hyp2_monitor_title    "Hypertension Med Risk: Monitoring"
  local  hyp3_monitor_title     "Hypertension High Risk: Monitoring"
  local  hyp1_treat_title      "Hypertension All Risk: Treatment"
  local  hyp2_treat_title     "Hypertension Med-High Risk: Treatment"
  local  infarction_title      "Myocardial Infarction: Monitoring"
  local  infarction_treat1_title   "Myocardial Infarction Beta Blockers: Treatment"
  local  infarction_treat2_title  "Myocardial Infarction Statins: Treatment"
  local  hypothyreosis_title    "Hypothyroidism: Monitoring"


  local yline_diab_monitor    76
  local yline_diab_treat      70
  local yline_hyp1_monitor      76
  local yline_hyp2_monitor      73
  local yline_hyp3_monitor      70
  local yline_hyp1_treat      90
  local yline_hyp2_treat      83
  local yline_infarction       90
  local yline_infarction_treat1    70
  local yline_infarction_treat2   70
  local yline_hypothyreosis      90


  foreach i of local indicators  {

    lowess coveragert_`i'  tgtgroup_`i' if coveragert_`i', yline(`yline_`i'', lcolor(black)) msize(0.7) mcolor(grey%30) mlwidth(none) lineopts(lcolor(red) lwidth(thick)) ylab(0(20)100, angle(0) nogrid)    ///
    tit("``i'_title'", size(medsmall) color(black)) xtit("") ytit("") note("")     ///
    subtit("``i'_sub'")    ///
    graphregion(color(white)) saving(`i'.gph, replace)

    }


    gr combine       diab_monitor.gph ///
              diab_treat.gph     ///
              hyp1_monitor.gph   ///
              hyp2_monitor.gph   ///
              hyp3_monitor.gph    ///
              hyp1_treat.gph     ///
              hyp2_treat.gph     ///
              infarction.gph     ///
              infarction_treat1.gph     ///
              infarction_treat2.gph     ///
              hypothyreosis.gph, col(3) xsize(8) ysize(8) graphregion(color(white))


      gr export "${output}/fig_4_lowess_indicator.pdf", replace

//////////////////////////////////////////////////////////////////////////////
    // FIGURE 4.

    // JS and NEED-BASED ESTIMATOR

    // Currently in 2_need_based_estimation.do, will move after finalizing
/////////////////////////////////////////////////////////////////////////////
use "${constructed}/qbs_shrinkage.dta", clear

  // Local for indicator names
  local indicators ///
    diab_monitor ///
    diab_treat ///
    hyp1_monitor   ///
    hyp2_monitor   ///
    hyp3_monitor   ///
    hyp1_treat     ///
    hyp2_treat     ///
    infarction     ///
    infarction_treat1   ///
    infarction_treat2     ///
    hypothyreosis
    
  // Graph
  foreach i of local indicators {
    
    local label : var label `i'_coveragert
    local label = subinstr("`label'"," - Coverage Ratio","",.)

    tw ///
      (scatter `i'_js `i'_coveragert ///
        , mlwidth(none) msize(0.6) mcolor(black%60) mlc(none)) ///
      (scatter `i'_nb `i'_coveragert ///
        , mlwidth(none) msize(0.6) mcolor(red%60) mlc(none)) ///
      (function x , lp(dash) lw(thin) lc(gray)) ///
    , xtit("Raw Score") legend(on order(1 "James-Stein" 2 "Need-Based")) ///
      title("`label'") nodraw ///
      xlab(0 "0%" .25 "25%" .5 "50%" .75 "75%" 1 "100%") ///
      ylab(0 "0%" .25 "25%" .5 "50%" .75 "75%" 1 "100%")
      
    graph save "${output}/`i'.gph" , replace
    local graphs "`graphs' ${output}/`i'.gph"
    
   }

 grc1leg `graphs' , altshrink ysize(8)


/////////////////////////////////////////////////////////////////////////////
  // FIGURE 5.

  // COVERAGE AND WEIGHT OF INDICATORS
/////////////////////////////////////////////////////////////////////////////

use "${constructed}/qbs-domainii_clean.dta", clear

  label var coveragert_diab_treat      "Type-II Diabetes (treat) [12]"
  label var coveragert_infarction_treat2   "Myocardial infarction statins (treat) [19]"
  label var coveragert_infarction_treat1   "Myocardial infarction beta blockers (treat) [14]"
  label var coveragert_hyp2_monitor       "Hypertension med risk (monitor) [68]"
  label var coveragert_hyp2_treat       "Hypertension med-high risk (treat) [22]"
  label var coveragert_hyp3_monitor     "Hypertension high risk: (monitor) [66]"
  label var coveragert_diab_monitor    "Type-II Diabetes (monitor) [68]"
  label var coveragert_infarction       "Myocardial infarction (monitor) [68]"
  label var coveragert_hypothyreosis     "Hypothyroidism (monitor) [63]"
  label var coveragert_hyp1_treat       "Hypertension all risk (treat) [22]"
  label var coveragert_hyp1_monitor     "Hypertension low risk (monitor) [66]"



  graph hbox     coveragert_diab_treat       coveragert_infarction_treat1   ///
          coveragert_infarction_treat2   coveragert_hyp2_treat       ///
          coveragert_hyp1_treat       coveragert_hypothyreosis     ///
          coveragert_hyp3_monitor      coveragert_hyp1_monitor     ///
          coveragert_infarction       coveragert_diab_monitor     ///
          coveragert_hyp2_monitor ,                     ///
          noout  ascat ylab(0 20 40 60 80 100, nogrid) ytit("") box(1,bfcolor(white) lcolor(black)) graphregion(color(white) lcolor(black))   ///
          tit("") note("")

  gr display, xsize(8)

gr export "${output}/fig_5_pca_weights.png", replace

/////////////////////////////////////////////////////////////////////////////
// FIGURE 6.

// FINAL SCORES PANEL
/////////////////////////////////////////////////////////////////////////////

use "${constructed}/qbs_shrinkage.dta", clear


gen total_den = tgtgroup_diab_monitor     +       ///
            tgtgroup_diab_treat         +      ///
            tgtgroup_hyp1_monitor       +       ///
            tgtgroup_hyp2_monitor       +       ///
            tgtgroup_hyp3_monitor       +       ///
            tgtgroup_hyp1_treat       +       ///
            tgtgroup_hyp2_treat       +       ///
            tgtgroup_infarction       +       ///
            tgtgroup_infarction_treat1     +       ///
            tgtgroup_infarction_treat2     +       ///
            tgtgroup_hypothyreosis

    gen total_num = covered_diab_monitor     +       ///
            covered_diab_treat         +       ///
            covered_hyp1_monitor       +       ///
            covered_hyp2_monitor       +       ///
            covered_hyp3_monitor       +       ///
            covered_hyp1_treat         +       ///
            covered_hyp2_treat         +       ///
            covered_infarction         +       ///
            covered_infarction_treat1     +       ///
            covered_infarction_treat2     +       ///
            covered_hypothyreosis

    gen total_cov = total_num/total_den




  gen y = .
  replace y = 0   if _n == 1
  replace y = 384 if _n == 2
  replace y = 432 if _n == 3
  replace y = 602 if _n == 4

  // Create points to get shaded area of the graph

  gen incentive_1  =  384
  gen incentive_2 =   432
  gen max_points  =  602
  gen max_domain  = 480
  gen base    =  0

  gen xpoint1     = .
  replace xpoint1  =  0   if _n == 1
  replace xpoint1  =  1000 if _n == 2
  replace xpoint1 =  2000 if _n == 3
  replace xpoint1 =   8000 if _n == 4

  // NEW SCORES = New coverage ratio * Weight from PCA

  tw /// Raw scores
      (area max_points max_domain incentive_2 incentive_1 base xpoint1,     ///
      color("143 188 143" "143 188 143" "100 149 237" "255 160 122" )     ///
      lcolor(black black black black) lwidth(thin thin thin thin))       ///
      (function y=602, range(0 8000) dropl(4000) base(0) n(1) color(black))  ///
    (scatter total_points_domain total_den if total_den <= 8000, msize(0.3)   ///
    yline(0, lcolor(black)) yline(384, lcolor(black)) yline(432, lcolor(black))  yline(480, lcolor(black))   ///
    mcolor(black) yscale(lstyle(none)) xscale(lstyle(none)) xla(none) legend(off)   ///
    yla(0 " 0 " 384 " 384 " 432 " 432 " 480 " 480 " , labsize(medium) angle(0) labgap(4)) xtit(" ")  ///
    ytit("") xscale(alt) yscale(alt) graphregion(color(white))), saving(scatter_orig, replace)


    tw /// Partial credit
      (area max_points max_domain incentive_2 incentive_1 base xpoint1,     ///
      color("255 255 255" "143 188 143" "100 149 237" "255 160 122" )     ///
      lcolor(black black black black) lwidth(thin thin thin thin))       ///
      (function y=602, range(0 8000) dropl(4000) base(0) n(1) color(black))  ///
    (scatter total_prop_score total_den if total_den <= 8000, msize(0.3)   ///
    yline(0, lcolor(black)) yline(384, lcolor(black)) yline(432, lcolor(black))  yline(480, lcolor(black))   ///
    mcolor(black) yscale(lstyle(none)) xscale(lstyle(none)) xla(none) legend(off)   ///
    yla(0 " 0 " 384 " 384 " 432 " 432 " 480 " 480 " , labsize(med) angle(0) labgap(4)) xtit(" ")  ///
    ytick( 0 384 432 480) ytit("") xscale(alt) yscale(alt) graphregion(color(white))), saving(scatter_prop, replace)


    tw /// PCA + Shrinkage
      (area max_points max_domain incentive_2 incentive_1 base xpoint1,     ///
      color("255 255 255" "143 188 143" "100 149 237" "255 160 122" )     ///
      lcolor(black black black black) lwidth(thin thin thin thin)) ///
      (function y=602, range(0 8000) dropl(4000) base(0) n(1) color(black))  ///
    (scatter new_pca_score total_den if total_den <= 8000, msize(0.3)       ///
    yline(0, lcolor(black)) yline(384, lcolor(black)) yline(432, lcolor(black)) yline(480, lcolor(black)) ///
    mcolor(black) yscale(lstyle(none)) xscale(lstyle(none)) xla(none)  legend(off) ///
    yla(none) xtit(" ")  ytick(0 384 432 480) ytit("")  xscale(alt)       ///
    graphregion(color(white))), saving(scatter_new, replace)


    gr combine scatter_orig.gph scatter_prop.gph scatter_new.gph, col(3) imargin(1) xsize(10) graphregion(color(white)) xcommon ycommon

    gr export "${output}/fig_6_combined_scatter.png", replace




//////////////// END OF DO FILE ////////////////////////////////////////////
