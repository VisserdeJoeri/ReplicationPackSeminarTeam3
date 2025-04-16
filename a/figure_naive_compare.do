/****************************************************/
/* FIGURE: Naive vs. Bound Comparison (2000–2021)   */
/****************************************************/

/* Assumes globals already defined:
   global out /Users/Stata/out
   global tmp /Users/Stata/temp
   global mdata /Users/Stata/data
*/

/****************************************************/
/* 0. Identify bin boundaries for naive graphs      */
/****************************************************/
import delimited "$mdata/mort/nchs/appended_rank_mort.csv", clear

* Cut years into 2-year bins starting at 2000
egen year_bin = cut(year), at(2000(2)2022)
collapse (mean) tpop_rolling tmort tmortrate dmortrate ed_rank_* cum_ed_rank_*, ///
    by(year_bin age sex race edclass)
rename year_bin year

* Optional inspection
list edclass cum_ed_rank_sex if year == 2000 & sex == 2 & race == 1 & age == 45
list edclass cum_ed_rank_sex if year == 2000 & sex == 2 & race == 1 & age == 50

/****************************************************/
/* 1. Collapse mortality rates into 2-year averages */
/****************************************************/
import delimited "$mdata/mort/nchs/appended_rank_mort.csv", clear
keep year sex race age edclass tmortrate dmortrate cum_ed_rank_sex

egen ycut = cut(year), at(2000(2)2022)
collapse (mean) tmortrate dmortrate cum_ed_rank_sex, by(ycut sex race age edclass)
rename ycut year

* Calculate baseline-relative mortality changes (baseline: 2001)
global group sex race age edclass

gen ttmp = tmortrate if year == 2000
gen dtmp = dmortrate if year == 2000
bys $group: egen tbaserate = max(ttmp)
bys $group: egen dbaserate = max(dtmp)

gen tmort_change = tmortrate / tbaserate
gen dmort_change = dmortrate / dbaserate

sort year
list year tmortrate tmort_change if edclass == 1 & sex == 1 & race == 1 & age == 25
list year dmortrate dmort_change if edclass == 1 & sex == 2 & race == 1 & age == 50

recode race 0 = 5   // Standardize race codes

save "$tmp/rawmort", replace

/****************************************************/
/* 2. Prepare bounds estimates from bounds dataset  */
/****************************************************/
import delimited "$mdata/int/bounds/mort_bounds_all.csv", clear

* Filter usable bounds only: monotonic spec, valid f2, rank by sex
keep if spec == "mon" & f2 < 1 & ranktype == "sex"

* Drop unneeded variables
drop mu_s mu_t

* Rename for consistency with earlier code
rename target_name target

* Keep relevant variables and causes of interest (total and despair)
keep year sex race age edclass mu_lb mu_ub cause target
keep if inlist(cause, "t", "d")

* Collapse to one observation per group-cause by averaging bounds
collapse (mean) mu_lb mu_ub, by(year sex race age edclass target cause)

* Reshape from long to wide on cause
reshape wide mu_lb mu_ub, i(year sex race age edclass target) j(cause) string

* Rename reshaped variables for clarity
rename mu_lbd lbd
rename mu_ubd ubd
rename mu_lbt lbt
rename mu_ubt ubt

* Compute changes vs. 2000 baseline
foreach b in ub lb {
    gen ttmp_`b' = `b't if year == 2000
    gen dtmp_`b' = `b'd if year == 2000
    bys $group target: egen base_`b't = max(ttmp_`b')
    bys $group target: egen base_`b'd = max(dtmp_`b')
}

gen tchange_ub = ubt / base_lbt
gen tchange_lb = lbt / base_ubt
gen dchange_ub = ubd / base_lbd
gen dchange_lb = lbd / base_ubd

sort year
save "$tmp/bounds", replace

/****************************************************/
/* 3. Merge raw and bounds data for plotting        */
/****************************************************/
use "$tmp/bounds", clear
merge m:1 year sex race age edclass using "$tmp/rawmort", keep(match) nogen

* Rescale from ratios to % change
foreach v in tmort_change tchange_lb tchange_ub {
    replace `v' = (`v' - 1) * 100
}

/****************************************************/
/* 4. Plot naive vs. bounded mortality comparisons  */
/*    Women only, Total cause, Edclass 3 and 4      */
/****************************************************/
foreach race in 5 {
  local cause t   // total mortality only
  foreach ed in 1 2 3 4 {
    local sex 2   // women only
    local sexlbl women

    if `race' == 5 local ylabel -25(25)50
    if inlist(`race', 1, 2) local ylabel -50(50)150

    local age = 50
    local target = "10-45-70"   // adjust if using group-specific targets
    local gname naive_`race'_`sexlbl'_`age'_`cause'_`ed'

    twoway ///
      (scatter `cause'mort_change year if target == "`target'" & race == `race' & sex == `sex' & edclass == `ed' & age == `age', msize(medlarge)) ///
      (rcap `cause'change_lb `cause'change_ub year if target == "`target'" & race == `race' & sex == `sex' & edclass == `ed' & age == `age', lwidth(thick)) ///
      , name(`gname', replace) ///
        ylabel(`ylabel', labsize(large)) ///
        xlabel(, labsize(large)) ///
        legend(size(large) ring(0) pos(11) lab(1 "Unadjusted") lab(2 "Constant Rank")) ///
        xtitle("Year", size(large)) ///
        ytitle("% Mortality Change", size(large)) ///
        yline(0, lpattern(dash) lcolor(gs8))

    graph export "$out/naive-`race'-`sexlbl'-`age'-`cause'-`ed'.pdf", replace name(`gname')
  }
}
