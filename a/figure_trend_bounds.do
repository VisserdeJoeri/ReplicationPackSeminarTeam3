/* open Matlab solver results */
import delimited using "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/bounds/mort_bounds_complete_parallel.csv", clear

//ssc install grc1leg
/* keep the standard time series spec only */
keep if ranktype == "sex" & spec == "mon" & target_name == "10-45-70" & f2 < 1

/* drop extraneous variables and sort for easy listing */
drop ranktype spec target_name f2
sort sex race edclass year

/* define looping categories */
global sex_list 1 2
global race_list 1 2 3 5
global ageset 25(5)65

/* create a temporary sample variable */
gen sample = .

/* replace year with year + 1 so it shows the midpoint */
replace year = year + 1

//global ageset 60(1)60

/* loop over cause, age, sex, and race */
qui foreach cause in t d n x{
  forval age = $ageset {
    local age_p4 = `age' + 4
    foreach sex in $sex_list {
      foreach race in $race_list {

        /* define graph labels */
        local sex1 "Men"
        local sex2 "Women"
        local race1 White
        local race2 Black
        local race3 Hispanic
        local race4 Other

        replace sample = sex == `sex' & race == `race' & age == `age' & cause == "`cause'"
        /* note we need to add redundant top and bottom lines because Stata/16 ignores rarea:lwidth() */
        twoway ///
            (rarea mu_lb mu_ub year if sample & edclass == 1, lwidth(medthick) cmissing(no) color("0 0 0"))       ///
            (rarea mu_lb mu_ub year if sample & edclass == 2, lwidth(medthick) cmissing(no) color("153 61 61"))   ///
            (rarea mu_lb mu_ub year if sample & edclass == 3, lwidth(medthick) cmissing(no) color("191 85 85")) ///
            (rarea mu_lb mu_ub year if sample & edclass == 4, lwidth(medthick) cmissing(no) color("217 112 112")) ///
            (line  mu_lb       year if sample & edclass == 1, lwidth(medthick)              color("0 0 0"))       ///
            (line  mu_lb       year if sample & edclass == 2, lwidth(medthick)              color("122 49 49"))   ///
            (line  mu_lb       year if sample & edclass == 3, lwidth(medthick)              color("160 70 70")) ///
            (line  mu_lb       year if sample & edclass == 4, lwidth(medthick)              color("180 90 90")) ///
            (line        mu_ub year if sample & edclass == 1, lwidth(medthick)              color("0 0 0"))       ///
            (line        mu_ub year if sample & edclass == 2, lwidth(medthick)              color("122 49 49"))   ///
            (line        mu_ub year if sample & edclass == 3, lwidth(medthick)              color("160 70 70")) ///
            (line        mu_ub year if sample & edclass == 4, lwidth(medthick)              color("180 90 90")) ///
            , xlabel(2000(5)2020) xtitle("Year") ytitle("Deaths per 100,000") title("`race`race'' `sex`sex'', Age `age'-`age_p4'", size(medsmall) pos(12) ring(0)) ///
            legend(order(1 2 3 4) symxsize(5) symysize(0.5) size(vsmall) subtitle("Education Percentile", size(small) pos(12)) rows(2) lab(1 "0-10") lab(2 "10-45") lab(3 "45-70") lab(4 "70-100")) name(mon_bounds_robust_`sex'_`race'_`age', replace) plotregion(margin(zero)) 
      }
    }
    grc1leg mon_bounds_robust_2_3_`age'  mon_bounds_robust_1_3_`age' mon_bounds_robust_2_3_`age' mon_bounds_robust_1_3_`age'  , rows(2) xcommon ycommon
	graph export "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/smooth graphs robust/trend-smooth-mon-step-robust-`cause'-sex-`age'.png", width(2000) replace

  }
}
