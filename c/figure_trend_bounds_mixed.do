/* open Matlab solver results */
import delimited using "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/bounds/mort_bounds_complete_parallel.csv", clear

/* keep the standard time series spec only */
keep if ranktype == "sex" & spec == "mon" & target_name == "10-45-70" & f2 < 1

/* drop extraneous variables and sort for easy listing */
drop ranktype spec target_name f2
sort sex race edclass year

/* Define looping categories */
global sex_list 1 2
global race_list 1 2
global edclass_list 1 2 3 4
global ageset 25(5)65


/* Create a temporary sample variable */
gen sample = .

/* Replace year with year + 1 so it shows the midpoint */
replace year = year + 1


/* Loop over cause, age, sex, and race */
qui {
  foreach sex in $sex_list {
      foreach race in $race_list {
	  	foreach edclass in $edclass_list {
			forval age = $ageset {
				local age_p4 = `age' + 4

		local sexlbl = cond(`sex' == 1, "Men", "Women")
          local racelbl = cond(`race' == 1, "White", cond(`race' == 2, "Black", "Other"))
		  
		            local edlbl = cond(`edclass' == 1, "0–10", ///
                        cond(`edclass' == 2, "10–45", ///
                        cond(`edclass' == 3, "45–70", ///
                        "70–100")))
		preserve
        keep if sex == `sex' & race == `race' & edclass == `edclass' & age == `age'
		        /* define graph labels */
//         local sex1 "Men"
//         local sex2 "Women"
//         local race1 White
//         local race2 Black
//
//         replace sample = sex == `sex' & race == `race' & age == `age' & cause == "`cause'"

        gen line_id = ""
        replace line_id = "t" if cause == "t"
        replace line_id = "d" if cause == "d"
        replace line_id = "n" if cause == "n"
		replace line_id = "x" if cause == "x"
        /* note we need to add redundant top and bottom lines because Stata/16 ignores rarea:lwidth() */
        twoway ///
            (rarea mu_lb mu_ub year if cause == "t", lwidth(medthick) cmissing(no) color("0 0 0"))       ///
            (rarea mu_lb mu_ub year if cause == "d", lwidth(medthick) cmissing(no) color("0 150 180"))   ///
            (rarea mu_lb mu_ub year if cause == "n", lwidth(medthick) cmissing(no) color("191 85 85")) ///
			(rarea mu_lb mu_ub year if cause == "x", lwidth(medthick) cmissing(no) color("204 119 34")) ///
            (line  mu_lb       year if cause == "t", lwidth(medthick)              color("0 0 0"))       ///
            (line  mu_lb       year if cause =="d", lwidth(medthick)              color("0 120 160"))   ///
            (line  mu_lb       year if cause == "n", lwidth(medthick)              color("160 70 70")) ///
			(line  mu_lb       year if cause == "x", lwidth(medthick)              color("180 105 30")) ///
            (line        mu_ub year if cause == "t", lwidth(medthick)              color("0 0 0"))       ///
            (line        mu_ub year if cause =="d", lwidth(medthick)              color("0 120 160"))   ///
            (line        mu_ub year if cause == "n", lwidth(medthick)              color("160 70 70")) ///
			(line        mu_ub year if cause == "x", lwidth(medthick)              color("180 105 30")) ///
            , xlabel(2000(5)2020) xtitle("Year") ytitle("Deaths per 100,000") title("`racelbl' `sexlbl', Age `age'-`age_p4', Education `edlbl' percentile", size(medsmall) pos(12) ring(0)) ///
          legend(order(5 "Total" 6 "Despair" 7 "Non-despair" 8 "Non-COVID-19") pos(11) size(small)) ///
          name(mort_plot_`sex'_`race'_`edclass'_`age', replace)
graph export "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/smooth graphs fixed noncov/trend-smooth-sex`sex'-race`race'-ed`edclass'-age`age'.png", width(2000) replace


			restore
	  }
    }


	  }
  }
}