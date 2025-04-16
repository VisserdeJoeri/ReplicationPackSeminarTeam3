/*
Prepares CPS data only (no ACS). 
This version is stripped of all ACS-related parts.
*/

capture program drop prep_inst
capture program define prep_inst
syntax, [edtype(string) dropstates granage] 

    // Load CPS microdata (March supplement, 1980–2018)
    use $mdata/int/cps_march_inc_8018.dta, clear
    keep if inrange(year, 1990, 2018)

    // Drop states if flag is set
    if "`dropstates'" != "" {
        rename state State 
        merge m:1 State using $mdata/raw/misc/STCROSS.dta
        drop if (fipsstr == 13 | fipsstr == 40 | fipsstr == 44 | fipsstr == 46)
    }

    // Set age variable (granular or grouped)
    if "`granage'" != "" { 
        local age_gp = "age"
    }
    if "`age_gp'" == "" {
        local age_gp = "age_gp"
    }

    // Create age group
    egen age_gp = cut(age), at(5(5)100)
    keep if inrange(age_gp, 25, 70)

    // Recode sex
    gen sex = female + 1 

    // Sum weights by group
    collapse (sum) wgt, by(sex wbho `age_gp' edclass`edtype' year) 

    tempfile cps
    save `cps', replace 

    // Totals by race and sex
    collapse (sum) wgt, by(sex `age_gp' edclass`edtype' year)
    gen wbho = 0
    append using `cps'
    save `cps', replace 

    // Totals by sex
    collapse (sum) wgt, by(wbho `age_gp' edclass`edtype' year)
    gen sex = 0
    append using `cps'

    rename wgt non_inst

	// Save permanently to desired folder
	save		"/Users/Stata/SeminarHealthSociety/cps_aggregated_excel/cps_output_files/cps_processed`edtype'`dropstates'`granage'.dta", replace

	
end

// Run the program with different options as needed
prep_inst, granage
prep_inst
