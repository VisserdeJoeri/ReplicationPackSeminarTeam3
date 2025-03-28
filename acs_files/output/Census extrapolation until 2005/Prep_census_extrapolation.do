clear
//load 2000 census data
use "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/2000_clean.dta"

keep wbho sex age perwt year educ edclass

// Get age group
egen age_gp = cut(age), at(5(5)100)
keep if inrange(age_gp, 25, 70)

//get sum by all cells 
collapse (sum) perwt, by(sex wbho age_gp edclass`edtype' year)

tempfile acs
save `acs' , replace

/* expand to get totals by race and sex */
collapse (sum) perwt, by(sex age_gp edclass`edtype' year)

gen wbho = 0
append using `acs'
save `acs' , replace

/* get total by sex */ 
collapse (sum) perwt, by(wbho age_gp edclass`edtype' year)
gen sex = 0
append using `acs'

//rename the column 'perwt' to 'inst'
ren perwt inst 
save `acs', replace 


append using "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/inst_pops (1).dta"

// Add rows for the years 2001-2005, where inst is empty such that we can use the extrapolation
levelsof wbho, local(wbho_levels)
levelsof sex, local(sex_levels)
levelsof edclass`edtype', local(edclass_levels)
levelsof age_gp, local(age_gp_levels)

local years = "2001 2002 2003 2004 2005"

foreach wbho in `wbho_levels' {
    foreach sex in `sex_levels' {
        foreach edclass in `edclass_levels' {
            foreach age_gp in `age_gp_levels' {
                foreach year in `years' {
                   
                    set obs `=_N + 1'
                    replace wbho = `wbho' in `=_N'
                    replace sex = `sex' in `=_N'
                    replace edclass = `edclass' in `=_N'
                    replace age_gp = `age_gp' in `=_N'
                    replace year = `year' in `=_N'
                    replace inst = . in `=_N'  
                }
            }
        }
    }
}



// Use this to check if the file was appended correctly
//save "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/appended_inst_pops.dta"

// Set missing 'inst' values to 0
replace inst = 0 if inst == .

//Create temporary variables for the extrapolation
foreach year in 2000 2006 {
    gen inst_`year'_tmp = inst if year == `year'
    bys wbho sex edclass age_gp: egen inst_`year' = mean(inst_`year'_tmp)
}

//Sort the table
sort wbho sex edclass age_gp year

//Do the extrapolation for 2001-2005
bys wbho sex edclass age_gp: replace inst = (year - 2000) * (inst_2006 - inst_2000) / (2006 - 2000) + inst_2000 if inrange(year,2001,2005)

// Delete temporary variables
drop *tmp

// Save the file
keep if inrange(year,1992,2018)
save "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/extrapolated_inst_pops_2000_2005.dta", replace