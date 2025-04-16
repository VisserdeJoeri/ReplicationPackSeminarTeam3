capture program drop prep_inst
program define prep_inst
    syntax, [edtype(string) dropstates granage] 

    /*************/
    /* PREP ACS  */
    /*************/       
    clear

    forv i = 2006/2018 {
        append using $mdata/int/acs_prepped/cepr_acs_`i'.dta, ///
            keep(wbho female age civnon type perwgt year educ state)
    }

    /* Drop states if needed (disabled for now) */
    // if "`dropstates'" != "" {
    //     drop if inlist(state,13,40,44,46)
    // }

    /* Keep only institutionalized population */
    keep if type == 2 & inrange(year, 2006, 2018)

    /* Create sex variable from female (1 = male, 2 = female) */
    gen sex = female + 1

    /* Create education class (2-bin) */
    gen edclass = min(educ, 4)

    /* Optional: 3-bin education class if edtype == _3bin */
    gen edclass_3bin = edclass - 1 
    replace edclass_3bin = 1 if edclass_3bin == 0

    /* Handle age grouping */
    if "`granage'" != "" {
        local age_gp = "age"
    }
    else {
        egen age_gp = cut(age), at(5(5)100)
        local age_gp = "age_gp"
    }

    /* Limit to working-age adults */
    keep if inrange(`age_gp', 25, 70)

    /* Collapse by full detail: sex, race, age group, education, year */
    collapse (sum) perwgt, by(sex wbho `age_gp' edclass`edtype' year)
    tempfile acs
    save `acs', replace 

    /* Collapse by race+sex */
    collapse (sum) perwgt, by(sex `age_gp' edclass`edtype' year)
    gen wbho = 0
    append using `acs'
    save `acs', replace 

    /* Collapse by sex only */
    collapse (sum) perwgt, by(wbho `age_gp' edclass`edtype' year)
    gen sex = 0
    append using `acs'

    /* Final formatting */
    rename perwgt inst
    keep if inrange(year, 2006, 2018)

    save $mdata/int/inst_pops`edtype'`dropstates'`granage', replace

end

prep_inst
prep_inst, edtype(_3bin)
prep_inst, granage
prep_inst, edtype(_3bin) granage
