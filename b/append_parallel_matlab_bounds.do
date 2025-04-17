/* Specify age and race lists of the datasets to append */
global agelist 25(5)65
global racelist 1 2 3 5

/* loop over age-race and convert everything to stata */
forval age = $agelist {
  foreach race in $racelist {
    di "`age'-`race'"
    
    /* open this age-race file */
    /* note to replicator: this intentionally uses bounds/ and not int/bounds/ -- see makefile
       for explanation */
	   /* Adjust filename accordingly to the format of your datasets */
	   import delimited using "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/bounds/mort_bounds_`age'_`race'.csv", clear

    /* drop ages / races that didn't fit this category -- from a prior parallel bug */
    keep if age == `age' & race == `race'
    
    /* save it in stata format */
    save "C:/Users/maud5/Documents/2024-2025/Seminar/paper data/tmp/mba2_`age'_`race'.dta", replace
  }
}

/* append all the age-race stata files */
clear
forval age = $agelist {
  foreach race in $racelist {
    di "`age'-`race'"
    cap append using "C:/Users/maud5/Documents/2024-2025/Seminar/paper data/tmp/mba2_`age'_`race'.dta"
  }
}

/* export the file as a CSV (since we need to replicate the output of the serial matlab program) */
export delimited using "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/bounds/mort_bounds_complete_parallel.csv", replace

