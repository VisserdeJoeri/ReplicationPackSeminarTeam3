/************** USER MUST SET THE FOLLOWING PATHS FOR REPLICATION
$out: path for output files to be created
mdata: path to data [intermediate data files will be put here too] */
global out "C:/Users/maud5/Documents/2024-2025/Seminar/paper data/out" /****change path*****/
global tmp "C:/Users/maud5/Documents/2024-2025/Seminar/paper data/tmp" /****change path*****/
global mdata "C:/Users/maud5/Documents/GitHub/paper-nra-mortality" /****change path*****/

/* REPLICATOR SHOULD NOT NEED TO CHANGE ANYTHING BELOW THIS POINT */
clear all
set more off

if mi("$out") | mi("$tmp") | mi("$mdata") {
  display as error "Globals 'out', 'tmp', and 'mdata' must be set for this to run."
  error 1
}

file open myfile using "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/matlab/set_basepath.m", write replace /****change path*****/
file write myfile "base_path = '$mdata';"
file close myfile

/* load Stata programs */
qui do "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/tools.do" /****change path*****/
qui do "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/masala-merge/masala_merge.do" /****change path*****/
qui do "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/stata-tex/stata-tex" /****change path*****/

/* load mortality programs (chiefly bound_mort and helpers) */
do "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/mortality_programs.do" /****change path*****/

/* add ado folder to adopath */
adopath + ado

/* start logging */
cap log close
log using "C:/Users/maud5/Documents/2024-2025/Seminar/paper data/out/newnewnewtest.log", text replace /****change path*****/

/* provide some info about how and when the program was run */
/* See https://www.stata.com/manuals13/pcreturn.pdf#pcreturn */
local variant = cond(c(MP),"MP",cond(c(SE),"SE",c(flavor)) )  
// alternatively, you could use 
// local variant = cond(c(stata_version)>13,c(real_flavor),"NA")  

di "=== SYSTEM DIAGNOSTICS ==="
di "Stata version: `c(stata_version)'"
di "Updated as of: `c(born_date)'"
di "Variant:       `variant'"
di "Processors:    `c(processors)'"
di "OS:            `c(os)' `c(osdtl)'"
di "Machine type:  `c(machine_type)'"
di "=========================="

/* set root folder for running to current path */
local pwd : pwd
global rootdir "`pwd'"

/* tell stata where to find packages */
capture mkdir "$rootdir/ado"
sysdir set PERSONAL "$rootdir/ado/personal"
sysdir set PLUS     "$rootdir/ado/plus"
sysdir set SITE     "$rootdir/ado/site"
sysdir



/* store current path (assumed to be repo root folder) */
global mcode = "C:/Users/maud5/Documents/GitHub/paper-nra-mortality" /****change path*****/


 cap mkdir "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/tmp" /****change path*****/
 foreach f in bounds nchs nhis matlab_inputs nhis/clean {
 	cap mkdir "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/`f'" /****change path*****/
}


/* Previous steps to prep input data are in GitHub */

/********************************************/
/* 4. Run all versions of the Matlab solver */
/********************************************/

do $mcode/run_matlab_solver.do

/************************************************************/
/* 5. Generate paper results  */
/************************************************************/

//do $mcode/make_results.do

