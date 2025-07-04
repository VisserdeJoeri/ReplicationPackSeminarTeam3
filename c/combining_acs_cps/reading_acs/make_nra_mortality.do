/* SEE STEPS 1 AND 2 BELOW TO SET UP REPLICATION */
clear all

//* ssc install things we need */
//foreach v in savesome listtex xfill unique {
//  ssc install `v'
//}

/* STEP 1: SET THE FOLLOWING GLOBALS:
$out: path for output files to be created
mdata: path to data [intermediate data files will be put here too] */
global out /Users/Stata/out
global tmp /Users/Stata/temp
global mdata /Users/Stata/data

if mi("$out") | mi("$tmp") | mi("$mdata") {
  display as error "Globals 'out', 'tmp', and 'mdata' must be set for this to run."
  error 1
}

/* STEP 2: SET MATLAB PATHS SIMILARLY IN matlab/set_matlab_paths.m */


/* load Stata programs */
qui do tools.do
qui do masala-merge/masala_merge
qui do stata-tex/stata-tex

/* load mortality programs (chiefly bound_mort and helpers) */
do mortality_programs

/* add ado folder to adopath */
adopath + ado

/* start logging */
cap log close
log using $out/nra_mortality.log, text replace

/* store current path (assumed to be repo root folder) */
global mcode /Users/Stata

/* create new folders */
cap mkdir $mdata/tmp
foreach f in bounds nchs nhis matlab_inputs nhis/clean {
  cap mkdir $mdata/int/`f'
}

/*********************************************************/
/* 1. Build mortality file from restricted NCHS data     */
/*********************************************************/
// Line commented out since it requires restricted data
do $mcode/make_mortality_data

/*********************/
/* 2. Prep NHIS data */
/*********************/
// do $mcode/make_nhis

/***************************************/
/* 3. Prepare inputs for Matlab solver */
/***************************************/

/* prepare the inputs for the matlab mort-solver */
//do $mcode/b/prep_matlab_inputs.do

/********************************************/
/* 4. Run all versions of the Matlab solver */
/********************************************/
//do $mcode/run_matlab_solver.do

/************************************************************/
/* 5. Process solver output and generate all paper results  */
/************************************************************/
//do $mcode/make_results.do

