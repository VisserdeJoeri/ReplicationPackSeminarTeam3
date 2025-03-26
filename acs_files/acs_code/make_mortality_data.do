/**************************************************/
/* BUILD COMBINED CENSUS/ACS/NCHS MORTALITY RATES */
/**************************************************/
/* build acs data */
// For 2006-2018
//do $mcode/b/cepr_acs_do_1.5/cepr_acs_master.do

/* reset working directory */
//#delimit cr 
//cd $mcode 
//do $mcode/b/prep_nchs_mortality // NCHS data

#delimit cr 
// individual CPS, not needed in our case
//do $mcode/b/prep_cps  

#delimit cr
// LOOK INTO PROGRAMS BELOW

//do $mcode/b/raw_census_1990.do
//do $mcode/b/raw_census_2000.do
//do $mcode/b/prep_census_1990.do
//do $mcode/b/prep_census_2000.do

#delimit cr 
//do $mcode/b/prep_mort_rates_all.do

#delimit cr
// CPS / CENSUS / ACS into 1 file
do $mcode/b/prep_institutionalized.do

/* create core mortality file (appended_rank_mort) */
//do $mcode/b/mort_moments.do

