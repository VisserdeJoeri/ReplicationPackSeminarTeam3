/*************************/
/* PROCESS SOLVER OUTPUT */
/*************************/

/* append parallel matlab bounds into a single file */
/* note for replicators: This reads data from individual matlab runs in $mdata/bounds/ and
   writes a single datafile to $mdata/bounds/int/. This is intentionally NOT reading data
   from $mdata/bounds/int/ (produced by the above lines), because there is minor variation in 
   solver solutions across different matlab machines, so we need to use *our* solver results
   to deliver exactly the outputs in the paper */

/* This code appends all age-race mortality data sets into one complete data set */
do $mcode/b/append_parallel_matlab_bounds.do //gecheckt

/*************************************************************************************/
/* NOTE: EVERYTHING ABOVE HERE SAVES BOUNDS TO mdata/int/bounds                             */
/*       EVERYTHING BELOW HERE LOADS BOUNDS FROM mdata/bounds                               */
/*                                                                                   */
/* This is because we supply mdata/bounds/ as a basis for replication, and the prior */
/* stuff is only there to verify that the bounds are produced correctly.

   However, some intermediate files below continue to be saved to int/ */

/*************************************************************************************/

/* Create smooth graphs of total mortality */
do $mcode/a/figure_trend_bounds.do // geeft nog errors

/* Create smooth mortality graphs of non-COVID-19, total mortality, despair deaths and non-despair deaths */
do "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/figure_trend_bounds_mixed.do"

