/*************************/
/* PROCESS SOLVER OUTPUT */
/*************************/

/* append parallel matlab bounds into a single file */
// do $mcode/b/append_parallel_matlab_bounds.do

/*************************************************************************************/
/* NOTE: EVERYTHING ABOVE HERE SAVES BOUNDS TO mdata/int/bounds                             */
/*       EVERYTHING BELOW HERE LOADS BOUNDS FROM mdata/bounds                               */
/*                                                                                   */
/* This is because we supply mdata/bounds/ as a basis for replication, and the prior */
/* stuff is only there to verify that the bounds are produced correctly.

   However, some intermediate files below continue to be saved to int/ */

/*************************************************************************************/

/* take the matlab output CSV and reshape to a directly graphable dataset */
// do $mcode/b/process_matlab_bounds.do

/*******************************/
/* GENERATE TABLES AND FIGURES */
/*******************************/

// cap erase $out/mort_paper_stats.csv

/* main figures */
// do $mcode/a/figure_scatter_smooth.do
// do $mcode/a/figure_scatter_granular.do
// do $mcode/a/figure_trend_bounds.do
// do $mcode/a/figure_causes.do

/* updated appendix tables and figures */
// do $mcode/a/calc_standardized_rates.do
// do $mcode/a/table_icd_causes.do

/* calculate stats for paper body */
// do $mcode/a/paper_stats.do

/* comparison of naive and bounded mortality estimates for one group */
do $mcode/a/figure_naive_compare.do

/* intuitive graphs explaining how bounds work */
// shell python $mcode/a/graph_intuitive.py

/* CEF graphs walking through method */
// do $mcode/a/plot_mort_cef.do

/* table 2 showing mortality stats under different assumptions */
// do $mcode/a/table_mort_stats

/* APPENDIX FIGURES */
// do $mcode/a/forecast_pop_change_all_cohorts.do
// do $mcode/a/polynomial_cbar.do
// do $mcode/a/graph_hisp_group_size.do
// do $mcode/a/graph_income_ranks.do
// do $mcode/a/graph_mort_hisp_sim.do
// do $mcode/a/graph_nhis_mortality.do 
// do $mcode/a/graph_nhis_self_reported_health.do

/* latex inserts */
// do $mcode/a/export_tex_inserts.do
