
baseline_path <- "./data/alewife-baseline.csv"
lat_long_path <- "./data/alewife-lat-long.txt"
rep_unit_path <- "./data/alewife-3-grps.txt"
locus_columns <-  14:35


#' main wrapper function that does all the steps of a full analysis
#' 
#' @inheritParams herring_csv2gpiper
#' 
herring_main_func <- function(
  baseline_path,
  lat_long_path, 
  locus_columns
  ) {
  
  # read in data
  baseline.df <- herring_csv2gpiper(baseline_path, lat_long_path, locus_columns)
  
  # make a gsi_sim file of it
  the.pops.f <- make_baseline_file(baseline.df)
  
  # run gsi_sim for self assignments:
  self_ass <- gsi_self_assignment(the.pops.f)
  
}