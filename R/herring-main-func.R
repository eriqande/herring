
baseline_path <- "./inst/data_files/alewife-baseline.csv"
lat_long_path <- "./inst/data_files/alewife-lat-long.txt"
rep_unit_path <- "./inst/data_files/alewife-3-grps.txt"
locus_columns <-  14:35
bycatch_path <- "./inst/data_files/alewife-bycatch.csv"
bycatch_locus_columns <- 15:36


baseline_path <- "./inst/data_files/blueback-baseline.csv"
lat_long_path <- "./inst/data_files/blueback-lat-long.txt"
rep_unit_path <- "./inst/data_files/blueback-4-grps.txt"
locus_columns <-  14:39



#' function to return parameter settings for herring_all_analysis() for blueback
#' 
#' this returns parameter settings as a list so you can say do.call(herring_all_analyses, args = blueback_run_settings())
#' @export
blueback_run_settings <- function() {
  list(
    baseline_path = file.path(system.file("data_files", package = "herring", mustWork = T), "blueback-baseline.csv"),
    lat_long_path = file.path(system.file("data_files", package = "herring", mustWork = T), "blueback-lat-long.txt"),
    rep_unit_path = file.path(system.file("data_files", package = "herring", mustWork = T), "blueback-4-grps.txt"),
    locus_columns =  14:39,
    bycatch_path = file.path(system.file("data_files", package = "herring", mustWork = T), "blueback-bycatch.csv"),
    bycatch_locus_columns = 15:40
  )
}



#' function to return parameter settings for herring_all_analysis() for alewife
#' 
#' this returns parameter settings as a list so you can say do.call(herring_all_analyses, args = alewife_run_settings())
#' @export
alewife_run_settings <- function() {
  list(
    baseline_path = file.path(system.file("data_files", package = "herring", mustWork = T), "alewife-baseline.csv"),
    lat_long_path = file.path(system.file("data_files", package = "herring", mustWork = T), "alewife-lat-long.txt"),
    rep_unit_path = file.path(system.file("data_files", package = "herring", mustWork = T), "alewife-3-grps.txt"),
    locus_columns =  14:35,
    bycatch_path = file.path(system.file("data_files", package = "herring", mustWork = T), "alewife-bycatch.csv"),
    bycatch_locus_columns = 15:36
  )
}


#' main wrapper for doing all the analyses
#' 
#' @export
herring_all_analyses <- function(
                                 baseline_path,
                                 lat_long_path,
                                 rep_unit_path,
                                 locus_columns,
                                 bycatch_path,
                                 bycatch_locus_columns) {
  
  # do the baseline assessment and capture the baseline_df as output.
  baseline_stuff <- herring_main_baseline_assess_func(baseline_path,
                                                   lat_long_path,
                                                   rep_unit_path,
                                                   locus_columns)
  
  # now do the bycatch analysis unstratified
  bycatch_lumped_together <- herring_main_bycatch_analyses(bycatch_path, 
                                                           bycatch_locus_columns, 
                                                           baseline_stuff$baseline, 
                                                           rep_unit_path,
                                                           stratifyOn = character(0))
  
  # now, do the bycatch analyses with things stratified
  bycatch_output <- herring_main_bycatch_analyses(bycatch_path, 
                                                  bycatch_locus_columns, 
                                                  baseline_stuff$baseline, 
                                                  rep_unit_path)
  
  
  list(baseline_assessment = baseline_stuff, bycatch_output = bycatch_output, bycatch_lumped_together = bycatch_lumped_together)
  
}



#' main wrapper function that does all the steps for assessing the baseline
#' 
#' @inheritParams herring_csv2gpiper
#' 
#' @export
herring_main_baseline_assess_func <- function(
  baseline_path,
  lat_long_path,
  rep_unit_path,
  locus_columns
  ) {
  
  # read in data
  baseline.df <- herring_csv2gpiper(baseline_path, lat_long_path, locus_columns)
  
  # make a gsi_sim file of it
  the.pops.f <- make_baseline_file(baseline.df)
  
  # run gsi_sim for self assignments.  the result is a list with $from_pop_to_pop, $from_pop_to_rg, and $from_rg_to_rg
  self_ass <- gsi_self_assignment(the.pops.f, rep_unit_path)
  
  # make some barplots.  Currently these does not save them.  But I will add that later.
  # simple barplot to rep unit:
  simple_barplot(self_ass$from_pop_to_rg$Cutoff_0$AssTable)
  
  # shade gradient barplot to pops
  to_pops_barplot(self_ass$from_pop_to_pop$Cutoff_0$AssTable, self_ass$rep_units)
  
  # return the baseline data and the reporting units and the populations as well
  list(baseline = baseline.df, rep_units = self_ass$rep_units, the.pops.f = the.pops.f)
}


#' all the bycatch related steps wrapped up in this function
#' 
#' I am going to add parameters and stuff to this later.  Just kine of a stub at this point. 
#' @export
herring_main_bycatch_analyses <- function(bycatch_path, bycatch_locus_columns, baseline.df, rep_unit_path, stratifyOn = c("Year", "Season", "Region", "Target.Fishery", "Gear.Type")) {
  
  # get all the bycatch taken care of and put in a big list
  bycatch_list <- read_and_stratify_bycatch(bycatch_path, bycatch_locus_columns = bycatch_locus_columns, stratifyOn = stratifyOn)
  
  
  # now, put the baseline loci in the correct order (i.e. so they line up with the locus order in the bycatch files)
  byc.loci <- make.unique(names(bycatch_list[[1]])[bycatch_locus_columns])
  names(baseline.df) <- make.unique(names(baseline.df))  # make the baseline loci have corresponding names
  baseline_reordered <- baseline.df[, byc.loci]
  names(baseline_reordered)[c(F,T)] <- names(baseline_reordered)[c(T,F)]
  
  
  lapply(bycatch_list, function(x)
    do_mixture_analysis(x, bycatch_locus_columns, baseline_reordered, rep_unit_path)
  )
    
}