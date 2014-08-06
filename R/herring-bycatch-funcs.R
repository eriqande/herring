


#' reads the bycatch into a data frame and makes each stratum a data frame in a list
#' 
#' @param bycatch_path  The path to the bycatch file.
#' @param dropBelow  Any stratum with fewer than dropBelow fish will be dropped from consideration
#' @param stratifyOn A character vector naming the columns holding the factors that determine the strata to 
#' stratify the data into
#' @param bycatch_locus_columns integer vector with the columns in the original CSV \emph{bycatch} file that have the loci 
#' that you want to use.  Should be twice as many columns as loci.
#' @export
read_and_stratify_bycatch <- function(bycatch_path, 
                          dropBelow = 5, 
                          stratifyOn = c("Year", "Season", "Region", "Target.Fishery", "Gear.Type"),
                          bycatch_locus_columns
                          ) {
  
  # read the data
  x <- read.csv(bycatch_path)
  
  # put locus names on each column
  names(x)[bycatch_locus_columns][c(F,T)] <- names(x)[bycatch_locus_columns][c(T,F)]
  
  # split it into a list of data frames
  if(length(stratifyOn > 0)) {
    xs <- split(x, x[stratifyOn])
  }
  else {
    xs <- list(All_Strata_Lumped = x)
  }
              
  # retain only those having enough observations and return
  xs[sapply(xs, nrow) >= dropBelow]
}


#' writes a gsi_sim mixture file from a data frame
#' 
#' @export
write_gsi_sim_mixfile_from_bycatch_data_frame <- function(df, bycatch_locus_columns) {
  x <- df[, bycatch_locus_columns]  # just get the loci
  rownames(x) <- df$Sample.ID  # put the names on there
  x[is.na(x)] <- 0  # make missing data 0's
  suppressWarnings(unlink("gsi_sim_mixture.txt")) # remove any mixture files already there.
  gPdf2gsi.sim(x, outfile="gsi_sim_mixture.txt")
}



#' analyzes a mixture from bycatch data frame to the end
#' 
#' @export
do_mixture_analysis <- function(bycatch_df, bycatch_locus_columns, baseline_df, rep_unit_path) {
  
  # write the mixture file
  write_gsi_sim_mixfile_from_bycatch_data_frame(bycatch_df, bycatch_locus_columns)
  
  # write the baseline file
  suppressWarnings(unlink("gsi_sim_file.txt"))
  tmp <- make_baseline_file(baseline_df)
  
  # get rep units and write the rep-units file
  rg <- read.table(rep_unit_path, header=T, row=1)
  rg.f <- factor(rg$RepGroup, levels = unique(rg$RepGroup))
  suppressWarnings(unlink("rep_units.txt"))
  gsi_WriteReportingUnitsFile(rownames(rg), rg.f, repufile="rep_units.txt")  # make a gsi_sim reporting units file
  
  # run gsi_sim   ## NOTE!  I need to pass in a list of parameters to the current fucntion
  # so we can change the run settings.
  gsi_Run_gsi_sim("-b gsi_sim_file.txt -t gsi_sim_mixture.txt -r rep_units.txt  --mcmc-sweeps 50000 --mcmc-burnin 20000 --pi-trace-interval 10 --mix-logl-sims 2500 0")
  
  # slurp up the results:
  ret <- list()  # initialize to grab all these and return them
  ret$Pi_Trace <- read.table("rep_unit_pi_trace.txt", header=T)
  ret$Pi_MLE <- read.table("rep_unit_pi_full_em_mle.txt", header=T)
  ret$Pi_PostMean <- read.table("rep_unit_pi_posterior_means.txt", header=T)
  ret$PofZ_MLE <- read.table("rep_unit_pofz_full_em_mle.txt", header=T)
  ret$PofZ_PostMean <- read.table("rep_unit_pofz_posterior_means.txt", header=T)
  
  # slurp up the results by population (not reporting units):
  ret$pop_Pi_Trace <- read.table("pop_pi_trace.txt", header=T)
  ret$pop_Pi_MLE <- read.table("pop_pi_full_em_mle.txt", header=T)
  ret$pop_Pi_PostMean <- read.table("pop_pi_posterior_means.txt", header=T)
  ret$pop_PofZ_MLE <- read.table("pop_pofz_full_em_mle.txt", header=T)
  ret$pop_PofZ_PostMean <- read.table("pop_pofz_posterior_means.txt", header=T)
  ret$Zscores <- read.table("bayes_mixture_logl_summary.txt", header=T)
  
  # rid ourselves of the gsi_sim output files:
  unlink(c(dir(pattern = "pop_pi_.*"), dir(pattern = "pop_pofz_.*"), dir(pattern = "rep_unit_.*")))
  
  # and of the input files:
  unlink(c("gsi_sim_file.txt", "gsi_sim_mixture.txt", "rep_units.txt"))
  
  # return ret
  ret
}

