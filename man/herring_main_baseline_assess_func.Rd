% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/herring-main-func.R
\name{herring_main_baseline_assess_func}
\alias{herring_main_baseline_assess_func}
\title{main wrapper function that does all the steps for assessing the baseline}
\usage{
herring_main_baseline_assess_func(baseline_path, lat_long_path, rep_unit_path,
  locus_columns)
}
\arguments{
\item{baseline_path}{Path to a baseline file that is in the (csv) format that Dan H uses.
See alewife-baseline.csv for an example.  Importantly, the file must have a column
called Drainage.code that has the drainage codes as in DH's file.}

\item{lat_long_path}{Path to a text file with lats and longs of the pops in the baseline file.
See file alewife-lat-long.txt for an example.}

\item{locus_columns}{integer vector with the columns in the original CSV file that have the loci
that you want to use.  Should be twice as many columns as loci.  In alewife-baseline.csv this is}
}
\description{
main wrapper function that does all the steps for assessing the baseline
}

