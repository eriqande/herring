% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/herring-mixture-sims.R
\name{herring_mixture_sims}
\alias{herring_mixture_sims}
\title{Do the mixture simulations and make some nice ggplots of the results}
\usage{
herring_mixture_sims(baseline_path, lat_long_path, locus_columns, rep_unit_path,
  plot_prefix = "herring", bycatch_N = 1000, random_seed = 50,
  num_rando_mix_props = 50, num_simmed_mixtures = 4, point_size = 1.7,
  point_shape = 1)
}
\arguments{
\item{baseline_path}{Path to a baseline file that is in the (csv) format that Dan H uses.
See alewife-baseline.csv for an example.  Importantly, the file must have a column
called Drainage.code that has the drainage codes as in DH's file.}

\item{lat_long_path}{Path to a text file with lats and longs of the pops in the baseline file.
See file alewife-lat-long.txt for an example.}

\item{locus_columns}{integer vector with the columns in the original CSV file that have the loci
that you want to use.  Should be twice as many columns as loci.  In alewife-baseline.csv this is}

\item{rep_unit_path}{Path to the reporting unit file}

\item{plot_prefix}{Prefix to put on the plot names}

\item{bycatch_N}{The number of fish in the simulated bycatch samples.}

\item{num_rando_mix_props}{the number of random mixing proportiong to simulate num_simmed_mixtures
at.}

\item{num_simmed_mixtures}{the number of mixtures to simulate for each mixing proportion}

\item{point_size}{Size of the points on the plots.}
}
\description{
Quickly threw this together
}

