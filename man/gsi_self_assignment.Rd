% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/herring-self-assignment-funcs.R
\name{gsi_self_assignment}
\alias{gsi_self_assignment}
\title{Run gsi_sim for a self-assignment analysis and caputure the requisite output}
\usage{
gsi_self_assignment(the.pops.f, rep_unit_path)
}
\arguments{
\item{the.pops.f}{factor vector giving the population that each individual in the
baseline belongs to.}

\item{rep_unit_path}{Path to the file designating the reporting units.}
}
\value{
Returns the self-assignment matrices to pop and to reporting group.
}
\description{
Note that this function will read a file called gsi_sim_file.txt in the
current working directory.
}

