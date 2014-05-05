
# install gpiper package (we do this every time because the package is currently evolving...)
#install.packages("gpiper_0.1.zip", repos=NULL)

# then load the package
library(gpiper)






#' Read in a herring baseline file and make a gPiper data frame out of it
#' 
#' @param baseline_path Path to a baseline file that is in the (csv) format that Dan H uses.  
#' See alewife-baseline.csv for an example.  Importantly, the file must have a column
#' called Drainage.code that has the drainage codes as in DH's file.
#' @param lat_long_path  Path to a text file with lats and longs of the pops in the baseline file.
#' See file alewife-lat-long.txt for an example.
#' @param locus_columns integer vector with the columns in the original CSV file that have the loci 
#' that you want to use.  Should be twice as many columns as loci.  In alewife-baseline.csv this is
#' @export
# This will become the @example but I need to get the data in there correctly
# herring_csv2gpiper("./data/alewife-baseline.csv", "./data/alewife-lat-long.txt", 14:35)
herring_csv2gpiper <- function(baseline_path, lat_long_path, locus_columns) {
  # get the alewife
  a <- read.csv(baseline_path, as.is=T)
  a <- data.frame(ID=gsub("_", "", a$Drainage.code), a, stringsAsFactors=F)  # get the column that we want for the IDs in there, remove the underscores.
  
  # drop populations that have fewer than 9 individuals
  tmp <- table(gsub("[0-9]", "", a$ID))
  a1 <- a[ gsub("[0-9]", "", a$ID) %in% names(tmp[tmp>=9]), ]  # this eliminates 1 fish from MIR
  
  
  # now retain just the columns we want
  a2<- a1[,c(1, (locus_columns + 1))] # this was for the nulls removed and no McBride ONLY loci
  names(a2)[1] <- ""  # remove the "ID" name
  
  
  ## Here we put it into gpipe format
  # turn missing data to 0
  a2[is.na(a2)] <- 0
  
  # make the ID the rownames and then toss that column
  rownames(a2) <- a2[,1]
  a2 <- a2[,-1]
  
  #finally, give the two locus columns identical names
  names(a2)[c(F,T)] <- names(a2)[c(T,F)] # get the locus names on each column
  
  a2
}  

#' make a gsi-sim file from a gpiper data frame
#' 
#' @param baseline.df  a data frame of the sort returned by \code{\link{herring_csv2gpiper}}
#' @return A factorvector of the populations in the baseline.  Has a side effect of writing a file named
#' gsi_sim_file.txt in the current working directory.
make_baseline_file <- function(baseline_df) {
  # then we should be able to write out a gsi_sim file easily
  # get the pops we want in the order we want them:
  the.pops <- gsub("[0-9]*", "", rownames(baseline_df))
  the.pops.f <- factor(the.pops, levels=unique(the.pops))
  
  # make a gsi_sim input file:
  gPdf2gsi.sim(baseline_df, the.pops.f)
  
  the.pops.f
}



#' Run gsi_sim for a self-assignment analysis and caputure the requisite output
#'
#' Note that this function will read a file called gsi_sim_file.txt in the 
#' current working directory.
#' 
#' @param the.pops.f  factor vector giving the population that each individual in the
#' baseline belongs to.
#' @return Returns the self-assignment matrices to pop and to reporting group.
gsi_self_assignment <- function(the.pops.f) {
  # and then run that file:
  gsi_Run_gsi_sim("-b gsi_sim_file.txt --self-assign")
  
  # and get the self assignment results
  SA <- gsi.simSelfAss2DF(file="GSI_SIM_Dumpola.txt")$Post
  
  # figure out the max assignments to population:
  MC <- gsi_simMaxColumnAndPost(SA,-c(1,2))  # dropping the first two columns here because they are not assignment posteriors
  
  # then, here are our assignment matrices to population
  topop<- gsi_simAssTableVariousCutoffs(SA$PopulationOfOrigin, MC$MaxColumn, MC$MaxPosterior)
  
  # let us explore aggregating assignments by reporting unit
  rg <- read.table(rep_unit_path, header=T, row=1)
  rg.f <- factor(rg$RepGroup, levels=unique(rg$RepGroup))
  
  SA.rg <- gsi_aggScoresByRepUnit(SA, levels(the.pops.f), rg.f)  # here are the assignment values by reporting group
  MC.rg <- gsi_simMaxColumnAndPost(SA.rg,-c(1,2))
  
  
  toprg <- gsi_simAssTableVariousCutoffs(SA.rg$PopulationOfOrigin, MC.rg$MaxColumn, MC.rg$MaxPosterior)
  
  list(topop = topop, toprg = toprg)
}






