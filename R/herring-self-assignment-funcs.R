



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
#' @return A factor vector of the populations in the baseline.  Has a side effect of writing a file named
#' gsi_sim_file.txt in the current working directory.
#' @export
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
#' @param rep_unit_path  Path to the file designating the reporting units.
#' @return Returns the self-assignment matrices to pop and to reporting group.
#' @export
gsi_self_assignment <- function(the.pops.f, rep_unit_path) {
  # and then run that file:
  gsi_Run_gsi_sim("-b gsi_sim_file.txt --self-assign")
  
  # and get the self assignment results
  SA <- gsi.simSelfAss2DF(file="GSI_SIM_Dumpola.txt")$Post
  
  # figure out the max assignments to population:
  MC <- gsi_simMaxColumnAndPost(SA,-c(1,2))  # dropping the first two columns here because they are not assignment posteriors
  
  # then, here are our assignment matrices to population
  topop<- gsi_simAssTableVariousCutoffs(SA$PopulationOfOrigin, MC$MaxColumn, MC$MaxPosterior, cutoffs = seq(0, 0.95, by = 0.05))
  
  
  # let us explore aggregating assignments by reporting unit
  rg <- read.table(rep_unit_path, header=T, row=1)
  rg.f <- factor(rg$RepGroup, levels=unique(rg$RepGroup))
  

  SA.rg <- gsi_aggScoresByRepUnit(SA, levels(the.pops.f), rg.f)  # here are the assignment values by reporting group
  MC.rg <- gsi_simMaxColumnAndPost(SA.rg,-c(1,2))
  
  
  toprg <- gsi_simAssTableVariousCutoffs(SA.rg$PopulationOfOrigin, MC.rg$MaxColumn, MC.rg$MaxPosterior, cutoffs = seq(0, 0.95, by = 0.05))
  
  # now we also want to condense the "from"'s to their reporting groups:
  frgtrg <-  lapply(toprg, function(z) {
    x <- z$AssTable
    z$AssTable <- do.call(rbind, tapply(1:nrow(x), rg.f, function(y) colSums(rbind(x[y, ]))))
    z
  })
  
  samp_sizes <- rowSums(topop[[1]]$AssTable)
  
  
  list(from_pop_to_pop = topop, 
       from_pop_to_rg = toprg,
       from_rg_to_rg = frgtrg,
       from_pop_sample_sizes = samp_sizes,
       from_rg_sample_sizes = tapply(samp_sizes, rg.f, sum),
       rep_units = rg.f
       )
}


#' make a simple barplot of proportions assigned from an assignment array
#' 
#' @param ass_array The self assignment arrray, as you might find as one of the 
#' components returned by gsi_self_assignment
#' @param cols the colors to use
#' @export
simple_barplot <- function(ass_array, cols=c("red", "blue", "green", "orange")) {
  x<-t(ass_array)
  y <- x[,rev(1:ncol(x))]  # get them in the right order
  
  yp <- apply(y, 2, function(x) x/sum(x))
  
  par(mar=c(2,7,.3,2))
  barplot(yp, horiz=T, col=cols, las=1, names.arg=paste(colnames(yp), " (",colSums(y) ,")"), cex.names=.7)
  
}


#' create a ramp of colors starting from the last and proceeding to the y-th of a color brewer palette
#' 
#' @param brew.pal The color brewer palette.  By name.
#' @param n The number of colors to return in the ramp
#' @param y Endpoint on the "light" side of the palette
#' @export
#' @examples
#' rr <- brewerRamp("Purples", 9)
#' image(1:length(rr), 1, as.matrix(1:length(rr)), col = rr, ylab = "", xaxt = "n", yaxt = "n", bty = "n")
brewerRamp <- function(brew.pal, n = 7, y = 3) {
  myCols <- rev(brewer.pal(9, name = brew.pal)[-(1:y)])
  ramp <- colorRampPalette(myCols)
  ramp(n)
  
}



brewerPairs <- function(brew.pal, y=3) {
  myCols <- rev(brewer.pal(9, name = brew.pal)[-(1:y)])
  c(myCols[1], myCols[length(myCols)])
}




#' make a barplot that has different densities of the same color for pops within the same reporting unit
#' 
#' @param ass_array An assignment array with rows being from and cols being "to". Where individuals
#' are assigned to populations.
#' @param rep_groups a factor vector given the reporting groups each populations belongs to.  For this to
#' work, the pops must all be ordered so that they are all next to one another within reporting groups
#' @param col.pairs matrix of pairs of colors
#' @export
to_pops_barplot <- function(ass_array, rep_groups, 
                            col.pairs = matrix(c(brewerPairs("Reds"), brewerPairs("Blues"), brewerPairs("Greens"), "yellow", "khaki"), nrow=2)
                            ){
  tab <- table(rep_groups)
  bp <- col.pairs[, rep(1:ncol(col.pairs), length.out=length(tab))]
  
  cols <- unlist(lapply(1:ncol(bp), function(x) {r <- colorRampPalette(bp[,x]); r(tab[x])}))
  
  simple_barplot(ass_array, cols)
}


