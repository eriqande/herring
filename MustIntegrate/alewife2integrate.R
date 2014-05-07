
#Export assignment matrices for additional analyses:
write.csv(topop,"1a. Alewife self assignment matrices to population - Palkovacs ONLY.csv")
write.csv(toprg,"2a. Alewife self assignment matrices to reporting group K=3 - Palkovacs ONLY.csv")

save(toprg, file= "Alewife toprg.R")



# here we summarize the number of correct assignments to population
yy <- t(sapply(topop, function(x) diag(x$AssTable)))
CorAssTabpop <- cbind(NumAssigned=sapply(topop, function(x) x$NumAssigned), yy, N.Correct=rowSums(yy))

# here are the cutoffs
cutspop <- as.numeric(sapply(strsplit(rownames(CorAssTabpop), "_"), function(x) x[2]))

#Export the matrix of correct self-assignments to population
write.csv(CorAssTabpop,"Alewife self assignment to population with increasing stringency.csv")





###################################################################################################################
#######THIS IS WRONG AND WILL NEED TO BE FIXED#######
# here we summarize the number of correct assignments to reporting group
zz <- t(sapply(toprg, function(x) diag(x$AssTable)))
CorAssTabprg <- cbind(NumAssigned=sapply(toprg, function(x) x$NumAssigned), zz, N.Correct=rowSums(zz))

# here are the cutoffs
cutsprg <- as.numeric(sapply(strsplit(rownames(CorAssTabprg), "_"), function(x) x[2]))

#Export the matrix of correct self-assignments to reporting group
write.csv(CorAssTabprg,"Alewife self assignment to reporting group with increasing stringency.csv")
####################################################################################################################




#### Here we start messing around with making a cool plot####
# if I were to fiddle around with making an interesting plot:
x<-topop[[1]]$AssTable  # this is using cutoff of 0
maxes <- apply(x,1,max)
sum.maxes <- sum(maxes)

# here is the rightward position to start each population
starts <- c(0,cumsum(maxes/sum.maxes))[-(length(maxes)+1)]
names(starts) <- names(maxes)  # these are the right hand start points
fish.x <- .9/sum.maxes   # this is the amount of x space each assigned fish equates to.

#####I think lat.table does not work b/c my file has lats and longs in it; how do I isolate the latitude column?######
lat.table <- read.table(lat_long_path, header=T, row=1)
lats <-lat.table[names(starts), 1] # latitudes of rivers as ordered in starts

rg.colors <- c("blue", "violet", "green", "orange")

plot(starts, lats, type="n")
abline(v=starts, lty="dotted", lwd=.1)

lapply(1:nrow(x), function(r) {
  z<-x[r,]
  the.col <- rg.colors[rg.f[r]]
  xx0<-starts[r]
  yy<-lats[z>0]
  xx1<-starts[r] + z[z>0]*fish.x
  segments(xx0, yy, xx1, yy, lend=1, col=rg.colors[rg.f[z>0]]) # this is if we want the destinations colored by reporting group
  #segments(xx0, yy, xx1, yy, lend=1, col=the.col)  # this is if we want the sources colored by reporting group
}
)




####################################################################################################
####################################################################################################
####
#### NOW WE MOVE ON TO THE BYCATCH
####
####################################################################################################
####################################################################################################
# read in the bycatch data.  Note that I had to mess with the data quite a bit.  There where 
# degree symbols which are multibyte, and the quoting was gnarly because some of the lat-longs
# were in minutes and seconds.  I ended up replacing all """ with " and all ' with nothing and 
# all weird degree symbols with nothing.  I should be able to parse it all out at some point.
byc <- read.csv("1a. Alewife bycatch - minimum 6 loci no nulls.csv", stringsAsFactors=F)


# hard-wired here to put the locus headers in there:
names(byc)[seq(16,36,by=2)] <- paste(names(byc)[seq(15,35,by=2)], "1", sep=".")

# here are the indices of those loci:
byc.loc.idx <- 15:36

# now, make a baseline data set that includes only those loci, and is assured to be in the 
# correct order
byc.base <- a2[,names(byc)[byc.loc.idx]]

# now, let's do a run on everybody together just to test.
# so, make a gPiper.data.frame out of it and then turn it to a 
# gsi_sim input file.  There is a bit of a problem in that the
# Sample.ID's are duplicated for a lot of the fish in byc.  For now
# we will just toss those out:
byc.gp <- byc[,byc.loc.idx]
rownames(byc.gp) <- byc$Sample.ID 
byc.gp[is.na(byc.gp)] <- 0  # put 0's in for missing data
gPdf2gsi.sim(byc.gp, outfile="c")
gPdf2gsi.sim(byc.base, pop.ize.them=the.pops.f, outfile="alewife-baseline-for-bycatch.txt")

gsi_WriteReportingUnitsFile(rownames(rg), rg$RepGroup, repufile="alewife_rep_units.txt")  # make a gsi_sim reporting units file


# now run gsi_sim
gsi_Run_gsi_sim("-b alewife-baseline-for-bycatch.txt -t alewife-baseline-for-bycatch.txt -r alewife_rep_units.txt  --mcmc-sweeps 10000 --mcmc-burnin 5000 --pi-trace-interval 1")

# looking at some results.
# here are Pi histograms:
dev.off()
PiTrace <- read.table("rep_unit_pi_trace.txt", header=T)
PiDens <- lapply(PiTrace[-1], function(x) density(x))
plot(0:1, c(0, max(unlist(lapply(PiDens, function(z) z$y)))), type="n", xlab="Proportion of Bycatch", ylab="Posterior Density")
i<-0
lapply(PiDens, function(z) {i<<-i+1; lines(z$x, z$y, col=c("green", "red", "blue")[i])})
legend("topright", legend=names(PiDens), col=c("green", "red", "blue"), lwd=1)

dev.copy2pdf(file="alewife-all-bycatch-pi.pdf")



#Now examine probabilities of assignments to stock of origin
repunitfull <- read.table("rep_unit_pofz_full_em_mle.txt", header=T)
head(repunitfull)
write.csv(repunitfull,"All alewife bycatch assignment probabilities.csv")


repunitposteriormean <- read.table("rep_unit_pofz_posterior_means.txt", header=T)
head(repunitposteriormean)
write.csv(repunitposteriormean,"All alewife bycatch assignment posterior probabilities.csv")




#####################################################################################################################
#####################################################################################################################
####
#### DETERMINE HOW BEST TO PARSE THE BYCATH DATA (E.G. BY SEASON, REGION, FISHERY, ETC. )
####
#####################################################################################################################
#####################################################################################################################
#Switch working directories to 1a. Alewife bycatch
##Here we are looking for associations between  Year, Season, Region, and Target Fishery to determine how best to
##assign alewife and blueback herring bycatch to stock of origin
##Started by Dan Hasselman, March 11, 2014
library(plyr)

#Read in the bycatch data for ALEWIFE
alewife <- read.csv("1a. Alewife bycatch - minimum 6 loci no nulls.csv", as.is=T)

#Look at how alewife bycatch is parsed by Year, season, region (stat areas, following Bethoney et al. 2014), fishery and gear type
alewife_Yr <- count(alewife, c("Year"))
alewife_Yr_Season <-count(alewife, c("Year", "Season"))
alewife_Yr_Season_Region <-count(alewife, c("Year", "Season", "Region"))
alewife_Yr_Season_Region_Fishery <-count(alewife, c("Year", "Season", "Region", "Target.Fishery"))
alewife_Yr_Season_Region_Fishery_Gear <-count(alewife, c("Year", "Season", "Region", "Target.Fishery","Gear.Type"))

#Export these groupings
write.csv(alewife_Yr,"Alewife bycatch by year.csv")
write.csv(alewife_Yr_Season,"Alewife bycatch by year season.csv")
write.csv(alewife_Yr_Season_Region,"Alewife bycatch by year season region.csv")
write.csv(alewife_Yr_Season_Region_Fishery,"Alewife bycatch by year season region fishery.csv")
write.csv(alewife_Yr_Season_Region_Fishery_Gear,"Alewife bycatch by year season region fishery gear.csv")


count(alewife, c("Target.Fishery"))
names(alewife)
alewife1 <- alewife[,c(2, 5, 6, 9, 10, 12)]
names(alewife1)

#Isolate alewife caught as bycatch in Atlantic Herring fishery
alewife2 <- alewife1[alewife1$Target.Fishery == "Atlantic Herring", ]


#Look for a correlation between [Year x Season x Region] with Target.Fishery with the 'stats' package
alewife.df <- data.frame(count(alewife, c("Year", "Season", "Region")))
alewife.df
is.numeric(alewife.df$freq)

cor(alewife.df$Year, alewife.df$freq)
####This is where I stopped for alewife (April 2, 2014)


boing <- split(alewife, f=alewife$Year)
str(boing)

