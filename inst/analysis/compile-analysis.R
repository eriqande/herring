

# This is a script that documents how we did the actual analyses for the paper.
# The main funtion is "herring_all_analyses" which spits results back in a big
# list.  Running that function also creates some plots.  Those just get saved
# in the plot viewer.  If you are using RStudio it is pretty easy to then export
# them in whatever format you want.  To create final figures, Dan Hasselman
# did a lot of hand-editing and placing of elements.  So we don't try to 
# recreate that here.


# we do the analyses both for blueback and herring.

library(herring)  # make sure the library is loaded
library(reshape2)
library(ggplot2)

#### Do the self-assignment analyses and the GSI once on blueback and herring  ####
message("Doing self-assignment and GSI one time")
#  bb is "blueback" and "aa" is alewife
bb1 <- do.call(herring_all_analyses, args = blueback_run_settings())  # first run
aa1 <- do.call(herring_all_analyses, args = alewife_run_settings())  # first run



#### Now let us investigate how we parse results out of these output lists ####
# First, it will be good to review what the structure of bb1 and aa1 are at this juncture.  We
# will use bb1 as an example, but aa1 is structured the same way.
#   bb1$baseline_assessment   <--- This just holds the baseline and the reporting units
#   bb1$bycatch_output    <---- This is a named list of output for each stratum
# 
#    The results for each stratum come in components of:
#    Pi_Trace"          "Pi_MLE"            "Pi_PostMean"       "PofZ_MLE"          "PofZ_PostMean"     
#    "pop_Pi_Trace"     "pop_Pi_MLE"         "pop_Pi_PostMean"  "pop_PofZ_MLE"      "pop_PofZ_PostMean" 
#    "Zscores"          
#
# The ones that don't start with "pop" are results from Reporting Units and the ones that do are
# results from populations in the baseline.

message("Making plots to show how to access lists")
# So, here are some examples of how we would access these:
# 1. Make plots of the mixing proportions in the "2012.Winter.South New England 611.Atlantic Herring." stratum
pi.trace.melt  <- melt(bb1$bycatch_output$"2012.Winter.South New England 611.Atlantic Herring."$Pi_Trace, id.vars = "SweepNumber")
ggplot(pi.trace.melt,aes(x=value, fill=variable)) + geom_density(alpha=0.25)  # density plot
ggplot(pi.trace.melt,aes(x=variable, y=value, fill = variable)) + geom_boxplot()

# 2. Do something similar, but make it boxplots for the pop's:
pop.pi.trace.melt  <- melt(bb1$bycatch_output$"2012.Winter.South New England 611.Atlantic Herring."$pop_Pi_Trace, id.vars = "SweepNumber")
ggplot(pop.pi.trace.melt,aes(x=variable, y=value, fill = variable)) + geom_boxplot()


# So, from that it should be clear how all the info for the figures Dan made was extracted.


#### Comparing Zscores for bycatch fish to the baseline fish ####
message("Comparing z-scores for mixture and baseline")
# It can be instructive to compare these Z-scores, to see if there are any
# in the bycatch that fall well outside what is expected.  So,  here we go.
# 1. First, extract the Z-scores for all the fish across all the strata from the 
#    bycatch data.
bb1.bc.zs <- do.call(rbind, lapply(bb1$bycatch_output, function(x) x$Zscores))

# this would plot those:
ggplot(bb1.bc.zs, aes(x=zScore)) + geom_density() 

# 2. Compute z scores for all the fish in the baseline
bb1.baseline.pops <- make_baseline_file(bb1$baseline_assessment$baseline)  # this makes gsi_sim_file.txt 
gsi_Run_gsi_sim(arg.string = "-b gsi_sim_file.txt --self-assign --base-logl-sims 2500 0")
bb1.base.zscores <- read.table("baseline_logl_summary.txt", header = T)

# 3. Now merge those two together and then melt it so we can use ggplot
bb1.bc.zs$Place  <- "MIX"
bb1.base.zscores$Place <- "BASE"
zmelt <- melt(rbind(bb1.bc.zs[c("Place", "zScore")], bb1.base.zscores[c("Place", "zScore")]), id.vars = "Place")
ggplot(zmelt, aes(x = value, fill = variable)) + geom_histogram()

# 4. And then plot them in various ways:
ggplot(zmelt, aes(x = value, fill = Place)) + geom_density(alpha = 0.25)  # density plot
gg_zhists <- ggplot(zmelt, aes(x = value, fill = Place)) + geom_histogram() + facet_wrap(~Place, nrow=2) # histograms

# it seems to me that the histogram above definitively shows us that we don't need to 
# toss any fish from the mixture. (though we have a handful in the baseline that are clearly
# not from the right baseline or are the wrong species.  Not enough to worry about though!)


#### Do replicate runs of the MCMC to confirm that things are mixing fine ####
# In a sense this is something of a no brainer, because GSI_SIM mixes well by 
# design, but it will be worth doing anyway.
message("Doing replicate runs of the MCMC")

# we will just re-do the whole analysis multiple times, and it will end up 
# using different seeds for the gsi_sim because it draws those from the 
# gsisim_seeds file.
bb.list <- lapply(1:6, function(x) {message(paste("Running Blueback Run", x)); do.call(herring_all_analyses, args = blueback_run_settings())})
aa.list <- lapply(1:6, function(x) {message(paste("Running Alewife Run", x)); do.call(herring_all_analyses, args = alewife_run_settings())})

# now save those to an rda
save(aa.list, bb.list, file="multi-mcmcm-runs.rda")
