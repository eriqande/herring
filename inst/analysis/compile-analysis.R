

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
library(grid)
library(plyr)

#### Do the self-assignment analyses and the GSI once on blueback herring and alewife  ####
message("Doing self-assignment and GSI one time")
#  bb is "blueback" and "aa" is alewife
bb1 <- do.call(herring_all_analyses, args = blueback_run_settings())  # first run
aa1 <- do.call(herring_all_analyses, args = alewife_run_settings())  # first run
# for development, Eric can get bb1-like and aa1-like output fast like this:
#   load("bb1_for_devel.rda"); load("aa1_for_devel.rda");


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


message ("Examine posterior probability of assignments for each bycatch strata and estimate 95% CI")
# this function takes one component of a list
# like bb1$bycatch_output, and squashes into a data frame
# row with the mean and the 95% quantiles. Change what to
# make it grab the pop_pi_traces if you want.
get_means_and_quants <- function(L, what = "Pi_Trace") {
  sapply(L[[what]], function(x) c(mean = mean(x), quantile(x, probs=c(0.025, 0.975) )))
}

# this function takes a bycatch output result BOR and the name of the csv file that
# has the strata in a manner that can be ordered, and makes a table of it
bycatch_prop_table <- function(BOR = bb1$bycatch_output, CSV = "blueback_bycatch_strata.csv") { 
  tmp <- lapply(BOR, function(x) {
    y <- get_means_and_quants(x);
    z <- data.frame(stat = rownames(y), y, stringsAsFactors = FALSE)
  })
  
  # get the order that we want to put those strata into:
  strat_nums <- read.csv(file.path(system.file("data_files", package = "herring", mustWork = T), CSV),
                            header=F, stringsAsFactors = FALSE)
  
  # reorder strata the way we want them
  tmp <- tmp[strat_nums$V2[order(strat_nums$V1)]]
  stat.frame <- ldply(tmp)
  
  sfm <- as.matrix(stat.frame[, -(1:3)])
  sfm[] <- sprintf("%0.4f", sfm)
  for(i in seq(2, nrow(sfm), by=3)) {
    sfm[i, ] <- paste("(", sfm[i,], "--", sfm[i+1, ], ")", sep = "")
  }
  sfm <- sfm[c(T, T, F), ]
  
  fin <- data.frame(Stratum = stat.frame$".id"[c(T, T, F)], sfm)
  
  write.csv(fin, file = gsub("\\.csv$", replacement = "_stats.csv", x = CSV), row.names = F)
}

# print out some tables to csv files:
bycatch_prop_table(BOR = bb1$bycatch_output, CSV = "blueback_bycatch_strata.csv")
bycatch_prop_table(BOR = aa1$bycatch_output, CSV = "alewife_bycatch_strata.csv")

message("Making plots to show how to access lists")
# So, here are some examples of how we would access these:


# prepare to make boxplots and color them by the reporting unit:
# here are some layers, saved in a list, to make boxplots
boxp_layers <- list(
  geom_boxplot(outlier.colour = NULL, outlier.size = 0.8),
  coord_flip(),
  ylab("Mixing Proportion")
)

# get molten data for when bycatch was all lumped (not stratified)
bb.pi.lump <- melt(bb1$bycatch_lumped_together$All_Strata_Lumped$Pi_Trace, id.vars = "SweepNumber")
bb.pi.lump$ord.variable <- factor(bb.pi.lump$variable, levels = rev(levels(bb.pi.lump$variable))) # reorder this factor to get the right order North to South


# here is the molten data when individuals are identified to population
bb.pi.pop.lump  <- melt(bb1$bycatch_lumped_together$All_Strata_Lumped$pop_Pi_Trace, id.vars = "SweepNumber")
ru <- bb1$baseline_assessment$rep_units  # these are the reporting unit factors
names(ru) <- levels(bb1$baseline_assessment$the.pops.f)  # give them names for converting pops to rep units
bb.pi.pop.lump$repu <- ru[as.character(bb.pi.pop.lump$variable)] # add a column for the reporting unit
bb.pi.pop.lump$ord.variable <- factor(bb.pi.pop.lump$variable, levels = rev(levels(bb.pi.pop.lump$variable)))  # order that better


# save a ggplot object of mixing proportions to population and put some black lines on it
p <- ggplot(bb.pi.pop.lump, aes(x=ord.variable, y=value, color = repu, fill = repu)) + 
  boxp_layers + 
  geom_boxplot(outlier.colour = NA, colour = "black") +
  scale_color_manual(name = "Reporting Units", values = c("red", "blue", "green", "yellow")) +
  scale_fill_manual(name = "Reporting Units", values = c("red", "blue", "green", "yellow")) +
  xlab("Population")

# Now make a boxplot of the repunits total to inset into a little window
# this plot doesn't get any black lines because it is already pretty compressed
q <- ggplot(bb.pi.lump,aes(x=ord.variable, y = value, fill=ord.variable, color = ord.variable)) + boxp_layers +
  scale_color_manual(name = "Reporting Units", values = rev(c("red", "blue", "green", "yellow"))) +
  scale_fill_manual(name = "Reporting Units", values = rev(c("red", "blue", "green", "yellow"))) +
#  guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  guides(fill = FALSE, color = FALSE) +
  xlab("Reporting Unit")

print(p)  # make the population plot
vp <- viewport(width = 0.5, height = 0.33, x = 0.35, y = 0.06, just = c("left", "bottom"))
print(q, vp = vp) # put the repu in a little window



# make faceted boxplots of the stratum results
df <- ldply(bb.pi.pop.lump, data.frame)  # THIS IS CURRENTLY BUGGY.
dfl <- df[sample(nrow(df), 100000),]  # use just a subset of the data for now
dfl$repu <- ru[as.character(dfl$variable)]
dfl$ord.var <- factor(dfl$variable, levels=rev(levels(dfl$variable)))
t <- ggplot(dfl, aes(x = ord.var, y = value, fill = repu, color = repu)) + 
  geom_boxplot(outlier.colour = NULL, outlier.size = 0.62) + 
  geom_boxplot(outlier.colour = NA, color = "black") +
  facet_wrap( ~ .id, ncol=5) + 
  scale_fill_manual(name = "Reporting Units", values = c("red", "blue", "green", "yellow")) +
  scale_color_manual(name = "Reporting Units", values = c("red", "blue", "green", "yellow")) +
  coord_flip()


#### Running GSI_SIM on all the bycatch unstratified ####


#### Comparing Zscores for bycatch fish to the baseline fish ####
message("Comparing z-scores for mixture and baseline")
# It can be instructive to compare these Z-scores, to see if there are any
# in the bycatch that fall well outside what is expected.  So,  here we go.
# 1. First, extract the Z-scores for all the fish across all the strata from the 
#    bycatch data.
bb1.bc.zs <- do.call(rbind, lapply(bb1$bycatch_output, function(x) x$Zscores))

# 2. Compute z scores for all the fish in the baseline
bb1.baseline.pops <- make_baseline_file(bb1$baseline_assessment$baseline)  # this makes gsi_sim_file.txt 
gsi_Run_gsi_sim(arg.string = "-b gsi_sim_file.txt --self-assign --base-logl-sims 2500 0")
bb1.base.zscores <- read.table("baseline_logl_summary.txt", header = T)

# 3. Now merge those two together and then melt it so we can use ggplot
bb1.bc.zs$Place  <- "Bycatch"
bb1.base.zscores$Place <- "Baseline"
zmelt <- melt(rbind(bb1.bc.zs[c("Place", "zScore")], bb1.base.zscores[c("Place", "zScore")]), id.vars = "Place")

# 4. And then plot them in various ways:
bb_zhists <- ggplot(zmelt, aes(x = value, fill = Place)) + geom_histogram() + facet_wrap(~Place, nrow=2) # histograms
ggsave("blueback_zhists.pdf", bb_zhists)

bb_zhists2<-ggplot(zmelt, aes(x = value, fill = Place)) + geom_density(alpha = 0.25) + theme_bw() +
  xlab("Z-score (-log-likelihood)") + ylab("Density") + theme(legend.title=element_blank())  # density plot
ggsave("blueback_zhists2.pdf", bb_zhists2)


# 5. Now do all the same stuff for alewife:
aa1.bc.zs <- do.call(rbind, lapply(aa1$bycatch_output, function(x) x$Zscores))
aa1.baseline.pops <- make_baseline_file(aa1$baseline_assessment$baseline)  # this makes gsi_sim_file.txt 
gsi_Run_gsi_sim(arg.string = "-b gsi_sim_file.txt --self-assign --base-logl-sims 2500 0")
aa1.base.zscores <- read.table("baseline_logl_summary.txt", header = T)
aa1.bc.zs$Place  <- "Bycatch"
aa1.base.zscores$Place <- "Baseline"
aa_zmelt <- melt(rbind(aa1.bc.zs[c("Place", "zScore")], aa1.base.zscores[c("Place", "zScore")]), id.vars = "Place")
aa_zhists <- ggplot(aa_zmelt, aes(x = value, fill = Place)) + geom_histogram() + facet_wrap(~Place, nrow=2) +
  xlab("Z-score (-log-likelihood)") +
  ylab("Density")  # histograms
ggsave("alewife_zhists.pdf", aa_zhists)
aa_zhists2<-ggplot(aa_zmelt, aes(x = value, fill = Place)) + geom_density(alpha = 0.25) + theme_bw() +
  xlab("Z-score (-log-likelihood)") + ylab("Density") + theme(legend.title=element_blank())  # density plot
ggsave("alewife_zhists2.pdf", aa_zhists2)



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

# now save those to an rda in case you want to use them later
save(aa.list, bb.list, file="multi-mcmcm-runs.rda", compress = "xz")

# then plot all the posterior means of the Pi parameter (over reporting units) for each 
# of those 6 runs against the first one we did (aa1 of bb1)
plot_pi <- function(x, yl, file = "pi-comp-plot.pdf") {  # x will be like bb1 and yl like bb.list
  xx <- do.call(rbind, lapply(x$bycatch_output, function(z) z$Pi_PostMean))  # get the Pi estimates from the very first run
  yy.list <- lapply(yl, function(x) do.call(rbind, lapply(x$bycatch_output, function(z) z$Pi_PostMean)))
  
  # now plot this
  pdf(file = file, width = 12, height = 9)
  par(mfrow=c(2,3))
  lapply(yy.list, function(y) {plot(xx$Mean.Pi, y$Mean.Pi); abline(a = 0, b = 1, lwd = .25, col = "gray")})
  dev.off()
}


plot_pi(bb1, bb.list, "blueback-pi-comp-between-runs.pdf")
plot_pi(aa1, aa.list, "alewife-pi-comp-between-runs.pdf")
