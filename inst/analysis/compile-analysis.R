

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



# 1.5 Make boxplots and density plots of the mixing proportions estimate when bycatch was all lumped (not stratified)
lumpy.pi.trace.melt <- melt(bb1$bycatch_lumped_together$All_Strata_Lumped$Pi_Trace, id.vars = "SweepNumber")
ggplot(lumpy.pi.trace.melt,aes(x=variable, y=value, fill = variable)) + geom_boxplot() + ggtitle("BB bycatch lumped (unstratified)")
ggplot(lumpy.pi.trace.melt,aes(x=value, fill=variable)) + geom_density(alpha=0.25)  # density plot
boxplot(lumpy.pi.trace.melt$value ~ lumpy.pi.trace.melt$variable)  # here is the non-ggplot version.

# 2. Do something similar, but make it boxplots for the pop's:
pop.pi.trace.melt  <- melt(bb1$bycatch_output$"2012.Winter.South New England 611.Atlantic Herring."$pop_Pi_Trace, id.vars = "SweepNumber")
ggplot(pop.pi.trace.melt,aes(x=variable, y=value, fill = variable)) + geom_boxplot()



# 2.25  Ah hell.  Why not make boxplots to population for all of the strata:
melty.pops <- lapply(bb1$bycatch_output, function(x) melt(x$pop_Pi_Trace, id.vars = "SweepNumber"))
many.pop.plots <- lapply(names(melty.pops), function(a) {
  z <- melty.pops[[a]]
  ggplot(z, aes(x = variable, y = value, fill = variable)) + geom_boxplot() + ggtitle(a) + coord_flip()
})
many.pop.plots[[5]]  # look at one of them

# 2.4 And here is the same thing for reporting units:
melty.reps <- lapply(bb1$bycatch_output, function(x) melt(x$Pi_Trace, id.vars = "SweepNumber"))
many.rep.plots <- lapply(names(melty.reps), function(a) {
  z <- melty.reps[[a]]
  ggplot(z, aes(x = variable, y = value, fill = variable)) + geom_boxplot() + ggtitle(a) + coord_flip()
})
many.rep.plots[[5]]  # look at one of them


# 2.5 Here is the lumped result by population:
lumpy.pop.pi.trace.melt  <- melt(bb1$bycatch_lumped_together$All_Strata_Lumped$pop_Pi_Trace, id.vars = "SweepNumber")
ggplot(lumpy.pop.pi.trace.melt, aes(x=variable, y=value, fill = variable)) + geom_boxplot() + ggtitle("BB Bycatch lumped and by population")


# here let's make boxplots and color them by the reporting unit:
# here are some layers, saved in a list, to make boxplots
boxp_layers <- list(
  geom_boxplot(outlier.colour = NULL),
  coord_flip(),
  scale_color_manual(values = c("red", "blue", "green", "yellow")), 
  scale_fill_manual(values = c("red", "blue", "green", "yellow"))
)

# here we make boxplots for each population
ru <- bb1$baseline_assessment$rep_units  # these are the reporting unit factors
names(ru) <- levels(bb1$baseline_assessment$the.pops.f)  # give them names for converting pops to rep units
newdf <- cbind(lumpy.pop.pi.trace.melt, repu = ru[as.character(lumpy.pop.pi.trace.melt$variable)])
newdf$variable <- factor(newdf$variable, levels = rev(levels(newdf$variable)))  # reverse their order so they plot North to South

# save a ggplot of that and put some black lines on it
p <- ggplot(newdf, aes(x=variable, y=value, color = repu, fill = repu)) + 
  boxp_layers + 
  geom_boxplot(outlier.colour = NA, colour = "black")
  

# Now make a boxplot of the repunits total to inset into a little window
lumpy.pi.trace.melt$variable <- factor(lumpy.pi.trace.melt$variable, levels = rev(levels(lumpy.pi.trace.melt$variable)))
# this plot doesn't get any black lines because it is already pretty compressed
q <- ggplot(lumpy.pi.trace.melt,aes(x=variable, y = value, fill=variable, color = variable)) + boxp_layers

print(p)  # make the population plot
vp <- viewport(width = 0.5, height = 0.33, x = 0.35, y = 0.06, just = c("left", "bottom"))
print(q, vp = vp)

#Any old plot
a_plot <- ggplot(cars, aes(speed, dist)) + geom_line()

#A viewport taking up a fraction of the plot area
vp <- viewport(width = 0.4, height = 0.4, x = 0.8, y = 0.2)

#Just draw the plot twice
png("test.png")
print(a_plot)
print(a_plot, vp = vp)
dev.off()


# So, from that it should be clear how all the info for the figures Dan made was extracted.



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
bb1.bc.zs$Place  <- "MIX"
bb1.base.zscores$Place <- "BASE"
zmelt <- melt(rbind(bb1.bc.zs[c("Place", "zScore")], bb1.base.zscores[c("Place", "zScore")]), id.vars = "Place")

# 4. And then plot them in various ways:
ggplot(zmelt, aes(x = value, fill = Place)) + geom_density(alpha = 0.25)  # density plot
bb_zhists <- ggplot(zmelt, aes(x = value, fill = Place)) + geom_histogram() + facet_wrap(~Place, nrow=2) # histograms
ggsave("blueback_zhists.pdf", bb_zhists)


# 5. Now do all the same stuff for alewife:
aa1.bc.zs <- do.call(rbind, lapply(aa1$bycatch_output, function(x) x$Zscores))
aa1.baseline.pops <- make_baseline_file(aa1$baseline_assessment$baseline)  # this makes gsi_sim_file.txt 
gsi_Run_gsi_sim(arg.string = "-b gsi_sim_file.txt --self-assign --base-logl-sims 2500 0")
aa1.base.zscores <- read.table("baseline_logl_summary.txt", header = T)
aa1.bc.zs$Place  <- "MIX"
aa1.base.zscores$Place <- "BASE"
aa_zmelt <- melt(rbind(aa1.bc.zs[c("Place", "zScore")], aa1.base.zscores[c("Place", "zScore")]), id.vars = "Place")
aa_zhists <- ggplot(aa_zmelt, aes(x = value, fill = Place)) + geom_histogram() + facet_wrap(~Place, nrow=2) # histograms
ggsave("alewife_zhists.pdf", aa_zhists)




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
