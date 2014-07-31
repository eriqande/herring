

# this is a script to do the actual analysis.
# it is not fully together yet, but I have a few blurbs to show some of it.  

# i need to formalize it.


# here we run two different runs of the MCMC and then compare the
# posterior means of the mixing proportions
bbres  <- do.call(herring_all_analyses, args = blueback_run_settings())  # first run
bbres2 <- do.call(herring_all_analyses, args = blueback_run_settings()) # second run

# get all the posterior means of the Pis into a single data frame
bb1 <-  do.call(rbind, lapply(bbres[[2]], function(x) x$Pi_PostMean))  # for first run
bb2 <-  do.call(rbind, lapply(bbres2[[2]], function(x) x$Pi_PostMean)) # for second run

# plot them:
plot(bb1$Mean.Pi, bb2$Mean.Pi)
abline(a=0, b=1) # show that y=x pretty much!

