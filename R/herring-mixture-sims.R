
#' Do the mixture simulations and make some nice ggplots of the results
#' 
#' Quickly threw this together
#' @inheritParams herring_csv2gpiper
#' @param rep_unit_path  Path to the reporting unit file
#' @param plot_prefix    Prefix to put on the plot names 
#' @param bycatch_N The number of fish in the simulated bycatch samples.
#' @param num_simmed_mixtures the number of mixtures to simulate for each mixing proportion
#' @param num_rando_mix_props the number of random mixing proportiong to simulate num_simmed_mixtures 
#' at.  
#' @param point_size Size of the points on the plots.
#' @export
herring_mixture_sims <- function(baseline_path, lat_long_path, locus_columns, rep_unit_path, plot_prefix = "herring",
                                 bycatch_N = 1000, random_seed = 50, num_rando_mix_props = 50, num_simmed_mixtures = 4,
                                 point_size = 1.7, point_shape = 1) {
  
  B <- herring_csv2gpiper(baseline_path, lat_long_path, locus_columns)
  
  # make a gsi_sim file of it, but really only to get the pops.f
  pops.f <- make_baseline_file(B)
  
  
  # handle the reporting units:
  r <- read.table(rep_unit_path, header = T, stringsAsFactors = FALSE)
  rf <- factor(r$RepGroup, levels = unique(r$RepGroup))
  names(rf) <- r$Pop
  repu.f <- rf[levels(pops.f)]
  
   
  # that was nice. Now do 4 simulations at each of 25 different random values of the mixing proportions
  set.seed(random_seed)
  cat(runif(n=2, min=1, max=10000000), file = "gsi_sim_seeds")  # set gsi_sim's seed too
  many.reps <- lapply(1:num_rando_mix_props, function(x) {
    
    # here we simulate the total mixing proportion for each repu and than we simulate
    # the amount for each pop within a repu as well. 
    dd <- rgamma(length(levels(repu.f)), shape = 1.5, scale = 1); 
    dd <- dd/sum(dd)  # now dd is the proportion of each reporting unit
    names(dd) <- levels(repu.f)
    pp <- rep(0, length(repu.f))
    for(i in levels(repu.f)) {
      ee <- rgamma(sum(repu.f == i), shape = 1.5, scale = 1); 
      ee <- ee/sum(ee) * dd[i]
      pp[repu.f == i] <- ee
    }
    
    gsi.mixtureSims(B, pops.f, pp, N = bycatch_N, 
                    Reps = num_simmed_mixtures, fixed = FALSE, repu.f = repu.f, samp_unit = "multilocus")
  })
  
  
  # now, we take the result and put it in long format for ggplot:
  pop_long <- do.call(rbind, args = lapply(many.reps, function(x) 
    data.frame(Pop = factor(rownames(x$estimated_pi), levels = rownames(x$estimated_pi)), 
               Truth = unname(x$true_pi), 
               Est = as.numeric(x$estimated_pi))
  ))
  # put a repu column on the pop_long data frame so we can color things as desired
  pop_long$Repu <- rf[as.character(pop_long$Pop)]
  
  # now get the result for the aggregated reporting unit Pi estimates
  rep_long <- do.call(rbind, args = lapply(many.reps, function(x) 
    data.frame(Repu = factor(rownames(x$repu_estimated_pi), levels = rownames(x$repu_estimated_pi)), 
               Truth = unname(x$repu_true_pi), 
               Est = as.numeric(x$repu_estimated_pi))
  ))
  
  
  
  # make a plot of the results
  require(ggplot2)
  
  # here we facet it by reporting unit
  g <- ggplot(data = rep_long, aes(x = Truth, y = Est, color = Repu)) + 
    geom_point(size = point_size, shape = point_shape) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_color_manual(name = "Reporting Units", values = c("red", "blue", "green", "yellow")[1:length(repu.f)]) +
    xlab("True (Simulated) Mixing Proportion") +
    ylab("Estimated Mixing Proportion") + 
    ggtitle(paste(plot_prefix, "simulated mixing proportions estimated by reporting unit"))
  repu_plot <- g + theme_bw() + facet_wrap( ~ Repu)
  ggsave(filename = paste(plot_prefix, "_repu_mix_sim.pdf", sep = ""), repu_plot, width = 8, height = 8)
  
  # and here by population
  p <- ggplot(data = pop_long, aes(x = Truth, y = Est, color = Repu)) + 
    geom_point(size = point_size, shape = point_shape) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_color_manual(name = "Reporting Units", values = c("red", "blue", "green", "yellow")[1:length(repu.f)]) +
    xlab("True (Simulated) Mixing Proportion") +
    ylab("Estimated Mixing Proportion") + 
    ggtitle(paste(plot_prefix, "simulated mixing proportions estimated by population"))
  pop_plot <- p + theme_bw() + facet_wrap( ~ Pop) 
  ggsave(filename = paste(plot_prefix, "_pop_mix_sim.pdf", sep = ""), pop_plot, width = 10, height = 10)
  
  # haven't put the truth on there yet.
  
  # need to incorporate reporting unit summaries in there too.
}