
#' Do the mixture simulations and make some nice ggplots of the results
#' 
#' Quickly through this together
#' @inheritParams herring_csv2gpiper
#' @param rep_unit_path  Path to the reporting unit file
#' @param plot_prefix    Prefix to put on the plot names 
#' @export
herring_mixture_sims <- function(baseline_path, lat_long_path, locus_columns, rep_unit_path, plot_prefix = "herring") {
  
  B <- herring_csv2gpiper(baseline_path, lat_long_path, locus_columns)
  
  # make a gsi_sim file of it, but really only to get the pops.f
  pops.f <- make_baseline_file(B)
  
  
  # handle the reporting units:
  r <- read.table(rep_unit_path, header = T, stringsAsFactors = FALSE)
  rf <- factor(r$RepGroup, levels = unique(r$RepGroup))
  names(rf) <- r$Pop
  repu.f <- rf[levels(pops.f)]
  
   
  # that was nice. Now do 4 simulations at each of 25 different random values of the mixing proportions
  set.seed(50)
  bycatch_N <- 1000
  many.reps <- lapply(1:25, function(x) {
    
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
                    Reps = 4, fixed = FALSE, repu.f = repu.f, samp_unit = "multilocus")
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
    geom_point() + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_color_manual(name = "Reporting Units", values = c("red", "blue", "green", "yellow")[1:length(repu.f)]) +
    xlab("True (Simulated) Mixing Proportion") +
    ylab("Estimated Mixing Proportion")
  repu_plot <- g + facet_wrap( ~ Repu)
  ggsave(filename = paste(plot_prefix, "_repu_mix_sim.pdf", sep = ""), repu_plot, width = 8, height = 8)
  
  # and here by population
  p <- ggplot(data = pop_long, aes(x = Truth, y = Est, color = Repu)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_color_manual(name = "Reporting Units", values = c("red", "blue", "green", "yellow")[1:length(repu.f)]) +
    xlab("True (Simulated) Mixing Proportion") +
    ylab("Estimated Mixing Proportion")
  pop_plot <- p + facet_wrap( ~ Pop)
  ggsave(filename = paste(plot_prefix, "_pop_mix_sim.pdf", sep = ""), pop_plot, width = 10, height = 10)
  
  # haven't put the truth on there yet.
  
  # need to incorporate reporting unit summaries in there too.
}