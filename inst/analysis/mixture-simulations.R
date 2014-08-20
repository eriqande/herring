library(herring)

# quick script to do the mixture simulations and make some plots
bbs <- blueback_run_settings()
aas <- alewife_run_settings()

# start out with blueback
message("Starting blueback mixture sims")
herring_mixture_sims(baseline_path = bbs$baseline_path, 
                     lat_long_path = bbs$lat_long_path, 
                     locus_columns = bbs$locus_columns, 
                     rep_unit_path = bbs$rep_unit_path, 
                     plot_prefix = "blueback")



# then do alewife
message("starting alewife mixture sims")
herring_mixture_sims(baseline_path = aas$baseline_path, 
                     lat_long_path = aas$lat_long_path, 
                     locus_columns = aas$locus_columns, 
                     rep_unit_path = aas$rep_unit_path, 
                     plot_prefix = "alewife")

                    