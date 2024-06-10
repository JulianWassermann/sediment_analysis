#!/usr/bin/env Rscript
suppressPackageStartupMessages({
library(G2Sd)
library(ggplot2)
library(argparse)
library(readr)
library(dplyr)
})
# you will need more libraries than those above

# given a plot, work out it's limits
get_plot_limits<-function(plot_object) {
# Use ggplot2, specifically a ggplot_build() function to extract the plot limits
  
}

read_sed_data<-function(input_file) {

}


read_core_depths <- function(depth_file) {

}

calculate_grain_percentages<-function(grain_sizes, grain_stats, gs_data_matrix) {

}


main <- function(args) {
        
    if (is.null(args$depth_data)) {
          # default depths file
          depth_file <- "depth_data.csv"
    } else {
          depth_file <- args$depth_data 
    }

    # now use the gradistats package to get the mean, kurtosis, etc and the % 
    # in each standard grain size bucket
    grain_stats <- granstat(gs_data_matrix)

    # set-up the order in which we want grainsizes to be displayed (and which we want to be displayed)
    grain_sizes <- c("Silt",
                     "VCSilt",
                     "VFS",
                     "FS",
                     "MS",
                     "CS",
                     "VCS",
                     "VFG"
                     )
    # Note, leaving off coarse grains

    # save your plot
}

if(sys.nframe() == 0) {

  # main program, called via Rscript
  parser = ArgumentParser(
                    prog="Sediment Analysis",
                    description="Plot graphs of sediment grain stats for a single core"
                    )
  parser$add_argument("core_filename",
                    help="the file to read the core data from")
  parser$add_argument("output_filename",
                    help="the file to save the graphic to")
  parser$add_argument('-v', '--verbose',
                    action='store_true',
                    help="Print progress")
  parser$add_argument('-d', '--depth_file',
                    help="Optional file for depth data")

  args = parser$parse_args()  
  main(args)
}
