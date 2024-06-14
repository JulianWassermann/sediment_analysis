#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(G2Sd)
  library(ggplot2)
  library(argparse)
  library(dplyr)
  library(tidyr)
  library(patchwork)
  library(readxl)
})

# Main body, use call command, figure out how to return a function
# given a plot, work out its limits
get_plot_limits <- function(data_exel) {
  # Build ggplot object for the data sets
  min_vale <- min(data_exel)
  max_value <- max(data_exel)
  
  # Return min and max values
  return(min_value, max_value)
}

read_sed_data <- function(depth_file) {
  # Check if the core_16 is a string
  if (!is.character(core_16)) {
    stop("Error: `core_16` must be a string")
  }
  
  # Implement logic to read sediment data from file
  data <- read_excel(core_16)
  return(data)
}

read_core_depths <- function(depth_file) {
  # Implement logic to read core depth data from file
  data <- read.csv(depth_file)
  return(data)
}

#calculate_grain_percentages <- function(grain_sizes, grain_stats, gs_data_matrix) {
#Placeholder for the actual implementation
#}

main <- function(args) {
  if (is.null(args$depth_data)) {
    # default depths file
    depth_file <- "depth_data.csv"
  } else {
    depth_file <- args$depth_data 
  }
  
  #grain_stats <- granstat(gs_data_matrix)
  
  # For percentage diagram
  plot1 <- ggplot(grain_stats, aes(x=x, y=y)) + geom_bar(stat = "identity") + xlim(25, 35) + ylim(0, 1)
  limits1 <- get_plot_limits()
  print("Plot 1 Limits (Percentage Diagram):")
  print(limits1)
  
  # For mean GS (mm) diagram
  plot2 <- ggplot(grain_stats, aes(x=x, y=y)) + geom_line() + xlim(25, 35) + ylim(0, 1.25)
  limits2 <- get_plot_limits()
  print("Plot 2 Limits (Mean GS (mm) Diagram):")
  print(limits2)
  
  # For kurtosis diagram
  plot3 <- ggplot(grain_stats, aes(x=x, y=y)) + geom_line() + xlim(25, 35) + ylim(0, 10)
  limits3 <- get_plot_limits()
  print("Plot 3 Limits (Kurtosis Diagram):")
  print(limits3)
  
  # For sorting diagram
  plot4 <- ggplot(grain_stats, aes(x=x, y=y)) + geom_line() + xlim(25, 35) + ylim(0, 10)
  limits4 <- get_plot_limits()
  print("Plot 4 Limits (Sorting Diagram):")
  print(limits4)
  
  # For skew diagram
  plot5 <- ggplot(grain_stats, aes(x=x, y=y)) + geom_line() + xlim(25, 35) + ylim(0, 10)
  limits5 <- get_plot_limits()
  print("Plot 5 Limits (Skew Diagram):")
  print(limits5)
  
  # Plotting Diagrams
  ggsave("plot1.pdf", plot=plot1)
  ggsave("plot2.pdf", plot=plot2)
  ggsave("plot3.pdf", plot=plot3)
  ggsave("plot4.pdf", plot=plot4)
  ggsave("plot5.pdf", plot=plot5)
}

# now use the gradients package to get the mean, kurtosis, etc and the % 
# in each standard grain size bucket
#grain_stats <- granstat(gs_data_matrix)

# set-up the order in which we want grain sizes to be displayed (and which we want to be displayed)
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

if(sys.nframe() == 0) {
  
  # main program, called via Rscript
  parser <- ArgumentParser(
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
  
  args <- parser$parse_args()  
  main(args)
}