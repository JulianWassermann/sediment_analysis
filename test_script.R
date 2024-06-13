suppressPackageStartupMessages({
library(testthat)
library(ggplot2)
})

# Run like:
#jh1889@mirovia:~/work/teaching/SEPwC_assessments/sediment_assessment/test$ Rscript test_script.R 
# Test passed 🥇
# Test passed 🌈
# Test passed 🎊
# Test passed 🥳
# ── Warning: check main# 
# ...
#

# load in the script you want to test
source("../sediment_analysis.R")

# tests --------------------
# check the get_plot_limit function

test_that("get_plot_limits", {
    data.mwe = data.frame(x = 1:5, 
                      y = 2:6)
    g<-ggplot(data.mwe, aes(x, y)) + 
    geom_line() + 
    coord_cartesian(xlim = c(2, 4),
                    ylim = c(2, 6),
                    expand = FALSE)
    expect_equal(get_plot_limits(g)$xmin, 2)
    expect_equal(get_plot_limits(g)$xmax, 4)
    expect_equal(get_plot_limits(g)$ymin, 2)
    expect_equal(get_plot_limits(g)$ymax, 6)

    g<-NA
    expect_equal(get_plot_limits(g),NA)
})

test_that("read_sed_data_test", {
    test_data<-"data/test_data.xlsx"
    gs_data <- read_sed_data(test_data)
    expect_true(is.matrix(gs_data))
    expect_equal(rownames(gs_data),c("0.5","1","1.5","2","2.5"))
    expect_equal(colnames(gs_data),c("11","11.25"))
    expect_equal(as.numeric(gs_data[1,][1]),0)
    expect_equal(as.numeric(gs_data[5,][1]),70)
})

# now let's try on real data
test_that("read_sed_data_read", {
    test_data<-"../data/core_16.xlsx"
    gs_data <- read_sed_data(test_data)
    expect_true(is.matrix(gs_data))
    expect_equal(length(rownames(gs_data)),69)
    expect_equal(length(colnames(gs_data)),40)
    expect_equal(as.numeric(gs_data[1,][1]),0)
    expect_gt(as.numeric(gs_data[69,][40]),0.0116)
})

# check errors OK
# first is handled by readxl, so checkd this function is being used
test_that("read_sed_data_error", {
    test_data<-"../data/core_16.xlsx"
    expect_error(read_sed_data(test_data),regexp="`path` does not exist:")
})

test_that("read_core_depths", {
    depth_file = "../data/depth_data.csv"
    depths <- read_core_depths(depth_file)
    index <- basename("data/core_16.xlsx")
    expect_equal(depths[index,"Top"],22.75)
    expect_equal(depths[index,"Bottom"],30)
})

test_that("check grain_stats", {
    test_data<-"../data/core_16.xlsx"
    gs_data_matrix <- read_sed_data(test_data)
    grain_stats <- granstat(gs_data_matrix)
    expect_equal(as.numeric(grain_stats$X21[1]), 123.702)
    expect_equal(grain_stats$X21[27], "Muddy Sand")
})

test_that("check grain_percentages", {
    test_data<-"../data/core_16.xlsx"
    grain_sizes <- c("Silt",
                     "VCSilt",
                     "VFS",
                     "FS",
                     "MS",
                     "CS",
                     "VCS",
                     "VFG"
                    )
    gs_data_matrix <- read_sed_data(test_data)
    grain_stats <- granstat(gs_data_matrix)
    grain_percentages <- calculate_grain_percentages(grain_sizes, grain_stats, gs_data_matrix)
    expect_equal(grain_percentages$Depth[1], 21)
    expect_equal(grain_percentages$Percent[1], 0.22952)
    expect_true(is.factor(grain_percentages$Grain.Size[1]))
    expect_equal(as.character(grain_percentages$Grain.Size[1]), "Silt")
})


test_that("check main", {
    main(args=c("../data/core_16.xlsx","test.pdf"))
    expect_gt(file.info("test.pdf")$size,5000)
})



if (requireNamespace("lintr")) {
    library(lintr)

    context("linting script")
    test_that("Coding style", {
        output<-lintr::lint("../sediment_analysis.R")
        expect_lt(length(output),500)
        expect_lt(length(output),400)
        expect_lt(length(output),250)
        expect_lt(length(output),100)
        expect_lt(length(output),50)
        expect_lt(length(output),10)
        expect_equal(length(output),0)
    })
}


