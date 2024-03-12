
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MetroSystemEval

The goal of MetroSystemEval is to simplify the running of the R code for
King County Metro’s System Evaluation. The functions in this package
produce standard route level metrics for all periods of the day and all
days of the week and require GTFS files for Metro’s current and future
networks.

## Installation

You can install the development version of MetroSystemEval from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("melissa-gaughan/MetroSystemEval")
```

## Example

You can call the create_route_metrics() function on any GTFS that you
have on your computer. You need to supply a file path (make sure to
switch back slashes to forward slashes!) as well as determine whether
the GTFS represents current or future service. You also need to provide
a date for the start of the service change in “YYYY-MM-DD” format:

``` r
library(MetroSystemEval)
example_route_metric_table <- create_route_metrics(gtfs_type = "current", gtfs_path = fs::path_package("extdata", "233_gtfs.zip", package = "MetroSystemEval"),
                                  service_change_start_date ="2023-09-02")
View(example_route_metric_table)
```