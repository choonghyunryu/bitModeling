## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70)
options(tibble.print_min = 4L, tibble.print_max = 4L)

library(bitmodeling)
library(dplyr)
library(ggplot2)

## ---- eval=FALSE--------------------------------------------------------------
#  eda_category(
#    x,
#    target_variable,
#    positive = "1",
#    output_file = NULL,
#    output_dir = getwd(),
#    sample_percent = 100,
#    parallel = FALSE,
#    cores = parallel::detectCores() - 2,
#    future_globals_maxsize = 500 * 1024^2,
#    verbose = TRUE
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  testdata <- dlookr::heartfailure
#  target_variable <- "death_event"
#  
#  # single core processing
#  eda_category(testdata, target_variable, positive = "Yes")

## ---- eval=FALSE--------------------------------------------------------------
#  eda_category(testdata, target_variable, positive = "Yes", sample_percent = 30)

## ---- eval=FALSE--------------------------------------------------------------
#  eda_category(testdata, target_variable, positive = "Yes",
#               parallel = TRUE, cores = 8)

## ---- eval=FALSE--------------------------------------------------------------
#  eda_numeric(
#    x,
#    target_variable,
#    positive = "1",
#    output_file = NULL,
#    output_dir = getwd(),
#    sample_percent = 100,
#    trim_quantile = NULL,
#    parallel = FALSE,
#    cores = parallel::detectCores() - 2,
#    future_globals_maxsize = 500 * 1024^2,
#    verbose = TRUE
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  testdata <- dlookr::heartfailure
#  target_variable <- "death_event"
#  
#  # single core processing
#  eda_numeric(testdata, target_variable, positive = "Yes")

## ---- eval=FALSE--------------------------------------------------------------
#  eda_numeric(testdata, target_variable, positive = "Yes", trim_quantile = NA)

## ---- eval=FALSE--------------------------------------------------------------
#  eda_numeric(testdata, target_variable, positive = "Yes", trim_quantile = c(0, 95))

## ---- eval=FALSE--------------------------------------------------------------
#  eda_numeric(testdata, target_variable, positive = "Yes", sample_percent = 30)

## ---- eval=FALSE--------------------------------------------------------------
#  eda_numeric(testdata, target_variable, positive = "Yes",
#              parallel = TRUE, cores = 8)

