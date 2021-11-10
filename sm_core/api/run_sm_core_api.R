#!/usr/bin/env Rscript

library(plumber)
pr("api/sm_core_api.R") %>%
  pr_run(port=8000, host="147.182.162.209")
# Setting the host option on a VM instance ensures the application can be accessed externally.
# (This may be only true for Linux users.)