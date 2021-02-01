library(testthat)
library(nflfastR)

future::plan("multisession")

test_check("nflfastR")
