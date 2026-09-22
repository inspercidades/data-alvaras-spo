library(testthat)

# Source the pipeline functions, matching how targets loads R/.
targets::tar_source()
testthat::test_dir("tests/testthat")
