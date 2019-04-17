setwd('/Users/alabadi/Documents/_Projects/_Personal/someWrappers')
############# build all documentation for the package and update namespace
roxygen2::roxygenise()
############# load the pks to access functions
devtools::load_all()
############# testthat
testthat::test_package("someWrappers")
############# test examples
## path to Rd files - will let you know if there are any errors/warnings
testthat::test_example("man/file_recorder.Rd")
testthat::test_example("man/parent_base_ext.Rd")
############# manual tests
## test files that cannot be included in standard testthat pipeline
invisible(lapply(list.files("tests/manual", full.names = TRUE), source))
############# check
# devtools::check()
############# build it
## by default, it is made in parent directoy of the package, we manually set it.
binary_dir <- "../__binary"
if(!dir.exists(binary_dir)){
  dir.create(binary_dir)
}
pkg <- devtools::build(path = binary_dir)
############# install it in my library
install.packages(pkg, repos = NULL)
