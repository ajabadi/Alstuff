library(testthat)
## parameters
base_dir <- "tests"
ex_file <- file.path(base_dir,'file_recorder_test.txt')
ex_dir <- 'file_recorder_test_tmp'
log_file_name <- "ex_log" ## without .log
run_descr <- "example run with no parameter"

ex_dir_full <- file.path(base_dir, ex_dir)
## in case they're there already
unlink(ex_dir_full, recursive = TRUE, force = TRUE)
if(file.exists(ex_file)){
  file.remove(ex_file)
}
## create from scratch
dir.create(ex_dir_full)
file.create(ex_file)
## before logging
files_before <- list.files(ex_dir_full) ## nothing
## after
file_recorder(outFile = ex_file,
              logFolder = ex_dir,
              logFile = log_file_name,
              date_ext=c("hour","second"),
              run_spec = run_descr)

files_after <- list.files(ex_dir_full, recursive = TRUE)
## tests
expect_true(any(grepl(log_file_name, files_after)))
expect_true(any(grepl(parent_base_ext(ex_file)$base, files_after)))
expect_true(length(files_after)==2)
## remmove files
unlink(ex_dir_full, recursive = TRUE, force = TRUE)
file.remove(ex_file)
message("\nTest Successful!\n")
