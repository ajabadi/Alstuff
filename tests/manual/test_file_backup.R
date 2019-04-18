library(testthat)
unlink('~/Desktop/test-file_backup', recursive = TRUE, force=TRUE)
setwd('~/Desktop')
## working directory -src
dir.create('test-file_backup/src', recursive = TRUE)
setwd('test-file_backup/src')
file.create('test.txt')
cat('something', file = 'test.txt')

dir.create('../dest', recursive = TRUE)
dirs_1 <- list.dirs()
files_1 <- list.files(recursive = TRUE)
## backup while in src in ./__backup
file_backup(file = 'test.txt', where = NA, run_spec = 'test run')
dirs_2 <- list.dirs()
files_2 <- list.files(recursive = TRUE)

test_that("__backup is only and the only folder created in src",
          expect_equal(base::setdiff(dirs_2, dirs_1), './__backup'))

test_that("two new files are created in ./__backup",
          expect_equal(length(base::setdiff(files_2, files_1)), 2))

## backup a file from another directory
file.create('../dest/text2.txt')
dirs_1 <- list.dirs('../dest')
files_1 <- list.files('../dest', recursive = TRUE)
file_backup('../dest/text2.txt', where = NA)
dirs_2 <- list.dirs('../dest')
files_2 <- list.files('../dest',recursive = TRUE)


test_that("__backup is only and the only folder created in src",
          expect_equal(base::setdiff(dirs_2, dirs_1), '../dest/__backup'))

test_that("two new files are created in ./__backup",
          expect_equal(length(base::setdiff(files_2, files_1)), 2))

## backup from src in dest relatively
files_1 <- list.files('../dest', recursive = TRUE)
file_backup('test.txt', where = '../dest')
files_2 <- list.files('../dest',recursive = TRUE)

test_that("two new files are created in dest",
          expect_equal(length(base::setdiff(files_2, files_1)), 2))


## backup from src in new_dir full path
dir.create('../new_dir')
fullPath <- R.utils::filePath(getwd(), '../new_dir')
files_1 <- list.files(fullPath, recursive = TRUE)
file_backup('test.txt', where = fullPath)
files_2 <- list.files(fullPath,recursive = TRUE)

test_that("two new files are created in full path dest",
          expect_equal(length(base::setdiff(files_2, files_1)), 2))

## toplevel test
file_backup(file='test.txt', where = '~/Downloads/test123/sub', toplevel = '/Users/alabadi')
test_that('external backup works',
          expect_true(length(list.files('~/Downloads/test123/sub', recursive = TRUE))==2)
          )

## remove all created folders/files
unlink('~/Desktop/test-file_backup', recursive = TRUE, force=TRUE)
unlink('~/Downloads/test123/sub', recursive = TRUE, force=TRUE)
