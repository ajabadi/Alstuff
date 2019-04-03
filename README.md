My personal functions and wrappers. Not recommended for public use.

# roxygen2_template.R
A template for package R files in _./R_, already including most of _roxygen2_ arguments.

# roxygenise.R
Manual test, build and installation for more control. Self-contained.

# tests
package test files. the ones in _./tests/manual_ to be run using _./roxygenise.R_.

# R
Contains all R functions, names begin with the function/wrapper category. Anything not exported is used only internally.

For faster search, you can search files by category:

  * ###stats: statistical code
  * ###file:  file handling (read, write, backup and log etc)
  * ###plot:  plot wrappers - helpers
  * ###misc:  anything else

Update the tags if needed.

  
# examples
Wrapper and function examples

# DESCRIPTION
To be updated with any package dependency/import added

# NAMESPACE
Managed by _roxygen2_.
