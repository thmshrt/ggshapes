library(devtools)
library(testthat)
library(rlang)
library(magrittr)
library(grid)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringi)
# devtools::install_github('thmshrt/writethat')
library(writethat)

rm(list=ls())

# # create the package
# usethis::create_package('.')
#
# # setup tests
# usethis::use_testthat()
#
# # setup readme
# usethis::use_readme_rmd()
#
# # setup git
# usethis::use_git_config(
#   scope = 'project',
#   user.name = 'Thomas Hart',
#   user.email = 'thh4002@med.cornell.edu'
# )
# usethis::use_git()
#
# # setup dependencies
# usethis::use_package('devtools')
# usethis::use_package('testthat')
# usethis::use_package('rlang')
# usethis::use_package('magrittr')
# usethis::use_package('grid')
# usethis::use_package('readr')
# usethis::use_package('ggplot2')
# usethis::use_package('tibble')
# usethis::use_package('dplyr')
