# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application and set some default {golem} options
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "cd2030.extractor", # The name of the golem package containing the app (typically lowercase, no underscore or periods)
  pkg_title = "CD2030 Data Extractor", # What the Package Does (One Line, Title Case, No Period)
  pkg_description = "A Shiny application for extracting and managing data from DHIS2.
    It provides an interactive interface to query, filter, and export DHIS2 data
    for further analysis. Designed for efficiency and ease of use in data-driven
    decision-making.", # What the package does (one paragraph).
  authors = c(
    person(
      given = "David", # Your First Name
      family = "Kariuki", # Your Last Name
      email = "hello@damurka.com", # Your email
      role = c("aut", "cre") # Your role (here author/creator)
    ),
    person("Martin", "Mutua", role = c("aut")),
    person("Peter", "Kaberia", , "pkaberia@aphrc.org", role = c("aut")),
    person("Godfrey", "Adero", , "gadero@aphrc.org", role = c("aut")),
    person("Rornald", "Muhumuza", , "rmuhumuza@aphrc.org", role = c("aut")),
    person("African Population and Health Research Center (APHRC)",
           role = c("cph", "fnd"))
  ),
  repo_url = NULL, # The URL of the GitHub repo (optional),
  pkg_version = "0.0.0.9000", # The version of the package containing the app
  set_options = TRUE # Set the global golem options
)

## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files ----
## See ?usethis for more information
# usethis::use_mit_license("Golem User") # You can set another license here
usethis::use_agpl3_license()
golem::use_readme_rmd(open = FALSE)
devtools::build_readme()
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
usethis::use_code_of_conduct(contact = "African Population and Health Research Center (APHRC)")
# usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## Use git ----
usethis::use_git()
## Sets the remote associated with 'name' to 'url'
usethis::use_git_remote(
  name = "origin",
  url = "https://github.com/damurka/dhis2.git"
)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
