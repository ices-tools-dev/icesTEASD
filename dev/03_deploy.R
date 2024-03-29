# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

# Deploy

# we deploy only through github actions, and when code is changed on the main
# branch :)
# update renv.lock file
deps <- renv::dependencies()
deps <- deps[!grepl("/dev/", deps$Source) & !grepl("rsconnect", deps$Source), ]
file <- renv::lockfile_create(packages = unique(deps$Package))
renv::lockfile_write(file)

# bump description file
desc::desc_bump_version(which = "dev")


# Deploy to Posit Connect or ShinyApps.io
# In command line.
rsconnect::deployApp(
  appName = desc::desc_get_field("Package"),
  appTitle = "Assessment EG Secretariat Checks",
  appFiles = c(
    # Add any additional files unique to your app here.
    "R/",
    "inst/",
    "man/",
    "NAMESPACE",
    "DESCRIPTION",
    ".Rbuildignore",
    "app.R"
  ),
  forceUpdate = TRUE,
  account = "ices-tools-dev",
)
