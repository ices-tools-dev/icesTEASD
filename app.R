# Launch the ShinyApp

options(shiny.autoload.r = FALSE)

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

print("hello")

run_app_icesTEASD() # add parameters here (if any)
