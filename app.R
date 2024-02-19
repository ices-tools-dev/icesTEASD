# Launch the ShinyApp

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

run_this_app <- get("run_app", envir = as.environment("package:icesTEASD"))
run_this_app() # add parameters here (if any)
