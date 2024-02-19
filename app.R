# Launch the ShinyApp

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

print("hello")
cat(capture.output(search()))
cat(ls())

#run_app_icesTEASD() # add parameters here (if any)

x <- get("run_app_icesTEASD", pos = 1)
x()
