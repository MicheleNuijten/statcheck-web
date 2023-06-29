library(shinytest2)

test_that("{shinytest2} recording: statcheck-web", {
  app <- AppDriver$new(variant = platform_variant(), name = "statcheck-web", height = 569, 
      width = 979)
  rlang::warn(paste0("`file` should be the path to the file, relative to the app's tests/testthat directory.\n", 
      "Remove this warning when the file is in the correct location."))
  app$upload_file(file = "Paper1.pdf")
  app$expect_screenshot()
})

