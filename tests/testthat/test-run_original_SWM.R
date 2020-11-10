context("test-run_original_SWM")

test_that("run_original_SWM works with all defaults", {
  data("REW_1978_2013")
  expect_identical(run_original_SWM(), REWdata)
})

test_that("run_original_SWM works changing rainfall data", {
  data("stoc")
  expect_identical(run_original_SWM(stoc[1:5000,]),run_original_SWM()[1:5000,] )
})

test_that("run_original_SWM works when soil data is inputed from the user side.", {
  data("soil_data")
  expect_equal(run_original_SWM(), run_original_SWM(soil_data = soil_data))
})

test_that("run_original_SWM works when understorey parameters are inputed from the user side", {
  # default_understorey_parameters <- check_format_understorey_parameters(); save(default_understorey_parameters, file = "./data/default_understorey_parameters.RData")
  data("default_understorey_parameters")
  expect_identical(run_original_SWM(), run_original_SWM(understorey_parameters = default_understorey_parameters))
  # understorey_parameters <- default_understorey_parameters
  # expect_identical(run_original_SWM(), run_original_SWM(understorey_parameters))
})

test_that("run_original_SWM works when interception parameters are inputed from the user side", {
  # default_interception_parameters <- check_format_interception_parameters(); save(default_interception_parameters, file = "./data/default_interception_parameters.RData")
  data("default_interception_parameters")
  expect_identical(run_original_SWM(), run_original_SWM(interception_parameters = default_interception_parameters))
})

# test_that("run_original_SWM works when roots data are inputed from the user side", {
#   # default_interception_parameters <- check_format_interception_parameters(); save(default_interception_parameters, file = "./data/default_interception_parameters.RData")
#   data("ParamRoots")
#   # run_original_SWM(roots_data = ParamRoots) -> t
#   expect_success()
#
# })

test_that("run_original_SWM works when PAI is inputed from the user side", {
  expect_identical(run_original_SWM(), run_original_SWM(PAI = 7))
})

test_that("run_original_SWM works when transpiration parameters are inputed from the user side", {
  # data("default_arguments"); default_transpiration_parameters <- list(ThetaTr = as.numeric(format(default_arguments[["ThTr"]], digits = 17)), Threshold = default_arguments[["p"]][3]); save(default_transpiration_parameters, file = "./data/default_transpiration_parameters.RData")
  data("default_transpiration_parameters")
  expect_equal(run_original_SWM(), run_original_SWM(transpiration_parameters = default_transpiration_parameters))
})
