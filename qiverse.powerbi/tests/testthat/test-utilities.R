context("utilities")

test_that("table_str_to_json works", {

  # Example to decode a PowerBI Compressed Table String to JSON
  table_str <- "i45W8lXSUfJNzElVitWJVnIDctxSc2HcCCDXMS+/JCO1SKEktSgXLBgKFAzNy87LL88D8/2AfL/8EoXigtTkzLTM1BSl2FgA"
  result <- table_str_to_json(table_str)
  expect_equal(
    result,
    '[["M","Male"],["F","Female"],["X","Another term"],["U","Unknown"],["N","Not specified"]]'
  )
})
