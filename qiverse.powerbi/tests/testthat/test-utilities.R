context("utilities")

test_that("decompress_string works", {
  # Example to decode a PowerBI Compressed Table String to JSON
  table_str <- "i45W8lXSUfJNzElVitWJVnIDctxSc2HcCCDXMS+/JCO1SKEktSgXLBgKFAzNy87LL88D8/2AfL/8EoXigtTkzLTM1BSl2FgA"
  result <- decompress_string(table_str)
  expect_equal(
    result,
    '[["M","Male"],["F","Female"],["X","Another term"],["U","Unknown"],["N","Not specified"]]'
  )

  # Check handling of multiple input strings
  table_strs <- c(
    "i45WMlDSUfLLz0tVitWJVjIEcjwSUxQ8HJ01ijXBQnmlOTlA0dC8nMy87NQUpdhYAA==",
    "i45W8ssvUXAsS8zMSUzKSVXSUTJQitWJVvKoLMjPSywpSkzNzUwEihqCRf3yi3ITc4BcI5ii1CJkVcZKsbEA"
  )

  true_res <- c(
    '[["0","None"],["1","Had HAC(s)"],["null","Unlinked"]]',
    '[["Not Available","0"],["Hyponatraemia","1"],["Normal","2"],["Hypernatraemia","3"]]'
  )

  expect_equal(
    decompress_string(table_strs),
    true_res
  )
})

test_that("queries escaped for xml", {
  query <- "FILTER(ColQuery, NOT(ISBLANK([ColumnName])) && NOT(ISBLANK([TableName])))"
  expect_equal(
    escape_xml_query(query),
    "FILTER(ColQuery, NOT(ISBLANK([ColumnName])) &amp;&amp; NOT(ISBLANK([TableName])))"
  )

  query <- "FILTER(ColQuery, NOT(ISBLANK([ColumnName])) < 1)"
  expect_equal(
    escape_xml_query(query),
    "FILTER(ColQuery, NOT(ISBLANK([ColumnName])) &lt; 1)"
  )
  query <- "FILTER(ColQuery, NOT(ISBLANK([ColumnName])) > 1)"
  expect_equal(
    escape_xml_query(query),
    "FILTER(ColQuery, NOT(ISBLANK([ColumnName])) &gt; 1)"
  )
  query <- "FILTER(ColQuery, NOT(ISBLANK([ColumnName])) <= 1)"
  expect_equal(
    escape_xml_query(query),
    "FILTER(ColQuery, NOT(ISBLANK([ColumnName])) &lt;= 1)"
  )
  query <- "FILTER(ColQuery, NOT(ISBLANK([ColumnName])) >= 1)"
  expect_equal(
    escape_xml_query(query),
    "FILTER(ColQuery, NOT(ISBLANK([ColumnName])) &gt;= 1)"
  )
})
