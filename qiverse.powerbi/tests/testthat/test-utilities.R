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
