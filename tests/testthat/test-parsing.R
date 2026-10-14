test_that("sarb_num handles decimal and grouping commas", {
  expect_equal(sarb_num(c("6,75", "1,234,567", "12.5", "1,234.5", "-", "", "0.00")),
               c(6.75, 1234567, 12.5, 1234.5, NA, NA, 0))
  expect_equal(sarb_num(c("-3,5", "-1,000")), c(-3.5, -1000))
  expect_equal(sarb_num(7.25), 7.25)
})

test_that("parse_sarb_form reads attribute values and maps zero-padded headers", {
  skip_if_not_installed("xml2")
  xml <- paste0('<SARBForms><SARBForm><Table TableNumber="1" TableDescription="LIABILITIES">',
                '<ColumnHeader ColumnNumber="1" ColumnDescription="Cheque"/>',
                '<ColumnHeader ColumnNumber="7" ColumnDescription="Total"/>',
                '<Row RowNumber="1" ItemDescription="Heading"/>',
                '<Row RowNumber="2" ItemNumber="1" ItemDescription="Total deposits">',
                '<Column ColumnNumber="0001" Value="10.00"/><Column ColumnNumber="0007" Value="25"/></Row>',
                '</Table><Table TableNumber="2" TableDescription="ASSETS">',
                '<Row RowNumber="1" ItemNumber="1" ItemDescription="Mortgages">',
                '<Column ColumnNumber="0005" Value="99"/></Row></Table></SARBForm></SARBForms>')
  d <- parse_sarb_form(xml)
  expect_equal(nrow(d), 3L)
  expect_equal(d$col_desc[d$col_no == "0001"], "Cheque")
  expect_equal(d$value[d$table_no == "2"], 99)
  expect_false(any(d$item_desc == "Heading"))
})

test_that("FRED csv parser handles both header styles and missing markers", {
  a <- parse_fred_csv("observation_date,GS10\n2020-01-01,1.76\n2020-02-01,.\n")
  b <- parse_fred_csv("DATE,GS10\n2020-01-01,1.76\n")
  expect_equal(nrow(a), 1L); expect_equal(a$value, 1.76); expect_equal(b$value, 1.76)
})

test_that("institution selection matches by pattern and keeps the total", {
  ins <- data.frame(Id = c("TOTAL", "1", "2", "3"),
                    Name = c("*TOTAL*", "ABSA BANK LTD", "THE STANDARD BANK OF S A LTD",
                             "STANDARD CHARTERED BANK"))
  s <- select_institutions(ins)
  expect_setequal(s$key, c("TOTAL", "ABSA", "STANDARD"))
  expect_false("3" %in% s$Id)
})
