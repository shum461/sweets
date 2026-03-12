small_df <- broken_pk[1:10, c("USUBJID", "DELFN")]

test_that("listing_table errors without a title", {
  expect_error(listing_table(small_df), regexp = "title")
})

test_that("listing_table returns a table_spec object", {
  tbl <- listing_table(small_df, title = "Test Listing")
  expect_s3_class(tbl, "table_spec")
})

test_that("add_listing_to_report errors when deletion_report is NULL", {
  tbl <- listing_table(small_df, title = "Test")
  expect_error(add_listing_to_report(NULL, tbl), regexp = "deletion report")
})

test_that("add_listing_to_report adds content to the report", {
  tmp <- tempfile()
  rpt  <- build_report(output_path = dirname(tmp), file_name = basename(tmp))
  tbl  <- listing_table(small_df, title = "Flag 7 Placebo")
  rpt2 <- suppressMessages(add_listing_to_report(rpt, tbl))
  # Each add_listing_to_report call appends 2 content items (table + row count)
  expect_gt(length(rpt2$content), length(rpt$content))
})

test_that("add_listing_to_report warns on duplicate listing title", {
  tmp  <- tempfile()
  rpt  <- build_report(output_path = dirname(tmp), file_name = basename(tmp))
  tbl  <- listing_table(small_df, title = "Duplicate Title")
  rpt2 <- suppressMessages(add_listing_to_report(rpt, tbl))
  expect_warning(
    suppressMessages(add_listing_to_report(rpt2, tbl)),
    regexp = "already exists"
  )
})
