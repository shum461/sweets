test_that("build_report returns a report_spec object", {
  tmp <- tempfile()
  rpt <- build_report(output_path = dirname(tmp), file_name = basename(tmp))
  expect_s3_class(rpt, "report_spec")
})

test_that("build_report sets file_path from output_path and file_name", {
  tmp <- tempfile()
  rpt <- build_report(output_path = dirname(tmp), file_name = basename(tmp))
  expect_true(grepl(basename(tmp), rpt$file_path, fixed = TRUE))
})

test_that("build_report output type is TXT", {
  tmp <- tempfile()
  rpt <- build_report(output_path = dirname(tmp), file_name = basename(tmp))
  expect_equal(rpt$output_type, "TXT")
})
