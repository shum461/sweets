
samples <- broken_pk |> dplyr::filter(EVID == 0)

test_that("sweet_disposition returns a data frame with disposition_table attribute", {
  disp <- sweet_disposition(samples, subjid = USUBJID, group_vars = STUDYID)
  expect_s3_class(disp, "data.frame")
  expect_true(isTRUE(attr(disp, "disposition_table")))
})

test_that("sweet_disposition has correct column names", {
  disp <- sweet_disposition(samples, subjid = USUBJID, group_vars = STUDYID)
  expect_named(
    disp,
    c("Flag #", "Reason for Deletion", "STUDYID",
      "Samples Excluded", "Subjects Affected",
      "Subjects Excluded", "Remaining Samples", "Remaining Subjects")
  )
})

test_that("sweet_disposition errors when DELFN is missing", {
  no_delfn <- dplyr::select(samples, -DELFN)
  expect_error(
    sweet_disposition(no_delfn, subjid = USUBJID, group_vars = STUDYID),
    regexp = "DELFN"
  )
})

test_that("sweet_disposition pools and returns data.frame when group_vars omitted", {
  # cli_alert_warning() messages to console, not an R warning; just check it runs
  disp <- sweet_disposition(samples, subjid = USUBJID)
  expect_s3_class(disp, "data.frame")
  expect_true(isTRUE(attr(disp, "disposition_table")))
})

test_that("cnt_n_keeps marks flag as count-and-keep (Subjects Excluded = 0)", {
  disp_keep50 <- sweet_disposition(
    samples, subjid = USUBJID, group_vars = STUDYID, cnt_n_keeps = 50
  )
  excl_50 <- sum(disp_keep50[disp_keep50$`Flag #` == 50, "Subjects Excluded"])
  expect_equal(excl_50, 0)
})

test_that("subjects_in_flag attribute is a nested data frame by DELFN", {
  disp <- sweet_disposition(samples, subjid = USUBJID, group_vars = STUDYID)
  sif  <- attr(disp, "subjects_in_flag")
  expect_s3_class(sif, "data.frame")
  expect_true("DELFN" %in% names(sif))
})
