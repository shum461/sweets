samples <- broken_pk |> dplyr::filter(EVID == 0)

test_that("pool_disposition errors on a plain data frame", {
  expect_error(pool_disposition(samples), regexp = "disposition table")
})

test_that("pool_disposition returns a data frame with a Grand Total row", {
  disp   <- sweet_disposition(samples, subjid = USUBJID, group_vars = STUDYID)
  pooled <- suppressMessages(pool_disposition(disp))
  expect_s3_class(pooled, "data.frame")
  expect_true("Grand Total" %in% pooled$`Reason for Deletion`)
})

test_that("pool_disposition collapses groups into one row per flag", {
  disp   <- sweet_disposition(samples, subjid = USUBJID, group_vars = STUDYID)
  pooled <- suppressMessages(pool_disposition(disp))
  # Each Flag # should appear only once (except Grand Total uses Reason for Deletion)
  non_total <- pooled[pooled$`Reason for Deletion` != "Grand Total", ]
  expect_equal(nrow(non_total), dplyr::n_distinct(non_total$`Reason for Deletion`))
})

test_that("pool_disposition Remaining Samples in Grand Total row is non-negative", {
  disp   <- sweet_disposition(samples, subjid = USUBJID, group_vars = STUDYID)
  pooled <- suppressMessages(pool_disposition(disp))
  grand  <- pooled[pooled$`Reason for Deletion` == "Grand Total", ]
  expect_gte(grand$`Remaining Samples`, 0)
})
