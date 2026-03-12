test_that("delimp_biscuit errors for a non-existent directory", {
  expect_error(
    delimp_biscuit(delimp_paths = "/nonexistent/path/delimp_xyz"),
    regexp = "not a valid directory"
  )
})

test_that("delimp_biscuit returns NULL invisibly for an empty directory", {
  empty_dir <- file.path(tempdir(), paste0("empty_delimp_", Sys.getpid()))
  dir.create(empty_dir, showWarnings = FALSE)
  on.exit(unlink(empty_dir, recursive = TRUE))

  result <- suppressMessages(delimp_biscuit(delimp_paths = empty_dir))
  expect_null(result)
})
