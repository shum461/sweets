test_that("sweet_save errors for a non-existent asmbdat path", {
  expect_error(
    sweet_save(asmbdat_path = "/nonexistent/path/asmbdat"),
    regexp = "not a valid path"
  )
})

test_that("sweet_save errors when path base name is not asmbdat or data", {
  bad_dir <- file.path(tempdir(), paste0("myproject_", Sys.getpid()))
  dir.create(bad_dir, showWarnings = FALSE)
  on.exit(unlink(bad_dir, recursive = TRUE))
  expect_error(sweet_save(asmbdat_path = bad_dir), regexp = "not a valid data directory")
})

test_that("sweet_save copies files into a dated archive sub-directory", {
  base    <- file.path(tempdir(), paste0("swsave_", Sys.getpid()))
  asmb    <- file.path(base, "asmbdat")
  dat_dir <- file.path(base, "data")
  dir.create(asmb,    recursive = TRUE, showWarnings = FALSE)
  dir.create(dat_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base, recursive = TRUE))

  writeLines("a,b\n1,2", file.path(dat_dir, "results.csv"))

  suppressMessages(sweet_save(asmbdat_path = asmb))

  archive_root <- file.path(dat_dir, "archive")
  expect_true(dir.exists(archive_root))
  copied_files <- list.files(archive_root, recursive = TRUE)
  expect_true(any(grepl("results.csv", copied_files)))
})
