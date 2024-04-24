
test_that("multiplication works", {
  withr::local_options(width= 20)
  expect_snapshot(
    waldo::compare(fd_old,fd_new)
  )
})
