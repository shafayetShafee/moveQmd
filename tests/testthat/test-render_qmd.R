expect_file <- function(...) {
  x <- file.path(...)
  expect_true(file.exists(x), x)
}

expect_no_file <- function(...) {
  x <- file.path(...)
  expect_false(file.exists(x), x)
}

test_that("File moving for html when it has dependency", {
  render_qmd(test_path("index.qmd"), "dest/output.html")
  expect_file("dest/index_files")
  expect_file("dest/output.html")
})


test_that("File moving for html when it has no dependency", {
  render_qmd(test_path("index_without_dep.qmd"), "dest/output.html")
  expect_no_file("dest/index_without_dep_files")
  expect_file("dest/output.html")
})
