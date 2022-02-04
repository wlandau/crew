test_that("crew_assert", {
  expect_silent(crew_assert(TRUE))
  expect_error(crew_assert(FALSE), class = "crew_error")
})

test_that("crew_assert_chr_scalar", {
  expect_silent(crew_assert_chr_scalar("abc"))
  expect_error(crew_assert_chr_scalar(NULL), class = "crew_error")
  expect_error(crew_assert_chr_scalar(character(0)), class = "crew_error")
  expect_error(crew_assert_chr_scalar(""), class = "crew_error")
  expect_error(crew_assert_chr_scalar(TRUE), class = "crew_error")
  expect_error(crew_assert_chr_scalar(letters), class = "crew_error")
})

test_that("crew_assert_pos_dbl_scalar()", {
  expect_silent(crew_assert_pos_dbl_scalar(1))
  expect_error(crew_assert_pos_dbl_scalar(-1), class = "crew_error")
  expect_error(crew_assert_pos_dbl_scalar(numeric(0)), class = "crew_error")
  expect_error(crew_assert_pos_dbl_scalar(NULL), class = "crew_error")
  expect_error(crew_assert_pos_dbl_scalar("a"), class = "crew_error")
  expect_error(crew_assert_pos_dbl_scalar(seq_len(2)), class = "crew_error")
})

test_that("crew_assert_lgl_scalar()", {
  expect_silent(crew_assert_lgl_scalar(TRUE))
  expect_silent(crew_assert_lgl_scalar(FALSE))
  expect_error(crew_assert_lgl_scalar(1), class = "crew_error")
  expect_error(crew_assert_lgl_scalar(c(TRUE, FALSE)), class = "crew_error")
  expect_error(crew_assert_lgl_scalar(logical(0)), class = "crew_error")
})
