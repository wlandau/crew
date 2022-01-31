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
