test_that("fltr()", {
  expect_equal(fltr(seq_len(10), ~.x < 5), seq_len(4))
})

test_that("map_chr()", {
  expect_equal(unname(map_chr(letters, identity)), letters)
})

test_that("map_dbl()", {
  x <- as.numeric(seq_len(4))
  expect_equal(map_dbl(x, identity), x)
})

test_that("map_int()", {
  expect_equal(map_int(seq_len(4), identity), seq_len(4))
})

test_that("map_lgl()", {
  expect_equal(map_lgl(c(TRUE, FALSE), identity), c(TRUE, FALSE))
})

test_that("map_rows()", {
  x <- data_frame(x = seq_len(3), y = rep(1, 3), z = rep(2, 3))
  expect_equal(map_rows(x, ~sum(.x)), seq_len(3) + 3)
})

test_that("walk", {
  envir <- new.env(parent = emptyenv())
  walk(letters, ~assign(.x, 1, envir = envir))
  expect_equal(sort(names(envir)), sort(letters))
})
