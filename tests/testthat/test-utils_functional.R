crew_test("fltr()", {
  expect_equal(fltr(seq_len(10), ~.x < 5), seq_len(4))
})

crew_test("map()", {
  expect_equal(unname(map(letters, identity)), as.list(letters))
})

crew_test("map_chr()", {
  expect_equal(unname(map_chr(letters, identity)), letters)
})

crew_test("map_dbl()", {
  x <- as.numeric(seq_len(4))
  expect_equal(map_dbl(x, identity), x)
})

crew_test("map_int()", {
  expect_equal(map_int(seq_len(4), identity), seq_len(4))
})

crew_test("map_lgl()", {
  expect_equal(map_lgl(c(TRUE, FALSE), identity), c(TRUE, FALSE))
})

crew_test("map_rows()", {
  x <- data.frame(x = seq_len(3), y = rep(1, 3), z = rep(2, 3))
  expect_equal(map_rows(x, ~sum(.x)), seq_len(3) + 3)
})

crew_test("walk", {
  envir <- new.env(parent = emptyenv())
  walk(letters, ~assign(.x, 1, envir = envir))
  expect_equal(sort(names(envir)), sort(letters))
})
