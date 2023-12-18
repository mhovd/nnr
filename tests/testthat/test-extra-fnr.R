test_that("Correct birthdate", {
  expect_equal(fnr_to_birthdate(fnr = 19053826639), lubridate::ymd("1938-05-19"))
})


test_that("Fails on wrong input", {
  expect_error(fnr_to_birthdate(fnr = 190538))
})
