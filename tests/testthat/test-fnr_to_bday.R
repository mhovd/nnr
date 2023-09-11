
test_that("fnr_to_bday behaves correctly", {

  # Test valid inputs
  expect_equal(fnr_to_bday("19053826639"), dmy("19-05-1938")) # 20th century

  expect_error(fnr_to_bday("12345"), "Person number must be 11 characters long") # less than 11 characters
  expect_error(fnr_to_bday("123456789012"), "Person number must be 11 characters long") # more than 11 characters
})

