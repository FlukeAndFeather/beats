test_that("reading sample ECG data returns correct values", {
  # Read UBE data
  fp <- system.file("extdata", "max_ecg_190826b.ube", package = "beats")
  ecg_data <- read_ube(fp)
  ecg_head <- head(ecg_data)

  # Expected values
  expected_timestamp <- c("20-08-26 12:35:08.000", "20-08-26 12:35:08.009", "20-08-26 12:35:08.019", "20-08-26 12:35:08.029", "20-08-26 12:35:08.039", "20-08-26 12:35:08.049"
  )
  expected_ecg <- c(1418, 1418, 1476, 1382, 1467, 1424)

  # Tests
  expect_equal(format(ecg_head$timestamp, "%y-%m-%d %H:%M:%OS3"), expected_timestamp)
  expect_equal(ecg_head$ecg, expected_ecg)
})
