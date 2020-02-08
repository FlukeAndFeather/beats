test_that("reading sample ECG data returns correct values", {
  # Read UBE data
  fp <- system.file("extdata", "max_ecg_190826b.ube", package = "beats")
  ecg_data <- read_ube(fp)
  ecg_head <- head(ecg_data)

  # Expected values
  expected_timestamp <- c(1598470508, 1598470508.01, 1598470508.02, 1598470508.03, 1598470508.04, 1598470508.05)
  expected_ecg <- c(1418, 1418, 1476, 1382, 1467, 1424)

  # Tests
  expect_equal(as.numeric(ecg_head$timestamp), expected_timestamp)
  expect_equal(ecg_head$ecg, expected_ecg)
})
