test_that("import_mhealth_csv matches legacy readr parsing for package examples", {
  for (filename in c("mhealth.csv", "mhealth1.csv")) {
    filepath <- system.file("extdata", filename, package = "MIMSunit")
    coltypes <- list(
      readr::col_datetime(format = "%Y-%m-%d %H:%M:%OS"),
      readr::col_double(),
      readr::col_double(),
      readr::col_double()
    )
    expected <- readr::with_edition(
      1,
      readr::read_csv(file = filepath,
                      quoted_na = TRUE,
                      col_types = coltypes)
    )
    colnames(expected) <- c("HEADER_TIME_STAMP", "X", "Y", "Z")
    expected <- data.frame(expected, stringsAsFactors = FALSE)

    result <- import_mhealth_csv(filepath)

    expect_equal(dim(result), c(480L, 4L))
    expect_equal(colnames(result), c("HEADER_TIME_STAMP", "X", "Y", "Z"))
    expect_s3_class(result$HEADER_TIME_STAMP, "POSIXct")
    expect_true(all(vapply(result[c("X", "Y", "Z")], is.numeric, logical(1))))
    expect_equal(result, expected)
  }
})

test_that("import_mhealth_csv preserves quoted NA behavior", {
  filepath <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "HEADER_TIME_STAMP,VALUE",
      "2017-03-16 12:25:50.000,\"NA\"",
      "2017-03-16 12:25:50.013,NA",
      "2017-03-16 12:25:50.025,1.5"
    ),
    filepath
  )

  expect_silent(result <- import_mhealth_csv(filepath))

  expect_equal(colnames(result), c("HEADER_TIME_STAMP", "VALUE"))
  expect_s3_class(result$HEADER_TIME_STAMP, "POSIXct")
  expect_true(is.numeric(result$VALUE))
  expect_true(is.na(result$VALUE[1]))
  expect_true(is.na(result$VALUE[2]))
  expect_equal(result$VALUE[3], 1.5)
})
