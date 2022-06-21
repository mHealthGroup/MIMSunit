check_epoch = function(breaks) {
  # taken from cut.POSIXct
  if (is.character(breaks) && length(breaks) == 1L) {
    by2 <- strsplit(breaks, " ", fixed = TRUE)[[1L]]
    if (length(by2) > 2L || length(by2) < 1L)
      stop("invalid specification of 'breaks'")
    valid <- pmatch(by2[length(by2)], c("secs", "mins", "hours",
                                        "days", "weeks", "months", "years", "DSTdays", "quarters"))
    if (is.na(valid)) {
      stop("invalid specification of 'breaks'")
    }
  }
}