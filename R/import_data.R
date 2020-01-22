#' Import raw multi-channel accelerometer data stored in mHealth Specification
#'
#' \code{import_mhealth_csv} imports the raw multi-channel accelerometer data
#' stored in mHealth Specification.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a File IO
#'   function that is used to import data stored in mHealth Specification during
#'   algorithm validation.
#'
#' @param filepath string. The filepath of the input data.
#' @return dataframe. The imported multi-channel accelerometer signal, with the
#'   first column being the timestamps in POSXlct format, and the rest columns
#'   being accelerometer values in \eqn{g} unit.
#'
#' @family Filo I/O functions
#'
#' @export
import_mhealth_csv <- function(filepath) {
  ncols <- readr::count_fields(filepath, readr::tokenizer_csv(), skip = 0, n_max = 1L)
  date_format <- readr::col_datetime(format = "%Y-%m-%d %H:%M:%OS")
  coltypes <- list(date_format)
  colheaders <- c("HEADER_TIME_STAMP")
  for (i in 2:ncols) {
    coltypes <- append(coltypes, list(readr::col_double()))
  }

  df <- readr::read_csv(
    file = filepath,
    quoted_na = TRUE,
    col_types = coltypes
  )
  # convert factors back to characters
  col_classes <- sapply(1:ncols, function(i) {
    return(class(df[1, i]))
  })
  factor_cols <- which(col_classes == "factor")
  df[, factor_cols] <- as.character(df[, factor_cols])

  # enhance column headers
  colnames(df)[1:length(colheaders)] <- colheaders
  colnames(df) <- toupper(colnames(df))
  options(digits.secs = 3)
  df <- data.frame(df, stringsAsFactors = FALSE)

  return(df)
}

#' Import large raw multi-channel accelerometer data stored in mHealth Specification
#' in chunks.
#'
#' \code{import_mhealth_csv_chunked} imports the raw multi-channel accelerometer
#' data stored in mHealth Specification in chunks.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a File IO
#'   function that is used to import data stored in mHealth Specification during
#'   algorithm validation.
#'
#' @param filepath string. The filepath of the input data.
#' @return list. The list contains three items. The first item is a generator
#' function that each time it is called, it will
#' return a list of `list(before_df, df, after_df)`. These are data.frames of
#' imported chunks, ordered in time that can be loaded with
#' the `MIMSunit::mims_unit` function directly. The second item is a
#' `has_more_chunks` function used to check if all chunks are loaded.
#' If it returns `FALSE`, it means the loading has ended. The third item is a
#' `close` function which you can call at any moment to close the file loading.
#' @family Filo I/O functions
#'
#' @export
import_mhealth_csv_chunked <- function(filepath) {
  options(digits.secs = 3)
  chunk_size <- 1800 * 100
  col_types <- c("character", "numeric", "numeric", "numeric")
  col_names <- c("HEADER_TIME_STAMP", "X", "Y", "Z")

  con <- file(filepath, open = "r")
  g.env <- new.env()
  g.env$before_df <- NULL
  g.env$df <- NULL
  g.env$after_df <- NULL


  has_more_chunks <- function() {
    is_open <- tryCatch(
      {
        isOpen(con)
      },
      error =
        function(cond) {
          return(FALSE)
        }
    )
    if (is.null(g.env$after_df) || !is_open) {
      return(FALSE)
    }
    if (nrow(g.env$after_df) == chunk_size) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  next_chunk <- function() {
    g.env$before_df <- cbind(g.env$df)
    if (!is.null(g.env$after_df)) {
      g.env$df <- cbind(g.env$after_df)
    } else {
      df <- read.csv(
        file = con,
        header = TRUE,
        skip = 0,
        nrows = chunk_size,
        colClasses = col_types,
        col.names = col_names
      )
      df <- .convert_mhealth_timestamps(df)
      g.env$df <- cbind(as.data.frame(df))
    }

    after_df <- read.csv(
      file = con,
      header = TRUE,
      skip = 0,
      nrows = chunk_size,
      colClasses = col_types,
      col.names = col_names
    )
    after_df <- .convert_mhealth_timestamps(after_df)
    g.env$after_df <- cbind(as.data.frame(after_df))
    if (nrow(after_df) == 0) {
      g.env$after_df <- NULL
    }

    return(list(g.env$before_df, g.env$df, g.env$after_df))
  }

  close_connection <- function() {
    close(con)
  }
  return(list(next_chunk, has_more_chunks, close_connection))
}

.convert_mhealth_timestamps <- function(dat) {
  time_format <- "%Y-%m-%d %H:%M:%OS"
  dat[["HEADER_TIME_STAMP"]] <-
    strptime(x = dat[["HEADER_TIME_STAMP"]], format = time_format) + 5e-04
  return(dat)
}



#' Import raw multi-channel accelerometer data stored in ActivPal3 csv format
#'
#' \code{import_activpal3_csv} imports the raw multi-channel accelerometer data
#' stored in ActivPal3 csv format by converting the accelerometer values (in
#' digital voltage values) to \eqn{g} unit.
#'
#' ActivPal 3 sensors have known dynamic range to be \eqn{(-2g, +2g)}. And the
#' sensor stores values using 8-bit memory storage. So, the digital voltage
#' values may be converted to \eqn{g} unit using following equation.
#'
#' \deqn{x_g = \frac{x_{voltage} - 127}{2^8} * 4}
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a File IO
#'   function that is used to import data from ActivPal3 devices during
#'   algorithm validation.
#'
#' @param filepath string. The filepath of the input data.
#' @param header boolean. If TRUE, the input csv file will have column names in
#'   the first row.
#' @return dataframe. The imported multi-channel accelerometer signal, with the
#'   first column being the timestamps in POSXlct format, and the rest columns
#'   being accelerometer values in \eqn{g} unit.
#'
#' @family Filo I/O functions
#'
#' @export
import_activpal3_csv <- function(filepath, header = FALSE) {
  ncols <-
    readr::count_fields(filepath, readr::tokenizer_csv(), n_max = 1)
  col_types <- paste(rep("d", ncols), collapse = "")
  if (header) {
    dat <-
      readr::read_csv(
        filepath,
        col_names = TRUE,
        trim_ws = TRUE,
        col_types = col_types
      )
  } else {
    dat <-
      readr::read_csv(
        filepath,
        col_names = FALSE,
        trim_ws = TRUE,
        col_types = col_types
      )
  }
  dat <- dat[1:4]
  colnames(dat) <- c("HEADER_TIME_STAMP", "X", "Y", "Z")
  dat["HEADER_TIME_STAMP"] <-
    as.POSIXct(dat[["HEADER_TIME_STAMP"]] * 60 * 60 * 24, origin = "1899-12-30", tz = "GMT")
  dat["HEADER_TIME_STAMP"] <-
    lubridate::force_tz(dat["HEADER_TIME_STAMP"], tzone = Sys.timezone())
  dat[2:4] <- (dat[2:4] - 127) / 2^8 * 4
  options(digits.secs = 3)
  dat <- as.data.frame(dat)
  return(dat)
}

#' Import large raw multi-channel accelerometer data stored in Actigraph raw csv
#' format in chunks
#'
#' \code{import_actigraph_csv_chunked} imports the raw multi-channel accelerometer data
#' stored in Actigraph raw csv format. It supports files from the following
#' devices: GT3X, GT3X+, GT3X+BT, GT9X, and GT9X-IMU.
#'
#' For old device (GT3X) that stores accelerometer values as digital voltage.
#' The function will convert the values to \eqn{g} unit using the following
#' equation.
#'
#' \deqn{x_g = \frac{x_{voltage}r}{(2 ^ r) - \frac{v}{2}}}
#'
#' Where \eqn{v} is the max voltage corresponding to the max accelerometer value
#' that can be found in the meta section in the csv file; \eqn{r} is the
#' resolution level which is the number of bits used to store the voltage
#' values. \eqn{r} can also be found in the meta section in the csv file.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a File IO
#'   function that is used to import data from Actigraph devices during
#'   algorithm validation.
#'
#' @param filepath string. The filepath of the input data.
#' @param in_voltage set as TRUE only when the input Actigraph csv file is in
#'   analog quantized format and need to be converted into g value
#' @param has_ts set as TRUE only when timestamp is provided as the first column
#' @param header boolean. If TRUE, the input csv file will have column names in
#'   the first row.
#' @return list. The list contains three items. The first item is a generator
#' function that each time it is called, it will
#' return a list of `list(before_df, df, after_df)`. These are data.frames of
#' imported chunks, ordered in time that can be loaded with
#' the `MIMSunit::mims_unit` function directly. The second item is a
#' `has_more_chunks` function used to check if all chunks are loaded.
#' If it returns `FALSE`, it means the loading has ended. The third item is a
#' `close` function which you can call at any moment to close the file loading.
#'
#' @family Filo I/O functions
#' @export
import_actigraph_csv_chunked <- function(filepath,
                                         in_voltage = FALSE,
                                         has_ts = TRUE,
                                         header = TRUE) {
  options(digits.secs = 3)
  chunk_size <- 1800 * 100
  actigraph_meta <- import_actigraph_meta(filepath)
  if (has_ts) {
    ncols <- 4
    col_types <- c("character", "numeric", "numeric", "numeric")
    col_names <- c("HEADER_TIME_STAMP", "X", "Y", "Z")
  } else {
    ncols <- 3
    col_types <- c("numeric", "numeric", "numeric")
    col_names <- c("X", "Y", "Z")
  }
  if (header) {
    skip <- 11
  } else {
    skip <- 10
  }
  con <- file(filepath, "r")
  g.env <- new.env()
  g.env$before_dat <- NULL
  g.env$dat <- NULL
  g.env$after_dat <- NULL

  has_more_chunks <- function() {
    is_open <- tryCatch(
      {
        isOpen(con)
      },
      error =
        function(cond) {
          return(FALSE)
        }
    )
    if (is.null(g.env$after_dat) || !is_open) {
      return(FALSE)
    }
    if (nrow(g.env$after_dat) == chunk_size) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  next_chunk <- function() {
    g.env$before_dat <- cbind(g.env$dat)
    if (!is.null(g.env$after_dat)) {
      g.env$dat <- cbind(g.env$after_dat)
    } else {
      dat <- read.csv(con,
        header = FALSE,
        skip = skip,
        nrows = chunk_size,
        colClasses = col_types,
        col.names = col_names
      )
      if (!has_ts) {
        dat <- .append_actigraph_timestamps(dat, actigraph_meta)
      } else {
        dat <- .convert_actigraph_timestamps(dat, actigraph_meta)
      }
      colnames(dat) <- c("HEADER_TIME_STAMP", "X", "Y", "Z")

      if (in_voltage) {
        dat <- .convert_voltage_to_g(dat, actigraph_meta)
      }

      g.env$dat <- as.data.frame(dat)
    }
    after_dat <- read.csv(con,
      header = FALSE,
      skip = 0,
      nrows = chunk_size,
      colClasses = col_types,
      col.names = col_names
    )
    if (!has_ts) {
      after_dat <- .append_actigraph_timestamps(after_dat, actigraph_meta)
    } else {
      after_dat <- .convert_actigraph_timestamps(after_dat, actigraph_meta)
    }
    colnames(after_dat) <- c("HEADER_TIME_STAMP", "X", "Y", "Z")

    if (in_voltage) {
      after_dat <- .convert_voltage_to_g(after_dat, actigraph_meta)
    }

    g.env$after_dat <- as.data.frame(after_dat)
    if (nrow(after_dat) == 0) {
      g.env$after_dat <- NULL
    }
    return(list(g.env$before_dat, g.env$dat, g.env$after_dat))
  }

  close_connection <- function() {
    close(con)
  }
  return(list(next_chunk, has_more_chunks, close_connection))
}

.append_actigraph_timestamps <- function(dat, actigraph_meta) {
  ts_col <-
    seq(
      from = actigraph_meta$st,
      to = actigraph_meta$dt,
      length.out = nrow(dat)
    )
  dat <- cbind(ts_col, dat)
  return(dat)
}

.convert_actigraph_timestamps <- function(dat, actigraph_meta) {
  time_format <-
    ifelse(test = actigraph_meta$imu,
      yes = "%Y-%m-%dT%H:%M:%OS",
      no = "%m/%d/%Y %H:%M:%OS"
    )
  dat[["HEADER_TIME_STAMP"]] <-
    strptime(x = dat[["HEADER_TIME_STAMP"]], format = time_format) + 5e-04
  return(dat)
}

.convert_voltage_to_g <- function(dat, actigraph_meta) {
  vs <- actigraph_meta$vs
  res <- actigraph_meta$res

  dat[, 2:ncol(dat)] <-
    (dat[, 2:ncol(dat)] * vs / (2^res) - vs / 2) / (vs / res)
  dat[, 2:ncol(dat)] <-
    as.data.frame(apply(dat[, 2:ncol(dat)], 2, function(col) {
      col[col == -5] <- 0
      return(as.numeric(col))
    }))
  return(dat)
}

#' Import raw multi-channel accelerometer data stored in Actigraph raw csv
#' format
#'
#' \code{import_actigraph_csv} imports the raw multi-channel accelerometer data
#' stored in Actigraph raw csv format. It supports files from the following
#' devices: GT3X, GT3X+, GT3X+BT, GT9X, and GT9X-IMU.
#'
#' For old device (GT3X) that stores accelerometer values as digital voltage.
#' The function will convert the values to \eqn{g} unit using the following
#' equation.
#'
#' \deqn{x_g = \frac{x_{voltage}r}{(2 ^ r) - \frac{v}{2}}}
#'
#' Where \eqn{v} is the max voltage corresponding to the max accelerometer value
#' that can be found in the meta section in the csv file; \eqn{r} is the
#' resolution level which is the number of bits used to store the voltage
#' values. \eqn{r} can also be found in the meta section in the csv file.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a File IO
#'   function that is used to import data from Actigraph devices during
#'   algorithm validation.
#'
#' @param filepath string. The filepath of the input data.
#' @param in_voltage set as TRUE only when the input Actigraph csv file is in
#'   analog quantized format and need to be converted into g value
#' @param has_ts set as TRUE only when timestamp is provided as the first column
#' @param header boolean. If TRUE, the input csv file will have column names in
#'   the first row.
#' @return dataframe. The imported multi-channel accelerometer signal, with the
#'   first column being the timestamps in POSXlct format, and the rest columns
#'   being accelerometer values in \eqn{g} unit.
#'
#' @family Filo I/O functions
#' @export
import_actigraph_csv <-
  function(filepath,
           in_voltage = FALSE,
           has_ts = TRUE,
           header = TRUE) {
    actigraph_meta <- import_actigraph_meta(filepath)

    if (has_ts) {
      ncols <- 4
      col_types <- paste(c("c", rep("d", ncols - 1)), collapse = "")
    } else {
      ncols <- 3
      col_types <- paste(rep("d", ncols), collapse = "")
    }

    if (header) {
      dat <-
        readr::read_csv(
          filepath,
          col_names = FALSE,
          skip = 11,
          trim_ws = TRUE,
          col_types = col_types
        )
    } else {
      dat <-
        readr::read_csv(
          filepath,
          col_names = FALSE,
          skip = 10,
          trim_ws = TRUE,
          col_types = col_types
        )
    }

    if (!has_ts) {
      ts_col <-
        seq(
          from = actigraph_meta$st,
          to = actigraph_meta$dt,
          length.out = nrow(dat)
        )
      dat <- cbind(ts_col, dat)
    }

    dat <- dat[, 1:4]

    names(dat) <- c("HEADER_TIME_STAMP", "X", "Y", "Z")

    if (has_ts) {
      time_format <-
        ifelse(test = actigraph_meta$imu,
          yes = "%Y-%m-%dT%H:%M:%OS",
          no = "%m/%d/%Y %H:%M:%OS"
        )
      dat[["HEADER_TIME_STAMP"]] <-
        strptime(x = dat[["HEADER_TIME_STAMP"]], format = time_format) + 5e-04
    }

    options(digits.secs = 3)
    dat <- as.data.frame(dat)

    if (in_voltage) {
      vs <- actigraph_meta$vs
      res <- actigraph_meta$res

      dat[, 2:ncol(dat)] <-
        (dat[, 2:ncol(dat)] * vs / (2^res) - vs / 2) / (vs / res)
      dat[, 2:ncol(dat)] <-
        as.data.frame(apply(dat[, 2:ncol(dat)], 2, function(col) {
          col[col == -5] <- 0
          return(as.numeric(col))
        }))
    }
    return(dat)
  }

#' Import Actigraph count data stored in Actigraph summary csv format
#'
#' \code{import_actigraph_count_csv} imports Actigraph count data stored in
#' Actigraph summary csv format, which was exported by Actilife.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a File IO
#'   function that is used to import Actigraph count data from Actigraph devices
#'   during algorithm validation.
#'
#' @note If both \code{count_col} and \code{count_per_axis_cols} are
#'   \code{NULL}, the function will raise an error.
#'
#' @param filepath string. The filepath of the input data.
#' @param count_col number. The index of column of Actigraph count (combined
#'   axes). If it is \code{NULL}, the function will use
#'   \code{count_per_axis_cols} to get the combined Actigraph count values.
#' @param count_per_axis_cols numerical vector. The indices of columns of
#'   Actigraph count values per axis. If \code{count_col} is not \code{NULL},
#'   the argument will be ignored. If it is \code{NULL}, the output dataframe
#'   will only have two columns without Actigraph count values per axis.
#' @return dataframe. The imported actigraph count data, with the first column
#'   being the timestamps in POSIXct format, and the second column being the
#'   combined Actigraph count values, and the rest of columns being the
#'   Actigraph cont values per axis if available. Column names:
#'   \code{HEADER_TIME_STAMP}, \code{ACTIGRAPH_COUNT},
#'   \code{ACTIGRAPH_COUNT_X}....
#'
#' @family Filo I/O functions
#' @export
import_actigraph_count_csv <-
  function(filepath,
           count_col = 2,
           count_per_axis_cols = c(2, 3, 4)) {
    dat <-
      utils::read.csv(
        file = filepath,
        header = TRUE,
        stringsAsFactors = FALSE
      )
    dat[, 1] <-
      as.POSIXct(dat[, 1],
        format = "%Y-%m-%d %H:%M:%OS",
        tz = "UTC"
      )

    result <- dat[, 1]
    colnames(result) <- c("HEADER_TIME_STAMP")

    if (~ is.null(count_col)) {
      count_value <- dat[, count_col]
    } else if (is.null(count_col) & ~ is.null(count_per_axis_cols)) {
      count_value <- sqrt(rowSums(dat[, count_per_axis_cols]^2))
    }

    result["ACTIGRAPH_COUNT"] <- count_value
    axes_names <- c("X", "Y", "Z")
    if (~ is.null(count_per_axis_cols)) {
      for (i in count_per_axis_cols) {
        result[paste("ACTIGRAPH_COUNT_", axes_names[i], sep = "")] <- dat[, i]
      }
    }
    return(result)
  }

#' Import ENMO data stored in csv csv
#'
#' \code{import_enmo_csv} imports ENMO data stored in a summary csv format,
#' which was exported by the
#' \href{https://github.com/activityMonitoring/biobankAccelerometerAnalysis}{biobank
#' data analysis tools}.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a File IO
#'   function that is used to import ENMO data from activity monitor devices
#'   during algorithm validation.
#'
#' @param filepath string. The filepath of the input data.
#' @param enmo_col number. The index of column of ENMO values in the csv file.
#' @return dataframe. The imported ENMO data, with the first column being the
#'   timestamps in POSIXct format, and the second column being the ENMO values.
#'   Column names: \code{HEADER_TIME_STAMP}, \code{ENMO}.
#'
#' @family Filo I/O functions
#' @export
import_enmo_csv <- function(filepath, enmo_col = 2) {
  dat <- readr::read_csv(filepath, col_names = TRUE)
  dat <- data.frame(dat)
  dat <- dat[, c(1, enmo_col)]
  dat[, 1] <-
    as.POSIXct(dat[, 1],
      format = "%Y-%m-%d %H:%M:%OS",
      tz = Sys.timezone()
    )
  result <- dat
  colnames(result) <- c("HEADER_TIME_STAMP", "ENMO")
  return(result)
}

#' Import The meta information stored in Actigraph RAW or summary csv file.
#'
#' \code{import_actigraph_meta} imports meta information stored in the Actigraph
#' summary csv file.
#'
#' The returned meta information includes following fields.
#'
#' \itemize{ \item sr: Sampling rate in Hz. \item fw: Firmware version. For
#' example "1.7.0". \item sw: Software version of Actilife. For example "6.13.0".
#' \item sn: Serial number of the device. \item st: Start time of the data, in
#' POSIXct format. \item dt: Download time of the data, in POSIXct format. \item
#' at: Type of the device. Could be "MAT","CLE", "MOS" or "TAS", corresponding
#' to different Actigraph devices. \item imu: Whether the file is about
#' Actigraph GT9X IMU data. \item gr: The dynamic range in \eqn{g} unit. \item
#' vs: The voltage level of the device, may be used in AD conversion. See
#' \code{\link{import_actigraph_csv}}. \item res: The resolution or the number of
#' bits used to store quantized voltage values of the device, may be used in AD
#' conversion. See \code{\link{import_actigraph_csv}}. }
#'
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a File IO
#'   function that is used to get related meta information such as sampling
#'   rate, firmware version from Actigraph devices.
#'
#' @param filepath string. The filepath of the input data.
#' @param header logical. Whether the Actigraph RAW or summary csv file includes
#'   column names. Default is TRUE.
#' @return list. A list of Actigraph device meta information.
#' @family Filo I/O functions
#' @export
import_actigraph_meta <- function(filepath, header = TRUE) {
  ACTIGRAPH_HEADER_SR_PATTERN <- "([0-9]+) Hz"
  ACTIGRAPH_FIRMWARE_PATTERN <- "Firmware v([0-9]+.[0-9]+.[0-9]+)"
  ACTIGRAPH_SOFTWARE_PATTERN <- "ActiLife v([0-9]+.[0-9]+.[0-9]+)"
  ACTIGRAPH_SERIALNUM_PATTERN <- "Serial Number: ([A-Za-z0-9]+)"
  ACTIGRAPH_TIMESTAMP <- "%m/%d/%Y %H:%M:%OS"

  headlines <- readLines(filepath, n = 10, encoding = "UTF-8")

  # Sampling rate
  sr_pattern <- ACTIGRAPH_HEADER_SR_PATTERN
  sr <- headlines[[1]]
  sr <- stringr::str_match(sr, sr_pattern)
  sr <- as.numeric(sr[2])

  # Firmware code
  fw_pattern <- ACTIGRAPH_FIRMWARE_PATTERN
  fw <- headlines[[1]]
  fw <- stringr::str_match(fw, fw_pattern)
  fw <- fw[2]

  # Software code
  sw_pattern <- ACTIGRAPH_SOFTWARE_PATTERN
  sw <- headlines[[1]]
  sw <- stringr::str_match(sw, sw_pattern)
  sw <- sw[2]

  # Serial number
  sn_pattern <- ACTIGRAPH_SERIALNUM_PATTERN
  sn <- headlines[[2]]
  sn <- stringr::str_match(sn, sn_pattern)
  sn <- sn[2]

  # actigraph type
  at <- substr(sn, 1, 3)

  # g range
  gr <- switch(
    at,
    MAT = "3",
    CLE = "6",
    MOS = "8",
    TAS = "8"
  )

  # IMU or not
  if (stringr::str_detect(headlines[[1]], "IMU")) {
    imu <- TRUE
  } else {
    imu <- FALSE
  }

  if (imu) {
    gr <- "16"
  }

  # Session start time
  st <- headlines[[3]]
  sd <- headlines[[4]]
  time_reg <- "[0-9]{2}(:[0-9]{2}){1,2}+"
  date_reg <- "[0-9]+/[0-9]+/[0-9]{4}"
  st <- regmatches(st, regexpr(time_reg, st, perl = TRUE))
  sd <- regmatches(sd, regexpr(date_reg, sd, perl = TRUE))
  st <- paste(sd, st, sep = " ")
  time_format <- ACTIGRAPH_TIMESTAMP
  st <- strptime(st, time_format) + 5e-04
  options(digits.secs = 3)

  # Session download time
  dt <- headlines[[6]]
  dd <- headlines[[7]]
  time_reg <- "[0-9]{2}(:[0-9]{2}){1,2}+"
  date_reg <- "[0-9]+/[0-9]+/[0-9]{4}"
  dt <- regmatches(dt, regexpr(time_reg, dt, perl = TRUE))
  dd <- regmatches(dd, regexpr(date_reg, dd, perl = TRUE))
  dt <- paste(dd, dt, sep = " ")
  time_format <- ACTIGRAPH_TIMESTAMP
  dt <- strptime(dt, time_format) + 5e-04
  options(digits.secs = 3)
  if (is.na(sr)) {
    # determine sr by start and download time options(digits = 13)
    duration <- as.numeric(dt - st, units = "secs")
    if (header) {
      nlines <- R.utils::countLines(filepath) - 11
    } else {
      nlines <- R.utils::countLines(filepath) - 10
    }
    sr <- as.numeric(ceiling(nlines / duration))
  }

  # input voltage
  vs <- headlines[[9]]
  vs_reg <- ": ([0-9](\\.[0-9]+)*)"
  vs <- as.numeric(stringr::str_match(vs, vs_reg)[2])

  # input resolution
  resolution <- headlines[[9]]
  res_reg <- "= ([0-9]+)"
  resolution <-
    as.numeric(stringr::str_match(resolution, res_reg)[2])

  # header object as output
  meta <- {

  }
  meta$sr <- sr
  meta$fw <- fw
  meta$sw <- sw
  meta$sn <- sn
  meta$st <- st
  meta$dt <- dt
  meta$at <- at
  meta$imu <- imu
  meta$gr <- gr
  meta$vs <- vs
  meta$res <- resolution

  return(meta)
}
