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
import_activpal3_csv <- function(filepath, header = FALSE)
{
  ncols <-
    readr::count_fields(filepath, readr::tokenizer_csv(), n_max = 1)
  col_types <- paste(rep("d", ncols), collapse = "")
  if (header)
  {
    dat <-
      readr::read_csv(
        filepath,
        col_names = TRUE,
        trim_ws = TRUE,
        col_types = col_types
      )
  } else
  {
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
  dat[2:4] <- (dat[2:4] - 127) / 2 ^ 8 * 4
  options(digits.secs = 3)
  dat <- as.data.frame(dat)
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
#' \deqn{\frac{x_{voltage}r}{(2 ^ r) - \frac{v}{2}}}
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
           header = TRUE)
  {
    actigraph_meta <- import_actigraph_meta(filepath)

    ncols <-
      readr::count_fields(filepath,
                          readr::tokenizer_csv(),
                          n_max = 1,
                          skip = 11)
    if (has_ts)
    {
      col_types <- paste(c("c", rep("d", ncols - 1)), collapse = "")
    } else
    {
      col_types <- paste(rep("d", ncols), collapse = "")
    }


    if (header)
    {
      dat <-
        readr::read_csv(
          filepath,
          col_names = FALSE,
          skip = 11,
          trim_ws = TRUE,
          col_types = col_types
        )
    } else
    {
      dat <-
        readr::read_csv(
          filepath,
          col_names = FALSE,
          skip = 10,
          trim_ws = TRUE,
          col_types = col_types
        )
    }

    if (!has_ts)
    {
      ts_col <-
        seq(from = actigraph_meta$st,
            to = actigraph_meta$dt,
            length.out = nrow(dat))
      dat <- cbind(ts_col, dat)
    }

    dat <- dat[, 1:4]

    names(dat) <- c("HEADER_TIME_STAMP", "X", "Y", "Z")

    if (has_ts)
    {
      time_format <-
        ifelse(test = actigraph_meta$imu,
               yes = "%Y-%m-%dT%H:%M:%OS",
               no = "%m/%d/%Y %H:%M:%OS")
      dat[["HEADER_TIME_STAMP"]] <-
        strptime(x = dat[["HEADER_TIME_STAMP"]], format = time_format) + 5e-04
    }

    options(digits.secs = 3)
    dat <- as.data.frame(dat)

    if (in_voltage)
    {
      vs <- actigraph_meta$vs
      res <- actigraph_meta$res

      dat[, 2:ncol(dat)] <-
        (dat[, 2:ncol(dat)] * vs / (2 ^ res) - vs / 2) / (vs / res)
      dat[, 2:ncol(dat)] <-
        as.data.frame(apply(dat[, 2:ncol(dat)], 2, function(col)
        {
          col[col == -5] <- 0
          return(as.numeric(col))
        }))
    }
    return(dat)
  }

#' @name import_actigraph_count
#' @title Import and convert Actigraph count csv files and load into data frame as in mhealth format.
#' @export
#' @rdname import_actigraph_count
#' @param filepath full file path of input Actigraph count csv file.
import_actigraph_count <-
  function(filepath,
           col_name = "ACTIGRAPH_COUNT",
           axes = c(2, 3, 4))
  {
    dat <-
      readr::read_csv(
        filepath,
        col_names = TRUE,
        col_types = readr::cols(
          timestamp = readr::col_character(),
          axis1 = readr::col_double(),
          axis2 = readr::col_double(),
          axis3 = readr::col_double()
        )
      )
    dat <- data.frame(dat)
    dat[, 1] <-
      as.POSIXct(dat[, 1],
                 format = mHealthR::mhealth$format$csv$TIMESTAMP,
                 tz = "UTC")
    if (length(axes) > 1)
    {
      count_value <- sqrt(rowSums(dat[, axes] ^ 2))
    } else
    {
      count_value <- dat[, axes]
    }
    dat[col_name] <- count_value
    result <- dat[c(1, ncol(dat))]
    colnames(result) <- c("HEADER_TIME_STAMP", col_name)
    return(result)
  }

#' @name import_actigraph_count_vm
#' @title Import and convert Actigraph count csv files and load into data frame as in mhealth format.
#' @export
#' @rdname import_actigraph_count
#' @param filepath full file path of input Actigraph count csv file.
import_actigraph_count_vm <-
  function(filepath, col_name = "ACTIGRAPH_COUNT")
  {
    dat <-
      readr::read_csv(
        filepath,
        col_names = TRUE,
        col_types = readr::cols(
          timestamp = readr::col_character(),
          vectormagnitude = readr::col_double()
        )
      )
    dat <- data.frame(dat)
    dat[, 1] <-
      as.POSIXct(dat[, 1],
                 format = mHealthR::mhealth$format$csv$TIMESTAMP,
                 tz = Sys.timezone())
    result <- dat
    colnames(result) <- c("HEADER_TIME_STAMP", col_name)
    return(result)
  }

#' @name import_biobank_enmo
#' @title Import and convert biobank epoch csv files and load into data frame as in mhealth format.
#' @export
#' @param filepath full file path of input biobank epoch csv file.
import_biobank_enmo <- function(filepath, col_name = "biobank_enmo")
{
  dat <- readr::read_csv(filepath, col_names = TRUE)
  dat <- data.frame(dat)
  dat <- dat[1:2]
  dat[, 1] <-
    as.POSIXct(dat[, 1],
               format = mHealthR::mhealth$format$csv$TIMESTAMP,
               tz = Sys.timezone())
  result <- dat
  colnames(result) <- c("HEADER_TIME_STAMP", col_name)
  return(result)
}

#' @name import_actigraph_meta
#' @title parse actigraph csv header to get related version and sampling rate information
#' @export
import_actigraph_meta <- function(filepath, header = TRUE)
{
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
  if (stringr::str_detect(headlines[[1]], "IMU"))
  {
    imu <- TRUE
  } else
  {
    imu <- FALSE
  }

  if (imu)
  {
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
  if (is.na(sr))
  {
    # determine sr by start and download time options(digits = 13)
    duration <- as.numeric(dt - st, units = "secs")
    if (header)
    {
      nlines <- R.utils::countLines(filepath) - 11
    } else
    {
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
  header <- {

  }
  header$sr <- sr
  header$fw <- fw
  header$sw <- sw
  header$sn <- sn
  header$st <- st
  header$dt <- dt
  header$at <- at
  header$imu <- imu
  header$gr <- gr
  header$vs <- vs
  header$res <- resolution

  return(header)
}
