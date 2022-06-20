#' Import raw multi-channel accelerometer data stored in mHealth Specification
#'
#' \code{import_mhealth_csv} imports the raw multi-channel accelerometer data
#'   stored in mHealth Specification. Note that this function will fail when
#'   loading data that have size too big to fit in the memory. For large data
#'   file, please use \code{\link{import_mhealth_csv_chunked}} to load.
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
#' @family File I/O functions
#'
#' @export
#' @examples
#'   default_ops = options()
#'   options(digits.secs=3)
#'   # Use the sample mhealth csv file provided by the package
#'   filepath = system.file('extdata', 'mhealth.csv', package='MIMSunit')
#'   filepath
#'
#'   # Load the file
#'   df = import_mhealth_csv(filepath)
#'
#'   # Check loaded file
#'   head(df)
#'
#'   # Check more
#'   summary(df)
#'
#'   # Restore default options
#'   options(default_ops)
import_mhealth_csv <- function(filepath) {
  ncols <-
    readr::count_fields(filepath,
                        readr::tokenizer_csv(),
                        skip = 0,
                        n_max = 1L)
  date_format <- readr::col_datetime(format = "%Y-%m-%d %H:%M:%OS")
  coltypes <- list(date_format)
  if (ncols == 4) {
    colheaders <- c("HEADER_TIME_STAMP", "X", "Y", "Z")
  } else if (ncols == 2) {
    colheaders <- c("HEADER_TIME_STAMP", "VALUE")
  } else {
    stop('The file does not contain the correct columns for mhealth raw accelerometer data.')
  }
  for (i in 2:ncols) {
    coltypes <- append(coltypes, list(readr::col_double()))
  }

  df <- readr::read_csv(file = filepath,
                        quoted_na = TRUE,
                        col_types = coltypes)
  # convert factors back to characters
  col_classes <- sapply(1:ncols, function(i) {
    return(class(df[1, i]))
  })
  factor_cols <- which(col_classes == "factor")
  if (length(factor_cols) > 0) {
    df[, factor_cols] <- as.character(df[, factor_cols])
  }

  # enhance column headers
  colnames(df)[1:length(colheaders)] <- colheaders
  colnames(df) <- toupper(colnames(df))
  df <- data.frame(df, stringsAsFactors = FALSE)

  return(df)
}

#' Import large raw multi-channel accelerometer data stored in mHealth Specification
#' in chunks.
#'
#' \code{import_mhealth_csv_chunked} imports the raw multi-channel accelerometer
#'   data stored in mHealth Specification in chunks.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a File IO
#'   function that is used to import data stored in mHealth Specification during
#'   algorithm validation.
#'
#' @param filepath string. The filepath of the input data.
#' @param chunk_samples number. The number of samples in each chunk. Default is
#'   180000, which is half hour data for 100 Hz sampling rate.
#' @return list. The list contains two items. The first item is a generator
#'   function that each time it is called, it will
#'   return a dataframe with at most \code{chunk_samples} samples of imported data.
#'   The third item is a \code{close_connection} function which you can call at
#'   any moment to close the file loading.
#' @family File I/O functions
#'
#' @export
#' @examples
#'   default_ops = options()
#'   options(digits.secs=3)
#'
#'   # Use the mhealth csv file shipped with the package
#'   filepath = system.file('extdata', 'mhealth.csv', package='MIMSunit')
#'
#'   # Example 1
#'   # Load chunks every 1000 samples
#'   results = import_mhealth_csv_chunked(filepath, chunk_samples=100)
#'   next_chunk = results[[1]]
#'   close_connection = results[[2]]
#'   # Check data as chunks, you can see chunk time is shifting forward at each iteration.
#'   n = 1
#'   repeat {
#'     df = next_chunk()
#'     if (nrow(df) > 0) {
#'       print(paste('chunk', n))
#'       print(paste("df:", df[1, 1], '-', df[nrow(df),1]))
#'       n = n + 1
#'     } else {
#'       break
#'     }
#'   }
#'
#'   # Close connection after reading all the data
#'   close_connection()
#'
#'   # Example 2: close loading early
#'   results = import_mhealth_csv_chunked(filepath, chunk_samples=1000)
#'   next_chunk = results[[1]]
#'   close_connection = results[[2]]
#'   # Check data as chunks, you can see chunk time is shifting forward at each iteration.
#'   n = 1
#'   repeat {
#'     df = next_chunk()
#'     if (nrow(df) > 0) {
#'       print(paste('chunk', n))
#'       print(paste("df:", df[1, 1], '-', df[nrow(df),1]))
#'       n = n + 1
#'       close_connection()
#'     }
#'     else {
#'       break
#'     }
#'   }
#'
#'  # Restore default options
#'  options(default_ops)
import_mhealth_csv_chunked <-
  function(filepath, chunk_samples = 180000) {
    chunk_size <- chunk_samples
    col_types <- c("character", "numeric", "numeric", "numeric")
    col_names <- c("HEADER_TIME_STAMP", "X", "Y", "Z")

    con <- file(filepath, open = "r")

    df = NULL

    next_chunk <- function() {
      is_open <- tryCatch({
        isOpen(con)
      },
      error =
        function(cond) {
          return(FALSE)
        })
      if (!is_open) {
        return(data.frame())
      }
      df <- utils::read.csv(
        file = con,
        header = TRUE,
        skip = 0,
        nrows = chunk_size,
        colClasses = col_types,
        col.names = col_names
      )
      df <- .convert_mhealth_timestamps(df)
      df <- cbind(as.data.frame(df))
      return(df)
    }

    close_connection <- function() {
      close(con)
    }
    return(list(next_chunk, close_connection))
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
#' @family File I/O functions
#'
#' @export
#' @examples
#'   default_ops = options()
#'   options(digits.secs=3)
#'   # Use the sample activpal3 csv file provided by the package
#'   filepath = system.file('extdata', 'activpal3.csv', package='MIMSunit')
#'
#'   # Check the csv format
#'   readLines(filepath)[1:5]
#'
#'   # Load the file, in our case without header
#'   df = import_activpal3_csv(filepath, header=FALSE)
#'
#'   # Check loaded file
#'   head(df)
#'
#'   # Check more
#'   summary(df)
#'
#'   # Restore default options
#'   options(default_ops)
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
  dat[2:4] <- (dat[2:4] - 127) / 2 ^ 8 * 4
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
#' @param filepath string. The filepath of the input data.The first column of
#' the input data should always include timestamps.
#' @param in_voltage set as TRUE only when the input Actigraph csv file is in
#'   analog quantized format and need to be converted into g value
#' @param header boolean. If TRUE, the input csv file will have column names in
#'   the first row.
#' @param has_ts boolean. If TRUE, the input csv file should have a timestamp
#'   column at first.
#' @param chunk_samples number. The number of samples in each chunk. Default is
#'   180000.
#' @return list. The list contains two items. The first item is a generator
#'   function that each time it is called, it will return a data.frame of the
#'   imported chunk. The second item is a \code{close} function which you can
#'   call at any moment to close the file loading.
#'
#' @family File I/O functions
#' @export
#' @examples
#'   default_ops = options()
#'   options(digits.secs=3)
#'
#'   # Use the actigraph csv file shipped with the package
#'   filepath = system.file('extdata', 'actigraph_timestamped.csv', package='MIMSunit')
#'
#'   # Check original file format
#'   readLines(filepath)[1:15]
#'
#'   # Example 1: Load chunks every 2000 samples
#'   results = import_actigraph_csv_chunked(filepath, chunk_samples=2000)
#'   next_chunk = results[[1]]
#'   close_connection = results[[2]]
#'   # Check data as chunks, you can see chunks are shifted at each iteration.
#'   n = 1
#'   repeat {
#'     df = next_chunk()
#'     if (nrow(df) > 0) {
#'       print(paste('chunk', n))
#'       print(paste("df:", df[1, 1], '-', df[nrow(df),1]))
#'       n = n + 1
#'     }
#'     else {
#'       break
#'     }
#'   }
#'
#'   # Close connection after reading all the data
#'   close_connection()
#'
#'   # Example 2: Close loading early
#'   results = import_actigraph_csv_chunked(filepath, chunk_samples=2000)
#'   next_chunk = results[[1]]
#'   close_connection = results[[2]]
#'   # Check data as chunks, you can see chunk time is shifting forward at each iteration.
#'   n = 1
#'   repeat {
#'     df = next_chunk()
#'     if (nrow(df) > 0) {
#'       print(paste('chunk', n))
#'       print(paste("df:", df[1, 1], '-', df[nrow(df),1]))
#'       n = n + 1
#'       close_connection()
#'     }
#'     else {
#'       break
#'     }
#'   }
#'
#'   # Restore default options
#'   options(default_ops)
import_actigraph_csv_chunked <- function(filepath,
                                         in_voltage = FALSE,
                                         header = TRUE,
                                         has_ts = TRUE,
                                         chunk_samples = 180000) {
  chunk_size <- chunk_samples
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

  env <- new.env()
  env$df = NULL

  next_chunk <- function() {
    is_open <- tryCatch({
      isOpen(con)
    },
    error =
      function(cond) {
        return(FALSE)
      })
    if (!is_open) {
      return(data.frame())
    }
    dat <- utils::read.csv(
      con,
      header = FALSE,
      skip = skip,
      nrows = chunk_size,
      colClasses = col_types,
      col.names = col_names
    )
    if (!has_ts) {
      if (!is.null(env$df)) {
        st = env$df[nrow(env$df), 1]
      } else {
        st = actigraph_meta$st
      }
      dat <-
        .append_actigraph_timestamps(dat, actigraph_meta, st = st)
    } else {
      dat <- .convert_actigraph_timestamps(dat, actigraph_meta)
    }
    colnames(dat) <- c("HEADER_TIME_STAMP", "X", "Y", "Z")

    if (in_voltage) {
      dat <- .convert_voltage_to_g(dat, actigraph_meta)
    }

    env$df <- as.data.frame(dat)
    return(env$df)
  }

  close_connection <- function() {
    close(con)
  }
  return(list(next_chunk, close_connection))
}

.append_actigraph_timestamps <- function(dat, actigraph_meta, st = NULL) {
  if (is.null(st)) {
    st = actigraph_meta$st
  } else {
    st = st + 1 /  actigraph_meta$sr
  }
  ts_col <-
    seq(
      from = st,
      by = 1 / actigraph_meta$sr,
      length.out = nrow(dat)
    )
  dat <- cbind(ts_col, dat)
  return(dat)
}

.convert_actigraph_timestamps <- function(dat, actigraph_meta) {
  time_format <-
    ifelse(test = actigraph_meta$imu,
           yes = "%Y-%m-%dT%H:%M:%OS",
           no = "%m/%d/%Y %H:%M:%OS")
  dat[["HEADER_TIME_STAMP"]] <-
    strptime(x = dat[["HEADER_TIME_STAMP"]], format = time_format) + 5e-04
  return(dat)
}

.convert_voltage_to_g <- function(dat, actigraph_meta) {
  vs <- actigraph_meta$vs
  res <- actigraph_meta$res

  dat[, 2:ncol(dat)] <-
    (dat[, 2:ncol(dat)] * vs / (2 ^ res) - vs / 2) / (vs / res)
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
#' @param filepath string. The filepath of the input data. The first column of
#' the input data should always include timestamps.
#' @param in_voltage set as TRUE only when the input Actigraph csv file is in
#'   analog quantized format and need to be converted into g value
#' @param header boolean. If TRUE, the input csv file will have column names in
#'   the first row.
#' @param has_ts boolean. If TRUE, the input csv file will have a timestamp column.
#' @return dataframe. The imported multi-channel accelerometer signal, with the
#'   first column being the timestamps in POSXlct format, and the rest columns
#'   being accelerometer values in \eqn{g} unit.
#'
#' @family File I/O functions
#' @export
#' @examples
#'   default_ops = options()
#'   options(digits.secs=3)
#'
#'   # Use the sample actigraph csv file provided by the package
#'   filepath = system.file('extdata', 'actigraph_timestamped.csv', package='MIMSunit')
#'
#'   # Check file format
#'   readLines(filepath)[1:15]
#'
#'   # Load the file with timestamp column
#'   df = import_actigraph_csv(filepath)
#'
#'   # Check loaded file
#'   head(df)
#'
#'   # Check more
#'   summary(df)
#'
#'   # Use the sample actigraph csv file without timestamp
#'   filepath = system.file('extdata', 'actigraph_no_timestamp.csv', package='MIMSunit')
#'
#'   # Check file format
#'   readLines(filepath)[1:15]
#'
#'   # Load the file without timestamp column
#'   df = import_actigraph_csv(filepath, has_ts = FALSE)
#'
#'   # Check loaded file
#'   head(df)
#'
#'   # Check more
#'   summary(df)
#'
#'   # Restore default options
#'   options(default_ops)
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

    if (has_ts && ncol(dat) == 3) {
      stop(
        "The input data only has 3 column
s, there should be 4 columns with the first column being the timestamps."
      )
    }

    if (!has_ts) {
      dat = .append_actigraph_timestamps(dat = dat,
                                         actigraph_meta = actigraph_meta)
    }

    dat <- dat[, 1:4]

    names(dat) <- c("HEADER_TIME_STAMP", "X", "Y", "Z")

    if (has_ts) {
      time_format <-
        ifelse(test = actigraph_meta$imu,
               yes = "%Y-%m-%dT%H:%M:%OS",
               no = "%m/%d/%Y %H:%M:%OS")
      dat[["HEADER_TIME_STAMP"]] <-
        strptime(x = dat[["HEADER_TIME_STAMP"]], format = time_format) + 5e-04
    }

    dat <- as.data.frame(dat)

    if (in_voltage) {
      vs <- actigraph_meta$vs
      res <- actigraph_meta$res

      dat[, 2:ncol(dat)] <-
        (dat[, 2:ncol(dat)] * vs / (2 ^ res) - vs / 2) / (vs / res)
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
#' @family File I/O functions
#' @export
#' @examples
#'   # Use the actigraph count csv file shipped with the package
#'   filepath = system.file('extdata', 'actigraph_count.csv', package='MIMSunit')
#'
#'   # Check original data format
#'   readLines(filepath)[1:5]
#'
#'   # Load file, default column for actigraph count values are 2, this file does not have
#'   # axial count values
#'   output = import_actigraph_count_csv(filepath, count_col=2)
#'
#'   # Check output
#'   head(output)
import_actigraph_count_csv <-
  function(filepath,
           count_col = 2,
           count_per_axis_cols = c(2, 3, 4)) {
    dat <-
      utils::read.csv(file = filepath,
                      header = TRUE,
                      stringsAsFactors = FALSE)
    dat[, 1] <-
      as.POSIXct(dat[, 1],
                 format = "%Y-%m-%d %H:%M:%OS",
                 tz = "UTC")

    if (!is.null(count_col)) {
      result <- dat[, c(1, count_col)]
      colnames(result) = c("HEADER_TIME_STAMP", "ACTIGRAPH_COUNT")
    } else if (is.null(count_col) & !is.null(count_per_axis_cols)) {
      result <-
        cbind(dat[, 1], sqrt(rowSums(dat[, count_per_axis_cols] ^ 2)), dat[, count_per_axis_cols])
      colnames(result) = c(
        "HEADER_TIME_STAMP",
        "ACTIGRAPH_COUNT",
        "ACTIGRAPH_COUNT_X",
        "ACTIGRAPH_COUNT_Y",
        "ACTIGRAPH_COUNT_Z"
      )
    } else {
      stop("You must set count_col or count_per_axis_cols")
    }
    return(result)
  }

#' Import ENMO data stored in csv csv
#'
#' \code{import_enmo_csv} imports ENMO data stored in a summary csv format,
#' which was exported by the
#' \href{https://github.com/OxWearables/biobankAccelerometerAnalysis}{biobank
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
#' @family File I/O functions
#' @export
#' @examples
#'   # Use the enmo csv file shipped with the package
#'   filepath = system.file('extdata', 'enmo.csv', package='MIMSunit')
#'
#'   # Check original data format
#'   readLines(filepath)[1:5]
#'
#'   # Load file, default column for enmo values are 2
#'   output = import_enmo_csv(filepath, enmo_col=2)
#'
#'   # Check output
#'   head(output)
import_enmo_csv <- function(filepath, enmo_col = 2) {
  dat <- readr::read_csv(filepath, col_names = TRUE)
  dat <- data.frame(dat)
  dat <- dat[, c(1, enmo_col)]
  dat[, 1] <-
    as.POSIXct(dat[, 1],
               format = "%Y-%m-%d %H:%M:%OS",
               tz = Sys.timezone())
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
#' @family File I/O functions
#' @export
#' @examples
#'   default_ops = options()
#'   options(digits.secs=3)
#'
#'   # Use the sample actigraph csv file provided by the package
#'   filepath = system.file('extdata', 'actigraph_timestamped.csv', package='MIMSunit')
#'
#'   # Check file format
#'   readLines(filepath)[1:15]
#'
#'   # Load the meta headers of input file
#'   import_actigraph_meta(filepath, header=TRUE)
#'
#'   # Restore default options
#'   options(default_ops)
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
