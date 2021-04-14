#' Export accelerometer data in Actilife RAW CSV format
#'
#' \code{export_to_actilife} exports the input dataframe as a csv file that is
#' compatible with Actilife.
#'
#' This function takes an input accelerometer dataframe and exports it in
#' Actilife RAW CSV format with a prepended a madeup header. The exported file
#' csv file has compatible header, column names, timestamp format with Actilife
#' and can be imported directly into Actilife software.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is an utility
#' function that was used to convert validation data into Actilife RAW CSV
#' format so that we can use Actilife to compute Actigraph counts values for
#' these data.
#'
#' @param df dataframe. Input accelerometer data. The first column is timestamp
#'   in POSXlct format, and the rest columns are accelerometer values in g
#'   (\eqn{9.81m/s^2}).
#' @param filepath string. The output filepath.
#' @param actilife_version string. The Actilife version number to be added to
#'   the header. Default is "6.13.3", that was used by the algorithm during
#'   development.
#' @param firmware_version string. The firmware version number to be added to
#'   the header. This is supposed to be the firmware version of the Actigraph
#'   devices. We did not see any usage of the number during the computation of
#'   Actigraph counts by Actilife, so it may be set with an arbitrary version
#'   code seen in any Actigraph devices. We use default version code "1.6.0".
#' @return No return value.
#' @family File I/O functions
#' @export
#' @examples
#'   # Use the first 5 rows from sample data
#'   df = sample_raw_accel_data[1:5,]
#'   head(df)
#'
#'   # Save to current path with default mocked actilife and firmware versions
#'   filepath = tempfile()
#'   export_to_actilife(df, filepath)
#'
#'   # The saved file will have the same format as Actigraph csv files
#'   readLines(filepath)
#'
#'   # Cleanup
#'   file.remove(filepath)
export_to_actilife <-
  function(df,
           filepath,
           actilife_version = "6.13.3",
           firmware_version = "1.6.0") {
    sr <- sampling_rate(df)
    start_time <-
      strftime(df[1, 1], format = "%H:%M:%S", tz = "UTC")
    start_date <-
      strftime(df[1, 1], format = "%m/%d/%Y", tz = "UTC")
    stop_time <-
      strftime(df[nrow(df), 1], format = "%H:%M:%S", tz = "UTC")
    stop_date <-
      strftime(df[nrow(df), 1], format = "%m/%d/%Y", tz = "UTC")
    actigraph_meta <-
      c(
        paste0(
          "------------ Data File Created By Actigraph GT3X+ ActiLife v",
          actilife_version,
          " Firmware v",
          firmware_version,
          " date format M/d/yyyy at ",
          sr,
          " Hz  Filter Nomral -----------"
        )
      )
    actigraph_meta <-
      c(actigraph_meta, "Serial Number: TAS1E00000000")
    actigraph_meta <-
      c(actigraph_meta, paste0("Start Time ", start_time))
    actigraph_meta <-
      c(actigraph_meta, paste0("Start Date ", start_date))
    actigraph_meta <-
      c(actigraph_meta, paste0("Epoch Period (hh:mm:ss) 00:00:00"))
    actigraph_meta <-
      c(actigraph_meta, paste0("Download Time ", stop_time))
    actigraph_meta <-
      c(actigraph_meta, paste0("Download Date ", stop_date))
    actigraph_meta <-
      c(actigraph_meta, paste0("Current Memory Address 0"))
    actigraph_meta <-
      c(
        actigraph_meta,
        paste0("Current Battery Voltage: 4.19     Mode = 12")
      )
    actigraph_meta <-
      c(
        actigraph_meta,
        paste0("--------------------------------------------------")
      )
    actigraph_meta <-
      c(
        actigraph_meta,
        paste0("Timestamp,Accelerometer X,Accelerometer Y,Accelerometer Z")
      )
    df[[1]] <-
      strftime(df[[1]], format = "%m/%d/%Y %H:%M:%OS3", tz = "UTC")
    df[c(2, 3, 4)] <- round(df[c(2, 3, 4)], digits = 3)
    readr::write_lines(x = actigraph_meta, file = filepath, append = FALSE)
    readr::write_csv(
      x = df,
      file = filepath,
      col_names = FALSE,
      append = TRUE
    )
  }
