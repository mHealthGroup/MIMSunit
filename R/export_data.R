#' @name export_actigraph_raw
#' @title Export to Actigraph raw csv files
#' @importFrom readr write_lines write_csv
#' @export
export_actigraph_raw <-
  function(df,
           filepath,
           actilife_version = "6.13.3",
           firmware_version = "1.6.0")
  {
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
      c(actigraph_meta,
        paste0("Current Battery Voltage: 4.19     Mode = 12"))
    actigraph_meta <-
      c(actigraph_meta,
        paste0("--------------------------------------------------"))
    actigraph_meta <-
      c(
        actigraph_meta,
        paste0("Timestamp,Accelerometer X,Accelerometer Y,Accelerometer Z")
      )
    df[[1]] <-
      strftime(df[[1]], format = "%m/%d/%Y %H:%M:%OS3", tz = "UTC")
    df[c(2, 3, 4)] <- round(df[c(2, 3, 4)], digits = 3)
    readr::write_lines(x = actigraph_meta, path = filepath, append = FALSE)
    readr::write_csv(
      x = df,
      path = filepath,
      col_names = FALSE,
      append = TRUE
    )
  }
