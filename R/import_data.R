
#' @name import_activpal
#' @title Import ActivPal Raw data files and load into dataframe as mhealth format.
#' @importFrom readr read_csv count_fields tokenizer_csv
#' @importFrom mHealthR mhealth
#' @importFrom lubridate force_tz
#' @export
import_activpal_raw = function(filename, header_provided = FALSE){
  ncols = readr::count_fields(filename, readr::tokenizer_csv(), n_max = 1)
  colTypes = paste(rep("d", ncols), collapse = "")
  if(header_provided){
    dat = readr::read_csv(
      filename, col_names = TRUE, trim_ws = TRUE, col_types = colTypes);
  }else{
    dat = readr::read_csv(
      filename, col_names = FALSE, trim_ws = TRUE, col_types = colTypes);
  }
  dat = dat[1:4]
  colnames(dat) = c("HEADER_TIME_STAMP", "X", "Y", "Z")
  dat["HEADER_TIME_STAMP"] = as.POSIXct(dat[["HEADER_TIME_STAMP"]] * 60 * 60 * 24, origin="1899-12-30", tz = "GMT")
  dat["HEADER_TIME_STAMP"] = lubridate::force_tz(dat["HEADER_TIME_STAMP"], tzone = Sys.timezone())
  dat[2:4] = (dat[2:4] - 127) / 2^8 * 4
  options(digits.secs = 3);
  dat = as.data.frame(dat);
  return(dat)
}

#' @name import_actigraph_raw
#' @title Import and convert Actigraph raw csv files and load into data frame as in mhealth format.
#' @export
#' @importFrom readr count_fields tokenizer_csv read_csv
#' @importFrom mHealthR mhealth
#' @note Also support GT9X IMU csv file
#' @param filename full file path of input Actigraph raw csv file.
#' @param ad_convert set as TRUE only when the input Actigraph csv file is in analog quantized format and need to be converted into g value
#' @param ts_provided set as TRUE only when timestamp is provided as the first column
#' @param header_provided set as TRUE only when column header is provided
import_actigraph_raw = function(filename, ad_convert = FALSE, ts_provided = TRUE, header_provided = TRUE) {
  actigraph_meta = import_actigraph_meta(filename)

  ncols = readr::count_fields(filename, readr::tokenizer_csv(), n_max = 1, skip = 11)
  if (ts_provided) {
    colTypes = paste(c("c", rep("d", ncols - 1)),collapse = "")
  }else{
    colTypes = paste(rep("d", ncols), collapse = "")
  }


  if(header_provided){
    dat = readr::read_csv(
      filename, col_names = FALSE, skip = 11, trim_ws = TRUE, col_types = colTypes);
  }else{
    dat = readr::read_csv(
      filename, col_names = FALSE, skip = 10, trim_ws = TRUE, col_types = colTypes);
  }

  if(!ts_provided){
    ts_col = seq(from = actigraph_meta$st, to = actigraph_meta$dt, length.out = nrow(dat))
    dat = cbind(ts_col, dat)
  }

  dat = dat[,1:4]

  names(dat) = c(
    "HEADER_TIME_STAMP",
    "X",
    "Y",
    "Z"
  )

  if(ts_provided){
    timeFormat = ifelse(test = actigraph_meta$imu,
                        yes = "%Y-%m-%dT%H:%M:%OS",
                        no = "%m/%d/%Y %H:%M:%OS")
    dat[["HEADER_TIME_STAMP"]] = strptime(x = dat[["HEADER_TIME_STAMP"]],
                                                   format = timeFormat) + 0.0005
  }

  options(digits.secs = 3);
  dat = as.data.frame(dat);

  if(ad_convert){
    vs = actigraph_meta$vs
    res = actigraph_meta$res

    dat[,2:ncol(dat)] = (dat[,2:ncol(dat)] * vs / (2^res) - vs/2)/(vs/res)
    dat[,2:ncol(dat)] = as.data.frame(apply(dat[,2:ncol(dat)], 2, function(col){
      col[col == -5] = 0
      return(as.numeric(col))
    }))
  }
  return(dat)
}

#' @name import_actigraph_count
#' @title Import and convert Actigraph count csv files and load into data frame as in mhealth format.
#' @export
#' @importFrom readr read_csv cols col_character col_double
#' @importFrom mHealthR mhealth
#' @rdname import_actigraph_count
#' @param filename full file path of input Actigraph count csv file.
import_actigraph_count = function(filename, col_name = "ACTIGRAPH_COUNT", axes = c(2,3,4)) {
  dat = readr::read_csv(
    filename, col_names = TRUE, col_types = readr::cols(timestamp = readr::col_character(), axis1 = readr::col_double(), axis2 = readr::col_double(), axis3 = readr::col_double())
  );
  dat = data.frame(dat)
  dat[,1] = as.POSIXct(dat[,1], format = mHealthR::mhealth$format$csv$TIMESTAMP, tz = "UTC")
  if(length(axes) > 1){
    count_value = sqrt(rowSums(dat[,axes]^2))
  }else{
    count_value = dat[,axes]
  }
  dat[col_name] = count_value
  result = dat[c(1, ncol(dat))]
  colnames(result) = c("HEADER_TIME_STAMP", col_name);
  return(result)
}

#' @name import_actigraph_count_vm
#' @title Import and convert Actigraph count csv files and load into data frame as in mhealth format.
#' @export
#' @rdname import_actigraph_count
#' @importFrom readr read_csv cols col_character col_double
#' @importFrom mHealthR mhealth
#' @param filename full file path of input Actigraph count csv file.
import_actigraph_count_vm = function(filename, col_name = "ACTIGRAPH_COUNT") {
  dat = readr::read_csv(
    filename, col_names = TRUE, col_types = readr::cols(timestamp = readr::col_character(), vectormagnitude = readr::col_double())
  );
  dat = data.frame(dat)
  dat[,1] = as.POSIXct(dat[,1], format = mHealthR::mhealth$format$csv$TIMESTAMP, tz = Sys.timezone())
  result = dat
  colnames(result) = c("HEADER_TIME_STAMP", col_name);
  return(result)
}

#' @name import_biobank_enmo
#' @title Import and convert biobank epoch csv files and load into data frame as in mhealth format.
#' @export
#' @importFrom readr read_csv cols col_character col_double
#' @importFrom mHealthR mhealth
#' @param filename full file path of input biobank epoch csv file.
import_biobank_enmo = function(filename, col_name = "biobank_enmo") {
  dat = readr::read_csv(
    filename, col_names = TRUE
  );
  dat = data.frame(dat)
  dat = dat[1:2]
  dat[,1] = as.POSIXct(dat[,1], format = mHealthR::mhealth$format$csv$TIMESTAMP, tz = Sys.timezone())
  result = dat
  colnames(result) = c("HEADER_TIME_STAMP", col_name);
  return(result)
}

#' @name import_actigraph_meta
#' @title parse actigraph csv header to get related version and sampling rate information
#' @export
#' @importFrom stringr str_match str_detect
#' @importFrom R.utils countLines
import_actigraph_meta = function(filename, header = TRUE) {

  ACTIGRAPH_HEADER_SR_PATTERN = "([0-9]+) Hz"
  ACTIGRAPH_HEADER_FIRMWARE_PATTERN = "Firmware v([0-9]+.[0-9]+.[0-9]+)"
  ACTIGRAPH_HEADER_SOFTWARE_PATTERN = "ActiLife v([0-9]+.[0-9]+.[0-9]+)"
  ACTIGRAPH_HEADER_SERIALNUM_PATTERN = "Serial Number: ([A-Za-z0-9]+)"
  ACTIGRAPH_TIMESTAMP = "%m/%d/%Y %H:%M:%OS"

  headlines = readLines(filename, n = 10, encoding = "UTF-8");

  # Sampling rate
  sr_pattern = ACTIGRAPH_HEADER_SR_PATTERN
  sr = headlines[[1]]
  sr = stringr::str_match(sr, sr_pattern)
  sr = as.numeric(sr[2])

  # Firmware code
  fw_pattern = ACTIGRAPH_HEADER_FIRMWARE_PATTERN
  fw = headlines[[1]]
  fw = stringr::str_match(fw, fw_pattern)
  fw = fw[2]

  # Software code
  sw_pattern = ACTIGRAPH_HEADER_SOFTWARE_PATTERN
  sw = headlines[[1]]
  sw = stringr::str_match(sw, sw_pattern)
  sw = sw[2]

  # Serial number
  sn_pattern = ACTIGRAPH_HEADER_SERIALNUM_PATTERN
  sn = headlines[[2]]
  sn = stringr::str_match(sn, sn_pattern)
  sn = sn[2]

  # actigraph type
  at = substr(sn, 1, 3)

  # g range
  gr = switch(at,
              MAT = "3",
              CLE = "6",
              MOS = "8",
              TAS = "8")

  # IMU or not
  if (stringr::str_detect(headlines[[1]], "IMU")) {
    imu = TRUE
  }else{
    imu = FALSE
  }

  if(imu){
    gr = "16"
  }

  # Session start time
  st = headlines[[3]]
  sd = headlines[[4]]
  timeReg = "[0-9]{2}(:[0-9]{2}){1,2}+";
  dateReg = "[0-9]+/[0-9]+/[0-9]{4}";
  st = regmatches(st, regexpr(timeReg, st, perl = TRUE))
  sd = regmatches(sd, regexpr(dateReg, sd, perl = TRUE))
  st = paste(sd, st, sep = ' ')
  timeFormat = ACTIGRAPH_TIMESTAMP
  st = strptime(st, timeFormat) + 0.0005
  options(digits.secs = 3);

  # Session download time
  dt = headlines[[6]]
  dd = headlines[[7]]
  timeReg = "[0-9]{2}(:[0-9]{2}){1,2}+";
  dateReg = "[0-9]+/[0-9]+/[0-9]{4}";
  dt = regmatches(dt, regexpr(timeReg, dt, perl = TRUE))
  dd = regmatches(dd, regexpr(dateReg, dd, perl = TRUE))
  dt = paste(dd, dt, sep = ' ')
  timeFormat = ACTIGRAPH_TIMESTAMP
  dt = strptime(dt, timeFormat) + 0.0005
  options(digits.secs = 3);
  if(is.na(sr)){
    # determine sr by start and download time
    # options(digits = 13)
    duration = as.numeric(dt - st, units = "secs")
    if(header){
      nlines = countLines(filename) - 11
    }else{
      nlines = countLines(filename) - 10
    }
    sr = as.numeric(ceiling(nlines / duration))
  }

  # input voltage
  vs = headlines[[9]]
  vsReg = ": ([0-9](\\.[0-9]+)*)"
  vs = as.numeric(stringr::str_match(vs, vsReg)[2])

  # input resolution
  resolution = headlines[[9]]
  resReg = "= ([0-9]+)"
  resolution = as.numeric(stringr::str_match(resolution, resReg)[2])

  # header object as output
  header = {
  }
  header$sr = sr
  header$fw = fw
  header$sw = sw
  header$sn = sn
  header$st = st
  header$dt = dt
  header$at = at
  header$imu = imu
  header$gr = gr
  header$vs = vs
  header$res = resolution

  return(header)
}

#' #' @name import_hdf5
#' #' @title import hdf5 format sensor data file
#' #' @export
#' #' @importFrom h5 h5file
#' import_hdf5 = function(filename, key) {
#'   data <- h5::h5file(filename, "r")
#'   values = data[paste0(key, '/block1_values')][,1:3]
#'   ts = data[paste0(key, '/block0_values')][]/1000000000
#'   ts = as.POSIXct(ts, origin="1970-01-01", tz = "UTC")
#'   test_data = data.frame(values)
#'   test_data["HEADER_TIME_STAMP"] = ts
#'   colnames(test_data)[1:3] = c("X", "Y", "Z")
#'   result = test_data[c("HEADER_TIME_STAMP", 'X', 'Y', 'Z')]
#'   return(result)
#' }