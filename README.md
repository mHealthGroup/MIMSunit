README
================
Qu Tang
November 21, 2017

MIMS-unit algorithm
===================

MIMS-unit is abbreviated for *Monitor Independent Movement Summary* unit. This measurement is developed to harmonize the processing of accelerometer data from different devices. See citation for detailed description of the algorithm.

> D. John, Q. Tang, F. Albinali, and S.S. Intille, A monitor-independent movement summary to harmonize accelerometer data processing. In: MSSE; 2017 (In submission)

Installation
------------

``` r
install.packages("devtools")
devtools::install_github('qutang/mHealthR')
devtools::install_github("qutang/MIMSunit")
```

Usage
-----

``` r
MIMSunit::mims_unit(input_dataframe, range=c(-3,3), breaks='1 min')
```

Assume the input dataframe is in following format, with the first column (timestamp) in `POSXlct` objects and the device used to collect this data has dynamic range being -3g to 3g. You may set the epoch length to be `1 min`, `1 sec`, `5 sec`, `10 sec` and so on.

    HEADER_TIME_STAMP,X,Y,Z
    2016-10-03 14:51:14.236,0.007,-0.005,0.984
    2016-10-03 14:51:14.256,0.008,-0.007,0.981
    2016-10-03 14:51:14.276,0.009,-0.006,0.978
    2016-10-03 14:51:14.297,0.009,-0.007,0.984
    2016-10-03 14:51:14.317,0.010,-0.010,0.982
    2016-10-03 14:51:14.337,0.011,-0.010,0.982
