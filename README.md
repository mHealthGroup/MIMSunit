README
================
Qu Tang
December 24, 2018

MIMS-unit algorithm
===================

[![Travis-CI build status](https://travis-ci.org/qutang/MIMSunit.svg?branch=master)](https://travis-ci.org/qutang/MIMSunit) [![CRAN Status](https://www.r-pkg.org/badges/version/MIMSunit)](https://cran.r-project.org/package=MIMSunit)

**This work is in submission. At the time of publication, this site will be updated with additional code/documentation. Before publication, the codes are subject to refactoring. Please create github [issues](https://github.com/qutang/mimsunit/issues/) if you have any question related to the package.**

Short introduction
------------------

MIMS-unit is abbreviated for *Monitor Independent Movement Summary* unit. This measurement is developed to harmonize the processing of accelerometer data from different devices. You may refer to the in publishing manuscript for the detail description of the algorithm. If you would like to get access to the manuscript, please contact [Qu Tang](mailto:%20tang.q@husky.neu.edu) or [Dinesh John](mailto:%20d.john@northeastern.edu).

Copyright and citation
----------------------

The copyright of the work belongs to Northeastern University, [mHealth Research Group](https://mhealthgroup.org). Please kindly cite the manuscript if you have used the package in your work.

> D. John, Q. Tang, F. Albinali, and S.S. Intille. 2018. *A monitor-independent movement summary to harmonize accelerometer data processing*. Journal of Measuring Physical Behaviors. Status: Submitted.

System Requirements
-------------------

1.  R (&gt;3.5.1)
2.  memory (&gt; 4GB)

### For Windows

Rtools 3.5 (see: <https://cran.r-project.org/bin/windows/Rtools/>)

### For Linux (use ubuntu as an example)

Install depedency system packages for `devtools`: `build-essential`, `libcurl4-gnutls-dev`, `libxml2-dev`, `libssl-dev`.

Installation
------------

1.  Stable version on CRAN (bundled or binary)

*Coming soon...*

1.  Development version (source codes)

-   For Windows users

``` r
install.packages("devtools")
devtools::install_github("qutang/MIMSunit", type='win.binary')
```

-   For mac or Linux users

``` r
install.packages("devtools")
devtools::install_github("qutang/MIMSunit")
```

*Note: It is recommended to use Rstudio when installing the package, because `devtools` has some compatible issues with R command line interface.*

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
