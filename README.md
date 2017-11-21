# Source codes, data and publication results for MIMS-unit algorithm

## Installation

Make sure you have `R` and preferred `R studio` installed. If on windows, make sure `Rtools` is installed.

```r
install.packages('devtools')
devtools::install_github('qutang/mHealthR')
devtools::install_github('qutang/MIMSunit')
library('MIMSunit')
```

## Data for reproducing results

All data are saved in `rds` format (loadable by R script) in `inst/extdata` folder.

## Script for reproducing publication results

All scripts are saved in `inst/src` folder. You may run each script to generate different plots or tables independently.

## Use MIMS-unit algorithm in your own script

After loading the library, make sure your data (in dataframe) is in following format. The first column (timestamp) should be `POXlct` objects.

```
HEADER_TIME_STAMP,X,Y,Z
2010-09-01 11:30:12.213,0.123,0.523,0.786
...
```

Assuming your data is in variable `df`, knowing accelerometer data's dynamic range (i.e. `c(-3, 3)`) and the desired epoch (i.e. '1 min'). You may run following script to generate MIMS-unit values.

```r
output = mims_unit(df = df, range = c(-3,3), breaks = '1 min')
```

Output will be in the same format as input, with the first column being the start time of each epoch segment, and the second column being the MIMS-unit value.
