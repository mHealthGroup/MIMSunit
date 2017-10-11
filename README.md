# Source codes, data and publication results for SMART-counts algorithm

## Installation

Make sure you have `R` and preferred `R studio` installed. If on windows, make sure `Rtools` is installed.

```
install.packages('devtools')
devtools::install_github('qutang/mHealthR')
devtools::install_github('qutang/SMARTcounts')
library('SMARTcounts')
```

## Data for reproducing results

All data are saved in `rds` format (loadable by R script) in `inst/extdata` folder.

## Script for reproducing publication results

All scripts are saved in `inst/src` folder. You may run each script to generate different plots or tables independently.

## Use SMARTcounts algorithm in your own script

After loading the library, make sure your data (in dataframe) is in following format. The first column (timestamp) should be `POXlct` objects.

```
HEADER_TIME_STAMP,X,Y,Z
2010-09-01 11:30:12.213,0.123,0.523,0.786
...
```

Assuming your data is in variable `df`, knowing accelerometer data's dynamic range (i.e. `c(-3, 3)`) and the desired epoch (i.e. '1 min'). You may run following script to generate SMART-counts values.

```
output = activity_count(df = df, range = c(-3,3), breaks = '1 min')
```

Output will be in the same format as input, with the first column being the start time of each epoch segment, and the second column being the SMART-count value.
