# MIMSunit 0.11.2

* Remove "matlab" dependency.

## BREAKING CHANGES

* Remove "remove_average" and "bessel" filtering functions. 

# MIMSunit 0.11.1

* Resolve CRAN release ERROR.

# MIMSunit 0.11.0

## BREAKING CHANGES

* Add a new input argument `use_snapshot_to_check` to `mims_unit()` to configure the data used to check duplicated timestamps.

* Enable `has_ts` argument in `import_actigraph_csv()` and `import_actigraph_csv_chunked()` functions. Now the timestamps will be computed based on the sampling rate and start time in the actigraph csv header.

## Bug fixes

* Check duplicated timestamps and raise error when calling `mims_unit()`. Use `diff` function to accelerate the computation ([@vincentvanhees](https://github.com/vincentvanhees)).
* Fix the `rest_on_table` sample data to remove duplicated timestamps in it.

## Documentation

* Update FAQ about the duplicated timestamps problem.

# MIMSunit 0.10.0

## BREAKING CHANGES

* `import_actigraph_csv()` and `import_actigraph_csv_chunked()` functions no longer support `has_ts` flag. Users should always ensure input data have timestamps in the first column. **Note that for versions `<=0.9.2`, always make sure the input raw accelerometer data includes timestamps in the first column and set `has_ts` to TRUE when using `import_actigraph_csv()` and `import_actigraph_csv_chunked()` functions to avoid a known bug.**

## Bug fixes

* Remove warnings from dplyr > 1.0.0.
* The first column of the input data.frame does not have to be HEADER_TIME_STAMP. [#29](https://github.com/mHealthGroup/MIMSunit/issues/29).

## Refactor

* Refactor `export_to_actilife()` and `conceptual_diagram` vignette to be compatible with newer versions of `readr` and `plyr` packages.

## Development

* Update development package versions

## Documentation

* Add back man pages to the github development version.
* Fix invalid urls in any documentations.

# MIMSunit 0.9.2

## Improvement

* Optimize memory usage when computing MIMS unit values. @muschellij2
* Reduce sample data size to accelerate example execution time.
* Support shiny progress indicator when running `mims_unit()` functions in a shiny app.

## Documentation

* Fix cross-package references.

# MIMSunit 0.9.1

## Features

* Added a function `shiny_app()` to start a local shiny app that allows users to load local files to compute mims unit values and provides an interactive graph to view the computed values. 

## Improvement

* Optimize memory usage when computing MIMS unit values.

## Documentation

* Added datasets article `vignette("datasets")`.

# MIMSunit 0.9.0

## Bug fixes

* Check column existence before coverting data type in `import_mhealth_csv()`.
* Quick failure if dynamic_range is not specified.
* Auto set `has_ts` to `False` when there are only three columns in the input data in `import_actigraph_csv()`.

## Features

* Sort timestamps of input data before computing mims unit values in `custom_mims_unit()`.

## Documentation

* Indicate columns cannot be reordered in the argument description of `extrapolate()` and `mims_unit()`.

## CI

* Add appveyor for non-Github actions testing


# MIMSunit 0.8.2

## Bug fixes

* Make sure codes do not change options.
* Fix timestamps in `import_actigraph_csv_chunked()`.

# MIMSunit 0.8.1

## New features

* Add two new sample data `edge_case` and `rest_on_table`.
* Add one new external sample data `actigraph_timestamped.csv`.

## Improvement

* Use tempfile in examples.

## Documentation

* Remove unused vignettes.
* Add documentation for the new sample data.
* Use internal data for all vignettes.

# MIMSunit 0.8.0

## New features

* New import data functions `import_actigraph_csv_chunked()` and `import_mhealth_csv_chunked()` to support loading large csv files in chunks. See issue [#11](https://github.com/mHealthGroup/MIMSunit/issues/11).

## Improvement

* Use chunked import functions in `mims_unit_from_files()` to support large input files.

## Bug fixes

* Column naming in `vector_magnitude()` and `sum_up()` functions.
* Crashing bug in plotting functions.
* Chances of incorrect parsing of time zone during interpolation and extrapolation.

## Documentation

* Add examples to functions.

## Minor changes

* `show_progress` is discarded in top-level `mims_unit()` functions. Use `use_gui_progress` instead.

## BREAKING CHANGES

* Minimal R version requirement is bumped to 3.6.0.

# MIMSunit 0.7.0

## Improvement

* Refine vignettes codes and texts.
* Better progress bar support for `mims_unit()` functions. Use `show_progress` to control whether to show progress bar during computation.

## Bug fixes

* Fix missing "testthat" dependency.
* Add `stringr` namespace before str_detect.

## Dependency changes

* Remove dependency on "akima". This package is not used.

## Breaking changes

* Remove dependency on "mHealthR" package.
* Add top-level API function `mims_unit_from_files()`.
* Add new plotting function `generate_interactive_plot()`.

# MIMSunit 0.6.2

## Bug fixes

* Fix crashing error in plotting functions when running vignettes.

# MIMSunit 0.6.1

## Bug fixes

* MIMSunit axial values are now passed through the [truncation algorithm](https://mhealthgroup.github.io/MIMSunit/articles/truncation_threshold.html) before computing the axial sum.

# MIMSunit 0.6.0

* Add dev dependencies to renv.

## Breaking changes

* Add two datasets for experiment results (see [this article](https://mhealthgroup.github.io/MIMSunit/articles/cross_device_consistency.html)).
* Move MIMSunit R project one level down to a subdirectory to better support git submodules.
* Add [`MIMSunit-dataset-shaker`](https://github.com/mHealthGroup/MIMSunit-dataset-shaker/) as submodule.

# MIMSunit 0.5.10

* Exclude data from package building.
* Exclude inst scripts, figures, tables and data from package building.
* Remove unused files in R folder.

## Breaking changes

* Use `renv` for dependency management.
* CRAN ready.


# MIMSunit 0.5.7

* Refactor `mims_unit()`.

## Breaking changes

* API changes for the following functions. Please refer to the documentations for the details.
  * `sensor_orientations()` is Added as an entry function to compute sensor orientations.
  * `mims_unit()` is simplified to not allow customization on parameters.
  * Old `mims_unit()` is now `custom_mims_unit()`, which allows full tuning of algorithm parameters.

# MIMSunit 0.5.6

* Refactor functions in `extrapolate.R`, `interpolate.R` and `simulate_data.R`.

## Breaking changes

* API changes for the following functions. Please refer to the documentations for the details.
  * `crop_grange()` is now `cut_off_signal()`
  * `make_new_data()` is now `simulate_new_data()`
  * `extrapolate.data.frame()` is now `extrapolate()`
  * `extrapolate()` is now `extrapolate_single_col()`

# MIMSunit 0.5.5

* Added a `NEWS.md` file to track changes to the package.
* Refactor functions in `import_data.R` file.

## Breaking changes

* API changes for the following functions. Please refer to the documentations for the details.
  * `import_actigraph_raw()` is now `import_actigraph_csv()`
  * `import_actigraph_count()` is now `import_actigraph_count_csv()`
  * `import_biobank_enmo()` is now `import_enmo_csv()`
  * `import_activpal_raw()` is now `import_activpal3_csv()`
