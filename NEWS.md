# MIMSunit 0.7.0

## Improved

* Refine vignettes codes and texts.
* Better progress bar support for `MIMSunit::mims_unit` functions. Use `show_progress` to control whether to show progress bar during computation.

## Bug fixes

* Fix missing "testthat" dependency.
* Add `stringr` namespace before str_detect.

## Dependency changes

* Remove dependency on "akima". This package is not used.

## Breaking changes

* Remove dependency on "mHealthR" package.
* Add top-level API function `MIMSunit::mims_unit_from_files`.
* Add new plotting function `MIMSunit::generate_interactive_plot`.

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
* Add [`MIMSunit-dataset-shaker`](https://github.com/qutang/MIMSunit-dataset-shaker) as submodule.

# MIMSunit 0.5.10

* Exclude data from package building.
* Exclude inst scripts, figures, tables and data from package building.
* Remove unused files in R folder.

## Breaking changes

* Use `renv` for dependency management.
* CRAN ready.


# MIMSunit 0.5.7

* Refactor functions in `mims_unit.R`.

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
