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
