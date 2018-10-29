# MIMSunit 0.5.5

* Added a `NEWS.md` file to track changes to the package.
* Refactor functions in `import_data.R` file.

## Breaking changes

* API change for the following functions. Please refer to the documentations for the details.
  * `import_actigraph_raw()` is now `import_actigraph_csv()`
  * `import_actigraph_count()` is now `import_actigraph_count_csv()`
  * `import_biobank_enmo()` is now `import_enmo_csv()`
  * `import_activpal_raw()` is now `import_activpal3_csv()`
