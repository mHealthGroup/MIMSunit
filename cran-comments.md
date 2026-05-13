## Test environments
* Ubuntu 26.04, R version 4.6.0

## R CMD check results
There were no ERRORs or WARNINGs.

The remaining CRAN incoming feasibility NOTEs are expected for this
resubmission:

* New submission after CRAN archival on 2026-03-27.
* Package was archived on CRAN on 2026-03-27 as issues were not corrected
  despite reminders.

In the local Ubuntu 26.04 r-devel check, additional environment-specific NOTEs
were reported because pandoc, HTML tidy, and V8 are not installed, and because
the local RTC is configured in local time. These are not package issues.

## Resubmission

This is a resubmission after CRAN archival on 2026-03-27. The release fixes
the failing `import_mhealth_csv()` example caused by the defunct `quoted_na`
argument in `readr >= 2`, while preserving the package's legacy CSV parsing
behavior.

This resubmission also addresses incoming pretest NOTEs by adding accepted
technical and author names to `inst/WORDLIST` and reducing the runtime of two
examples without changing package behavior.

## Maintainer change

The package maintainer changed from Qu Tang <tang.q@northeastern.edu> to
Umberto Mazzucchelli <mazzucchelli.u@northeastern.edu>. Confirmation from the
previous maintainer address can be provided separately if requested by CRAN.

## Downstream dependencies  
There is currently no downstream dependencies.
