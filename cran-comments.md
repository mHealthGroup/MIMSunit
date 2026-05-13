## Test environments
* Ubuntu 26.04, R version 4.6.0

## R CMD check results
There were no ERRORs or WARNINGs.

There were 4 NOTEs:

* New submission after CRAN archival.
* Package was archived on CRAN on 2026-03-27 as issues were not corrected
  despite reminders.
* Local check environment does not have pandoc installed, so README.md and
  NEWS.md could not be checked.
* Local check environment does not have HTML tidy or V8 installed, and reports
  an RTC local time warning from timedatectl.

## Resubmission

This is a resubmission after CRAN archival on 2026-03-27. The release fixes
the failing `import_mhealth_csv()` example caused by the defunct `quoted_na`
argument in `readr >= 2`, while preserving the package's legacy CSV parsing
behavior.

## Downstream dependencies  
There is currently no downstream dependencies.
