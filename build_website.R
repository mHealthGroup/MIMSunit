
# renv does not work well with callr which was used by devtools and pkgdown, so use this workaround
# https://github.com/r-lib/callr/issues/139
system("Rcmd.exe INSTALL --no-multiarch --with-keep.source .")
roxygen2::roxygenize('.', roclets = c('rd', 'collate', 'namespace', 'vignette'))
pkgdown::build_site(examples = TRUE, lazy = FALSE, new_process = FALSE, devel = TRUE, preview = TRUE, )
remove.packages('MIMSunit', lib = .libPaths()[1])
