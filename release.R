
# format codes
formatR::tidy_dir("R", comment = TRUE, blank = TRUE, arrow = TRUE, brace.newline = TRUE)
devtools::build(path = "dists/", vignettes = TRUE, manual = TRUE)
devtools::build(path = "dists/", vignettes = TRUE, manual = TRUE, binary = TRUE)