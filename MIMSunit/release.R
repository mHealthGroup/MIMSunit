
pkgdown::build_site(examples = TRUE, mathjax = TRUE, lazy = FALSE, new_process = TRUE)
devtools::build(path = "dists/", vignettes = TRUE, manual = TRUE)
devtools::build(path = "dists/", vignettes = TRUE, manual = TRUE, binary = TRUE)