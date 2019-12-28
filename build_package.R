# build source and binary packages
renv::deactivate()
devtools::build(path = 'dists/', binary = TRUE, vignettes = TRUE, manual = TRUE)
devtools::build(path = 'dists/', vignettes = TRUE, manual = TRUE, binary = FALSE)
renv::activate()
