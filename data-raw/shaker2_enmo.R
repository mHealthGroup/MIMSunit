# create exported dataset for mhealth package
#
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)
require(GGIR)

folder = "F:/data/shaker2/test/";
output_folder = "F:/Data/shaker2/output_enmo/"

GGIR::g.shell.GGIR(datadir = folder, outputdir = output_folder, f0=1, f1=2,do.enmo=TRUE)

