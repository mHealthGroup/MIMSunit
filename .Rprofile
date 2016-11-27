packs = rownames(utils::installed.packages())
if(!'devtools' %in% packs){
  print("Not found devtools, install it...")
  install.packages('devtools')
}else{
  print("Found package: devtools")
}

if(!'mHealthR' %in% packs){
  print("Not found mHealthR, install it from github...")
  devtools::install_github("qutang/mHealthR")
}else{
  print("Found package: mHealthR")
}

print("Finish initial setup for package development")