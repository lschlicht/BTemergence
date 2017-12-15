
.onLoad <- function(libname, pkgname){
  dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )
  packageStartupMessage(paste('This is', pkgname, dcf[, "Version"] ))

  #options(describeSNB_host 		 = 'localhost')

}

