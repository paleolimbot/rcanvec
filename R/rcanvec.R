#setup globals
canvec_cachedir <- function() {paste(getwd(), "rcanvec.cache", sep="/")}
dir.create(canvec_cachedir()) #change to run only if directory does not exist
