#NTS tools (translated from Java)

#nts format is a vector of character strings, e.g. c("021", "h", "01")

nts <- function(...) {
  ntsstring <- list(...)
  if(any(grepl("-", ntsstring))) {
    out <- strsplit(toupper(ntsstring),"-")
  } else {
    out <- list()
    for(i in 1:length(ntsstring)) {
      st <- toupper(ntsstring[i])
      if(nchar(st)==2){
        out[[i]] <- paste0("0", st)
      } else if(nchar(st)==3) {
        if(suppressWarnings(!is.na(as.numeric(st)))) {
          out[[i]] <- st
        } else {
          out[[i]] <- c(paste0("0", substr(st,1,2)), substr(st,3,3))
        }
      } else if(nchar(st) == 4) {
        out[[i]] <- c(substr(st,1,3), substr(st,4,4))
      } else {
        stop("50k mapsheets not yet supported by parsents")
      }
    }
  }
  
  if(length(out)==1) {
    out[[1]]
  } else {
    out
  }
}