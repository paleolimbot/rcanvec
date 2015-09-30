#functions involving canvec

# setup and create cache directory ----
#' Get the default cache directory, which is the folder rcanvec.cache
#' in the current working directory. Modify this behaviour by passing
#' a \code{cachedir} argument to canvec.download(), canvec.load(), or
#' canvec.qplot().
#' 
#' @export
canvec.cachedir <- function() {
  dirname <- file.path(getwd(), "rcanvec.cache")
  created<-suppressWarnings(dir.create(dirname))
  dirname
}


# Functions to get file names --------

canvec.layers <- function(...) {
  layerids <- list(...)
  filt <- match(layerids, canvec_layers$id)
  if(any(is.na(filt))) {
    stop("Could not find layer(s): ", paste(layerids[is.na(filt)], collapse=", "))
  }
  paste0(canvec_layers$filename[filt],
        canvec_layers$geometry_ext[filt])
} 

canvec.filename <- function(ntsid, ext=NULL) {
  if(length(ntsid)>=3) {
    #canvec
    out <- paste("canvec", paste(tolower(ntsid), collapse=""), "shp", sep="_")
  } else if(length(ntsid)==2) {
    #canvec+
    out <- paste("canvec", paste(toupper(ntsid), collapse=""), "shp", sep="_")
  } else {
    stop("Invalid nts passed to canvec.filename")
  }
  
  if(is.null(ext)) {
    out
  } else {
    paste0(out, ext)
  }
}

#' Generate url from a single NTS reference for canvec or canvec+
#' @export
canvec.url <- function(ntsid, server="http://ftp2.cits.rncan.gc.ca/pub") {
  if(length(ntsid)>=3) {
    #assume canvec, available in 50k sheets
    paste(server, "canvec/50k_shp", ntsid[1], tolower(ntsid[2]), canvec.filename(ntsid, ext=".zip"), sep="/")
  } else if(length(ntsid)==2) {
    #assume canvec+, only available in 250k sheets
    paste(server, "canvec+/shp", ntsid[1], canvec.filename(ntsid, ext=".zip"), sep="/")
  } else {
    stop("Invalid nts id passed to canvec.url: ", ntsid)
  }
}

#' Download and extract canvec or canvec+ data to \code{cachedir}.
#' @export
canvec.download <- function(..., forcedownload=FALSE, forceextract=FALSE, cachedir=NULL) {#list of ids
  if(is.null(cachedir)) {
    cachedir <- canvec.cachedir()
  }
  
  ntsids <- list(...)
  if(length(ntsids)==1 && class(ntsids[[1]])=="list") {
    ntsids <- ntsids[[1]]
  }
  
  for(ntsid in ntsids) {
    #get folder path
    folderpath <- paste(cachedir, canvec.filename(ntsid), sep="/")
    zippath <- paste(cachedir, canvec.filename(ntsid, ext=".zip"), sep="/")
    skipextract <- FALSE
    if(!file.exists(zippath) || forcedownload) { #don't know how to test if it is a directory
      #download
      uri <- canvec.url(ntsid)
      cat("Downloading sheet", paste(ntsid,collapse=""), "from", uri, "\n")
      tryCatch(download.file(uri, zippath),
               error=function(err) {
                 skipextract<<-TRUE
                 unlink(zippath)
                 cat("Could not download sheet ", paste(ntsid, collapse=""), " (sheet may not exist)")
               })
    } else {
      cat("Skipping download of", paste(ntsid,collapse=""), "\n")
    }
    if((!file.exists(folderpath) || forceextract || forcedownload) && !skipextract) {
      cat("Extracting to", folderpath, "\n")
      unzip(zipfile=zippath, exdir=folderpath, overwrite=TRUE)
    } else {
      cat("Skipping extraction", "\n")
    }
  }
  cat("Done\n")
}

canvec.findlayer <- function(ntsid, layerid, cachedir=NULL) {
  if(is.null(cachedir)) {
    cachedir <- canvec.cachedir()
  }
  wd <- file.path(cachedir, canvec.filename(ntsid))
  layername <- canvec.layers(layerid)
  
  if(length(ntsid)>=3) {
    #canvec
    files <- list.files(wd, pattern=paste0("*", paste0(toupper(layername), ".shp")))
    if(length(files)==1) {
      layername <- substr(files[1], 1, nchar(files[1])-4)
      c(wd, layername)
    } else {
      warning("Layer ", layerid, " does not exist for NTS ", ntsid)
      c(wd, NA)
    }
  } else if(length(ntsid)==2) {
    #canvec+
    c(wd, layername)
  }
}

#' Load layerid for NTS reference(s) that were previously downloaded to cachedir.
#' @export
canvec.load <- function(ntsid, layerid, cachedir=NULL) {
  if(is.null(cachedir)) {
    cachedir <- canvec.cachedir()
  }
  if(class(ntsid)=="list") {
    out <- list()
    for(singleid in ntsid) {
      out[[length(out)+1]] <- canvec.load(singleid, layerid, cachedir)
    }
    out
  } else {
    #check if file exists before reading
    layerinfo <- canvec.findlayer(ntsid, layerid, cachedir)
    if(is.na(layerinfo[2])) return(NULL) #shapefile not found in canvec
    shapefile <- file.path(layerinfo[1], paste0(layerinfo[2], ".shp"))
    if(file.exists(shapefile)) {
      rgdal::readOGR(dsn=layerinfo[1], layer=layerinfo[2])
    } else {
      warning("Layer ", layerid, " does not exist for NTS ", ntsid)
      NULL
    }
  }
}

#' Export layerids for NTS reference(s) to path tofolder, automatically renaming layers 
#' based on their associated layerid.
#' 
#' @export
canvec.export <- function(ntsid, tofolder, layerids=NULL, cachedir=NULL, overwrite=TRUE) {

  dir.create(tofolder)
  
  if(class(ntsid) != "list") {
    ntsid <- list(ntsid)
  }
  if(is.null(cachedir)) {
    cachedir <- canvec.cachedir()
  }
  if(is.null(layerids)) {
    layerids <- canvec_layers$id
  }
  
  layerinfo <- list()
  filesto <- rep(NA, length(layerids)*length(ntsid))
  for(i in 1:length(ntsid)) {
    for(j in 1:length(layerids)) {
      ind <- (i-1)*length(layerids)+j
      layerinfo[[ind]] <- canvec.findlayer(ntsid[[i]], layerid=layerids[j], cachedir=cachedir)
      filesto[ind] <- file.path(tofolder,
                                paste(paste(ntsid[[i]], collapse=""), layerids[j], sep="_"))
    }
  }
  
  extensions <- c(".cpg", ".dbf", ".prj", ".shp", ".shx")
  for(i in 1:length(layerinfo)) {
    for(ext in extensions) {
      filefrom <- file.path(layerinfo[[i]][1], paste0(layerinfo[[i]][2], ext))
      if(file.exists(filefrom)) {
        fileto <- paste0(filesto[i],ext)
        cat("Copying", filefrom, "to", fileto, "\n")
        file.copy(filefrom, fileto, overwrite=overwrite)
      } else {
        warning("File ", filefrom, " not found. not copied")
      }
    }
    
  }
  
}

#' Quickly plot one more NTS references or automatically look up references based on a bbox
#' (in the same format returned by sp::bbox())
#' @export
canvec.qplot <- function(ntsid=NULL, bbox=NULL, waterbody=TRUE, river=TRUE, contour=FALSE, building=FALSE, road=FALSE,
                         waterbody.col="lightblue", waterbody.border="lightblue", contour.col="brown",
                         contour.lwd=0.2, river.col="lightblue", river.lwd=1, road.col="black", road.lwd=0.5,
                         building.pch=".", building.col="black", plotdata=TRUE, cachedir=NULL, data=NULL, atscale=nts.SCALE50K, ...) {
  
  
  if(!is.null(bbox)) {
    if(is.null(ntsid)) {
      ntsid <- nts(bbox=bbox, atscale = atscale)
    }
    if(!exists("xlim"))
      xlim <- bbox[1,]
    if(!exists("ylim"))
      ylim <- bbox[2,]
  } else if(is.null(ntsid)) {
    stop("No arguments specified for data to plot")
  }
  
  if(is.null(cachedir)) {
    cachedir <- canvec.cachedir()
  }
  
  if(class(ntsid) != "list") {
    ntsid <- list(ntsid)
  }
  
  if(is.null(data)) {
    #download
    canvec.download(ntsid, cachedir=cachedir)
    #load data
    data <- list()
  }
  
  #does not take into account changed ntsids
  if(waterbody && is.null(data$waterbody))
    data$waterbody <- canvec.load(ntsid, "waterbody", cachedir=cachedir)
  if(building && is.null(data$building))
    data$building <- canvec.load(ntsid, "building", cachedir=cachedir)
  if(river && is.null(data$river))
    data$river <- canvec.load(ntsid, "river", cachedir=cachedir)
  if(road && is.null(data$road))
    data$road <- canvec.load(ntsid, "road", cachedir=cachedir)
  if(contour && is.null(data$contour))
    data$contour <- canvec.load(ntsid, "contour", cachedir=cachedir)
  
  if(plotdata) {
    #plot corners of mapsheet extents 
    if(class(ntsid)=="list") {
      if(length(ntsid)==0) stop("Cannot plot background for zero mapsheets")
      bbox1 <- nts.bbox(ntsid[[1]])
      for(singleid in ntsid) {
        bbox2 <- nts.bbox(singleid)
        bbox1 <- matrix(c(min(bbox1[1,1], bbox2[1,1]), min(bbox1[2,1], bbox2[2,1]),
                         max(bbox1[1,2], bbox2[1,2]), max(bbox1[2,2], bbox2[2,2])), ncol=2, byrow=FALSE)
      }
    } else {
      bbox1 = nts.bbox(ntsid)
    }
    coords <- coordinates(t(bbox1))
    spoints = SpatialPoints(coords, proj4string = CRS("+proj=longlat +ellps=GRS80 +no_defs"))
    
    if(!exists("xlim"))
      xlim <- bbox1[1,]
    if(!exists("ylim"))
      ylim <- bbox1[2,]
    plot(spoints, pch=".", xlim=xlim, ylim=ylim, ...)
    
    if(waterbody)
      for(layer in data$waterbody) plot(layer, add=T, col=waterbody.col, border=waterbody.border)
    if(contour)
      for(layer in data$contour) plot(layer, add=T, col=contour.col, lwd=contour.lwd)
    if(river)
      for(layer in data$river) plot(layer, add=T, col=river.col, lwd=river.lwd)
    if(building)
      for(layer in data$building) plot(layer, add=T, pch=building.pch, col=building.col)
    if(road)
      for(layer in data$road) plot(layer, add=T, lwd=road.lwd, col=road.col)
  }
  
  invisible(data)
}
