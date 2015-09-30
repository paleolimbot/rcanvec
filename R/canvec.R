#functions involving canvec

# setup and create cache directory ----
canvec.cachedir <- function() {
  dirname <- file.path(getwd(), "rcanvec.cache")
  created<-suppressWarnings(dir.create(dirname))
  dirname
}

#Define canvec layers data frame ----

canvec_layers <- structure(list(name = c("Navigational aid", "Residential area", 
                                         "Parabolic antenna", "Building", "Chimney", "Tank", "Cross", 
                                         "Transmission line", "Wall/fence", "Pipeline (Sewage/liquid waste)", 
                                         "Well", "Underground reservoir", "Silo", "Tower", "Power transmission line", 
                                         "Pipeline", "Valve", "Gas and oil facilities", "Transformer station", 
                                         "Wind-operated device", "Contour", "Landform", "Esker", "Glacial debris undifferentiated", 
                                         "Moraine", "Sand", "Tundra polygon", "Pingo", "Elevation point", 
                                         "Contour imperial", "Elevation point imperial", "Permanent snow and ice", 
                                         "Shoreline", "Manmade hydrographic entity", "Hydrographic obstacle entity", 
                                         "Single line watercourse", "Waterbody", "Island", "Pit", "Quarry", 
                                         "Extraction area", "Mine", "Peat cutting", "Domestic waste", 
                                         "Industrial solid waste", "Industrial and commercial area", "Lumber yard", 
                                         "Mining area", "Landmass", "Municipality Regional area", "Upper Municipality", 
                                         "Municipality", "Aboriginal Lands", "NTS50K boundary polygon", 
                                         "Lookout", "Ski centre", "Cemetery", "Fort", "Marina", "Sports track/Race track", 
                                         "Golf course", "Camp", "Drive-in theatre", "Botanical garden", 
                                         "Shrine", "Historical site/Point of interest", "Amusement park", 
                                         "Park/sports field", "Footbridge", "Ruins", "Trail", "Stadium", 
                                         "Campground", "Picnic site", "Golf drining range", "Exhibition ground", 
                                         "Zoo", "Tundra pond", "Palsa bog", "Saturated soil", "Wetland", 
                                         "String bog", "Named feature", "Railway", "Railway Structure", 
                                         "Rail Ferry", "Railway Station", "Runway", "Ferry connection segment", 
                                         "Road segment", "Junction", "Blocked passage", "Toll point", 
                                         "Wooded area", "Cut line"), 
                                id = c("navigational_aid", "residential_area", 
                                        "parabolic_antenna", "building", "chimney", "tank", "cross", 
                                        "transmission_line", "wall_fence", "pipeline_sewage", 
                                        "well", "underground_reservoir", "silo", "tower", "power_transmission_line", 
                                        "pipeline", "valve", "gas_and_oil_facilities", "transformer_station", 
                                        "wind-operated_device", "contour", "landform", "esker", "glacial_debris_undifferentiated", 
                                        "moraine", "sand", "tundra_polygon", "pingo", "elevation_point", 
                                        "contour", "elevation_point", "permanent_snow_and_ice", 
                                        "shoreline", "manmade_hydrographic_entity", "hydrographic_obstacle_entity", 
                                        "river", "waterbody", "island", "pit", "quarry", 
                                        "extraction_area", "mine", "peat_cutting", "domestic_waste", 
                                        "industrial_solid_waste", "industrial_and_commercial_area", "lumber_yard", 
                                        "mining_area", "landmass", "municipality_regional_area", "upper_municipality", 
                                        "municipality", "aboriginal_lands", "nts50k_boundary_polygon", 
                                        "lookout", "ski_centre", "cemetery", "fort", "marina", "sports_track", 
                                        "golf_course", "camp", "drive-in_theatre", "botanical_garden", 
                                        "shrine", "historical_site/point_of_interest", "amusement_park", 
                                        "park/sports_field", "footbridge", "ruins", "trail", "stadium", 
                                        "campground", "picnic_site", "golf_drining_range", "exhibition_ground", 
                                        "zoo", "tundra_pond", "palsa_bog", "saturated_soil", "wetland", 
                                        "string_bog", "named_feature", "railway", "railway_structure", 
                                        "rail_ferry", "railway_station", "runway", "ferry_connection_segment", 
                                        "road", "junction", "blocked_passage", "toll_point", 
                                        "forest", "cut_line"), 
                                theme = c("bs", "bs", "bs", "bs", 
                                          "bs", "bs", "bs", "bs", "bs", "bs", "bs", "bs", "bs", "bs", "en", 
                                          "en", "en", "en", "en", "en", "fo", "fo", "fo", "fo", "fo", "fo", 
                                          "fo", "fo", "fo", "fo", "fo", "hd", "hd", "hd", "hd", "hd", "hd", 
                                          "hd", "ic", "ic", "ic", "ic", "ic", "ic", "ic", "ic", "ic", "ic", 
                                          "la", "la", "la", "la", "la", "li", "lx", "lx", "lx", "lx", "lx", 
                                          "lx", "lx", "lx", "lx", "lx", "lx", "lx", "lx", "lx", "lx", "lx", 
                                          "lx", "lx", "lx", "lx", "lx", "lx", "lx", "ss", "ss", "ss", "ss", 
                                          "ss", "to", "tr", "tr", "tr", "tr", "tr", "tr", "tr", "tr", "tr", 
                                          "tr", "ve", "ve"), 
                                filename = c("bs_1250009", "bs_1370009", "bs_2000009", 
                                            "bs_2010009", "bs_2060009", "bs_2080009", "bs_2120009", "bs_2230009", 
                                            "bs_2240009", "bs_2310009", "bs_2350009", "bs_2380009", "bs_2440009", 
                                            "bs_2530009", "en_1120009", "en_1180009", "en_1340009", "en_1360049", 
                                            "en_1360059", "en_2170009", "fo_1030009", "fo_1080019", "fo_1080029", 
                                            "fo_1080039", "fo_1080049", "fo_1080059", "fo_1080069", "fo_1080079", 
                                            "fo_1200009", "fo_2570009", "fo_2610009", "hd_1140009", "hd_1440009", 
                                            "hd_1450009", "hd_1460009", "hd_1470009", "hd_1480009", "hd_1490009", 
                                            "ic_1350019", "ic_1350029", "ic_1350039", "ic_1350049", "ic_1350059", 
                                            "ic_1360019", "ic_1360029", "ic_1360039", "ic_2110009", "ic_2600009", 
                                            "la_1150009", "la_1680019", "la_1680029", "la_1680039", "la_1690009", 
                                            "li_1210009", "lx_1000019", "lx_1000029", "lx_1000039", "lx_1000049", 
                                            "lx_1000069", "lx_1000079", "lx_1000089", "lx_2030009", "lx_2070009", 
                                            "lx_2200009", "lx_2210009", "lx_2220009", "lx_2260009", "lx_2270009", 
                                            "lx_2280009", "lx_2400009", "lx_2420009", "lx_2460009", "lx_2480009", 
                                            "lx_2490009", "lx_2500009", "lx_2510009", "lx_2560009", "ss_1320019", 
                                            "ss_1320029", "ss_1320039", "ss_1320049", "ss_1320059", "to_1580009", 
                                            "tr_1020009", "tr_1040009", "tr_1050009", "tr_1060009", "tr_1190009", 
                                            "tr_1750009", "tr_1760009", "tr_1770009", "tr_1780009", "tr_1790009", 
                                            "ve_1240009", "ve_2290009"), 
                                geometry = c("point", "polygon", 
                                            "point", "point", "point", "point", "point", "line", "line", "line", 
                                            "point", "point", "point", "point", "line", "line", "point", 
                                            "point", "point", "point", "line", "polygon", "line", "polygon", 
                                            "polygon", "polygon", "polygon", "point", "point", "line", "point", 
                                            "polygon", "line", "point", "point", "line", "polygon", "polygon", 
                                            "polygon", "polygon", "point", "point", "polygon", "polygon", 
                                            "point", "point", "polygon", "point", "polygon", "polygon", "polygon", 
                                            "polygon", "polygon", "polygon", "point", "point", "point", "polygon", 
                                            "point", "line", "polygon", "point", "point", "polygon", "point", 
                                            "point", "polygon", "polygon", "line", "point", "line", "polygon", 
                                            "point", "point", "point", "polygon", "polygon", "polygon", "polygon", 
                                            "polygon", "polygon", "polygon", "point", "line", "point", "line", 
                                            "point", "point", "line", "line", "point", "point", "point", 
                                            "polygon", "line")), 
                     .Names = c("name", "id", "theme", "filename", "geometry"), 
                     class = "data.frame", 
                     row.names = c(NA, -95L))

canvec_layers$geometry_ext <- gsub("point", "_0", gsub("line", "_1", gsub("polygon", "_2", canvec_layers$geometry)))

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

canvec.load <- function(ntsid, layerid, cachedir=NULL) {
  library(rgdal)
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
      readOGR(dsn=layerinfo[1], layer=layerinfo[2])
    } else {
      warning("Layer ", layerid, " does not exist for NTS ", ntsid)
      NULL
    }
  }
}

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
