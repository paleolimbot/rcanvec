#functions involving canvec



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
                                        "transmission_line", "wall/fence", "pipeline_sewage", 
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
                                geometry = c("line", "polygon", 
                                            "point", "point", "line", "line", "line", "line", "line", "line", 
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

canvec_filename <- function(ntsid, ext=NULL) {
  out <- paste("canvec", paste(toupper(ntsid[1:2]), collapse=""), "shp", sep="_")
  if(is.null(ext)) {
    out
  } else {
    paste0(out, ext)
  }
}

canvec_url <- function(ntsid, server="http://ftp2.cits.rncan.gc.ca/pub/canvec+/shp") {
  paste(server, ntsid[1], canvec_filename(ntsid, ext=".zip"), sep="/")
}

canvec.shapefiles <- function(ntsid, ..., cachedir=NULL) {
  if(is.null(cachedir)) {
    cachedir <- canvec_cachedir()
  }
  
  if(class(ntsid)=="list") {
    #return layers for all nts ids
    nlayers <- length(list(...))
    out <- rep("", length(ntsid)*nlayers)
    for(i in 1:length(ntsid)) {
      singleid <- ntsid[[i]]
      startindex <- (i-1)*nlayers+1
      endindex <- i*nlayers
      out[startindex:endindex] <- canvec_cached(singleid, ..., cachedir=cachedir)
    }
    out
  } else {
    paste(cachedir, canvec_filename(ntsid), canvec_shapefilename(...), sep="/")
  }
}

canvec.download <- function(..., forcedownload=FALSE, forceextract=FALSE, cachedir=NULL) {#list of ids
  if(is.null(cachedir)) {
    cachedir <- canvec_cachedir()
  }
  
  ntsids <- list(...)
  if(length(ntsids)==1 && class(ntsids[[1]])=="list") {
    ntsids <- ntsids[[1]]
  }
  
  for(ntsid in ntsids) {
    #get folder path
    folderpath <- paste(cachedir, canvec_filename(ntsid), sep="/")
    zippath <- paste(cachedir, canvec_filename(ntsid, ext=".zip"), sep="/")
    if(!file.exists(zippath) || forcedownload) { #don't know how to test if it is a directory
      #download
      uri <- canvec_url(ntsid)
      cat("Downloading sheet", paste(ntsid,collapse="-"), "from", uri, "\n")
      download.file(uri, zippath)
    } else {
      cat("Skipping download of", paste(ntsid,collapse="-"), "\n")
    }
    if(!file.exists(folderpath) || forceextract) {
      cat("Extracting to", folderpath, "\n")
      unzip(zipfile=zippath, exdir=folderpath)
    } else {
      cat("Skipping extraction", "\n")
    }
  }
  cat("Done")
}

canvec.load <- function(ntsid, layerid, cachedir=NULL) {
  library(rgdal)
  if(is.null(cachedir)) {
    cachedir <- canvec_cachedir()
  }
  #check if file exists before reading
  if(file.exists(canvec.shapefiles(ntsid, layerid, cachedir=cachedir))) {
    layername <- canvec.layers(layerid)
    wd <- paste(cachedir, canvec_filename(ntsid), sep="/")
    readOGR(dsn=wd, layer=layername)
  } else {
    NULL
  }
}
