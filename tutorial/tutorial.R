# RCANVEC TUTORIAL

# ==== A review of baseplot: plot(), text(), points(), lines(), polygon(), arrows(), locator() ====

#setup data
xs <- c(-64.35134, -64.36888, -64.35984)
ys <- c(45.06345, 45.08933, 45.09176)
mylabels <- c("Graveyard", "Huggins", "TAN")

#plot x and y points (try pch=XX, cex=XX, xlim=c(X, X), and ylim=c(X, X))


#use points() to add points (try pch=XX)


#use arrows() to draw an arrow from "My House" to "Huggins" (try length=XX)


#use lines() to add lines (try col=XX, lty=XX, and lwd=XX)


#add text() labels (try adj=c(X, X) and cex=XX)


#add a polygon between the three points (try col=XX and border=XX)


#not sure where something is on your map and don't feel like typing?
#use locator() to find points (hit ESC when done!)


# ==== A review of plotting spatial data ====

#load packages
library(sp)
library(rgdal)

#use rgdal::readOGR() to read shapefiles, kml, etc
altashoreline <- readOGR(dsn="data", layer="alta_shoreline") #dsn=SOURCE FOLDER, #layer=FILENAME (minus .shp)

#plot data (try axes=TRUE, col=XX, lwd=XX, lty=XX, and border=XX)


#use locator() (ESC when done) and xlim=c(X1, X2); ylim=c(Y1, Y2) to zoom in


#we can use points(), lines(), polygon(), arrows(), and text() to overlay on this map (try using locator())


#plot Alta Lake and cores
altacores <- read.delim("data/alta_cores.txt")


# ==== Using {rosm} to plot your basemaps ===

library(prettymapr)
library(rosm)

# STEP 1: Find your bounding box (try prettymapr::searchbbox(XX, source="google")
# or http://www.openstreetmap.org/export and prettymapr::makebbox(n, e, s, w))


# STEP 2: Test your bounding box with osm.plot() or bmaps.plot()

# STEP 3: Choose your map type and zoom level

# STEP 4: Add overlays using points(), lines(), polygon(), arrows(), text() etc.
# you will need to use project=FALSE to keep the osm.plot() map in lat/lon coordinates


# ==== Using {rcanvec} plot basemaps ====

library(prettymapr)
library(rcanvec)

# STEP 1: Find your bounding box (try prettymapr::searchbbox(XX, source="google")
# or http://www.openstreetmap.org/export and prettymapr::makebbox(n, e, s, w))


#get familiar with your NTS sheet(s)


# STEP 2: Use canvec.qplot(bbox=) to preview your area (and refine your bbox)
# try layers=c("waterbody", "forest", "river", "contour", "building", "road")


# STEP 3: (Optional) Refine your plotting options
# try layerids "waterbody", "forest", "river", "contour", "building", and "road"
#load your data using canvec.load()


#plot() your data (try col="#D0EADD" for forest)


#you can also do this directly into canvec.qplot()


# STEP 4: Add overlays using points(), lines(), polygon(), arrows(), text() etc.


# Neat trick: use osm.plot(..., type="hillshade", project=FALSE, add=TRUE) to add hillshading



# ==== Use {prettymapr} to add north arrow and scale

library(prettymapr)

# plot map


# use addscalebar() and addnortharrow() 


# use prettymap() to do it all (try prettymap({...})) for multiple plotting expressions

# save to a pdf using pdf() ... dev.off()


