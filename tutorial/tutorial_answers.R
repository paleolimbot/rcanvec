# RCANVEC TUTORIAL

# ==== A review of baseplot: plot(), text(), points(), lines(), polygon(), arrows(), locator() ====

#setup data
xs <- c(-64.35134, -64.36888, -64.35984)
ys <- c(45.06345, 45.08933, 45.09176)
mylabels <- c("My house", "Huggins", "TAN")

#plot x and y points (try pch=XX, cex=XX, xlim=c(X, X), and ylim=c(X, X))
plot(xs, ys)

#use points() to add points (try pch=XX)
points(xs, ys, pch=2)

#use arrows() to draw an arrow from "My House" to "Huggins" (try length=XX)
arrows(xs[1], ys[1], xs[2], ys[2])

#use lines() to add lines (try col=XX, lty=XX, and lwd=XX)
lines(xs, ys)

#add text() labels (try adj=c(X, X) and cex=XX)
text(xs, ys, labels=mylabels)

#add a polygon between the three points (try col=XX and border=XX)
polygon(xs, ys, col="green", border="red")

#not sure where something is on your map and don't feel like typing?
#use locator() to find points (hit ESC when done!)
locator()

# ==== A review of plotting spatial data ====

#load packages
library(sp)
library(rgdal)

#use rgdal::readOGR() to read shapefiles, kml, etc
altashoreline <- readOGR(dsn="data", layer="alta_shoreline") #dsn=SOURCE FOLDER, #layer=FILENAME (minus .shp)

#plot data (try axes=TRUE, col=XX, lwd=XX, lty=XX, and border=XX)
plot(altashoreline, col="lightblue")

#use locator() (ESC when done) and xlim=c(X1, X2); ylim=c(Y1, Y2) to zoom in
locator()
plot(altashoreline, xlim=c(-122.9870, -122.9757), ylim=c(50.11096, 50.12223), axes=TRUE)

#we can use points(), lines(), polygon(), arrows(), and text() to overlay on this map (try using locator())
points(locator())

#plot Alta Lake and cores
altacores <- read.delim("data/alta_cores.txt")

plot(altashoreline, col="lightblue", axes=TRUE)
points(altacores, pch=15, cex=.6)
text(altacores, labels=altacores$name, adj=c(-.2, 0.5), cex=.5)

# ==== Using {rosm} to plot your basemaps ===

library(prettymapr)
library(rosm)

# STEP 1: Find your bounding box (try prettymapr::searchbbox(XX, source="google")
# or http://www.openstreetmap.org/export and prettymapr::makebbox(n, e, s, w))
altalake <- searchbbox("alta lake, BC", source="google")

# STEP 2: Test your bounding box with osm.plot()
osm.plot(altalake)
bmaps.plot(altalake)

# STEP 3: Choose your map type and zoom level
osm.types()
osm.plot(altalake, type="stamenbw")

bmaps.types()
bmaps.plot(altalake, type="AerialWithLabels")

bmaps.plot(altalake, zoomin=1)
bmaps.plot(altalake, res=300)

# STEP 4: Add overlays using points(), lines(), polygon(), arrows(), text() etc.
# you will need to use project=FALSE to keep the osm.plot() map in lat/lon coordinates
# if that's what you would like
points(altacores, pch=15, cex=.6)
text(altacores, labels=altacores$name, adj=c(-.2, 0.5), cex=.5)


# ==== Using {rcanvec} plot basemaps ====

library(prettymapr)
library(rcanvec)

# STEP 1: Find your bounding box (try prettymapr::searchbbox(XX, source="google")
# or http://www.openstreetmap.org/export and prettymapr::makebbox(n, e, s, w))
altalake <- searchbbox("alta lake, BC", source="google")

#get familiar with your NTS sheet(s)
nts(bbox=altalake)

# STEP 2: Use canvec.qplot(bbox=) to preview your area (and refine your bbox)
# try layers=c("waterbody", "forest", "river", "contour", "building", "road")
canvec.qplot(bbox=altalake)

# STEP 3: (Optional) Refine your plotting options
# try layerids "waterbody", "forest", "river", "contour", "building", and "road"
#load your data using canvec.load()
waterbody <- canvec.load(nts(bbox=altalake), layerid="waterbody")
rivers <- canvec.load(nts(bbox=altalake), layerid="river")
forest <- canvec.load(nts(bbox=altalake), layerid="forest")

#plot() your data (try col="#D0EADD" for forest)
plot(waterbody, col="lightblue", border=0, xlim=altalake[1,], ylim=altalake[2,])
plot(forest, col="#D0EADD", border=0, add=T)
plot(rivers, col="lightblue", add=T)

#you can also do this directly into canvec.qplot()
plotoptions = list()
plotoptions$waterbody <- list(col="lightblue", border=0)
plotoptions$roads <- list()

# STEP 4: Add overlays using points(), lines(), polygon(), arrows(), text() etc.
canvec.qplot(bbox=altalake)
points(altacores, pch=15, cex=.6)
text(altacores, labels=altacores$name, adj=c(-.2, 0.5), cex=.5)

# Neat trick: use osm.plot(..., type="hillshade", project=FALSE, add=TRUE) to add hillshading
canvec.qplot(bbox=altalake)
osm.plot(altalake, type="hillshade", project=FALSE, add=TRUE)


# ==== Use {prettymapr} to add north arrow and scale

library(prettymapr)

# plot map
bmaps.plot(altalake)

# use addscalebar() and addnortharrow() 
addscalebar()
addnortharrow()

# use prettymap() to do it all (try prettymap({...})) for multiple plotting expressions
prettymap(bmaps.plot(altalake))

# save to a pdf using pdf() ... dev.off()
pdf()
prettymap(bmaps.plot(altalake))
dev.off()

