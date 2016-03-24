\name{rcanvec-package}
\alias{rcanvec-package}
\alias{rcanvec}
\docType{package}
\title{
Find, load, and plot Canadian vector basemap data   
}
\description{
Provides an interface to the National Topographic System (NTS), which is
the way in which several a number of freely available Canadian datasets 
are available. CanVec and CanVec+ datasets, which include all data used
to create Canadian topographic maps, are two such items that are useful
in creating vector-based maps for locations across Canada.
}
\details{
This package provides access to the CanVec/CanVec+ datasets via the \link{canvec.download}
function. CanVec data is organized by National Topographic System number (e.g. 021H01), 
so the \link{nts} function is provided to look up NTS number by location (e.g. lat/lon),
parsed text (e.g. "21h1"), or bounding box. Searching a bounding box using \code{prettymapr::searchbbox} based on human-readable location may be helpful.(e.g. \code{prettymapr::searchbbox("wolfville ns")}). 
Using the \link{canvec.qplot} function, this data can be plotted using default options for
standard layers. Combining the \code{searchbbox()} and \link{canvec.qplot} functions, it is possible to make a vector-based map of any location 
in Canada with one incredible super simple line of code (data is
downloaded automatically):
\code{canvec.qplot(bbox=searchbbox("wolfville ns"))}. For
prettier maps, the \link{canvec.export} function exports shapefiles in a human-readable
format (e.g. building_021H01.shp). If more refined
plotting or manipulation is desired, further functions are available to manually load or
obtain filenames of cached files. Files are downloaded to the working directory by default,
although this location can be defined using the \code{cachedir} argument available to
many functions.
}
\author{
Dewey Dunnington <dewey@fishandwhistle.net>
}
\references{
   \href{http://ftp2.cits.rncan.gc.ca/pub/canvec/archive/canvec+_archive_20151029/doc/CanVec+_en_distribution_formats.pdf}{CanVec+ Product Specifications},
   \href{http://www.nrcan.gc.ca/earth-sciences/geography/topographic-information/maps/9767}{National Topographic System (NTS) Documentation} 
}  
\keyword{ package }
\examples{
library(prettymapr)

#nts() function generates nts references based on lat, lon, or
#bounding coordinates
nts('21h')
nts('21h1')
nts('21h1', '21a16', '21A15')
nts(lat=45.2, lon=-64.32)
nts(lat=c(45.2, 46.2), lon=c(-64.32, -64.81))
nts(bbox=makebbox(45.125, -64.25, 44.875, -64.75))

#variant ntsstring() converts nts to string formats or takes the same
#arguments as nts() and returns a string vector instead.
ntsstring(c("021", "H", "01"))
ntsstring(bbox=makebbox(45.125, -64.25, 44.875, -64.75))
\donttest{
#bbox functions from {prettymapr} make it easy to manipulate bounding boxes
wolfville <- searchbbox("wolfville ns", source="google")
wolfvillezoomedout <- zoombbox(wolfville, 0.5)

#easy plotting with canvec.qplot()
canvec.qplot(bbox=searchbbox("wolfville ns", source="google"))

#download canvec or canvec+ data. 250k references use canvec+ (large amounts of data)
#and 50k references use canvec data (older but distributed in smaller chunks).
canvec.download(nts('21h1'))

#load data
buildings <- canvec.load(nts("21h1"), "building")
lakes <- canvec.load(nts("21h1"), "waterbody")
rivers <- canvec.load(nts('21h1'), "river")
roads <- canvec.load(nts('21h1'), "road")
contours <- canvec.load(nts('21h1'), "contour")

#plot data
sp::plot(lakes, col="lightblue", border="lightblue")
sp::plot(rivers, add=TRUE, col="lightblue")
sp::plot(buildings, add=TRUE, pch=".")

#zoomed in
sp::plot(lakes, col="lightblue", border="lightblue", 
     xlim=c(-64.4,-64.35), ylim=c(45.05,45.1))
sp::plot(contours, add=TRUE, col="brown", lwd=0.2)
sp::plot(rivers, add=TRUE, col="lightblue")
sp::plot(buildings, add=TRUE, pch=".")
sp::plot(roads, add=TRUE, lwd=0.5)


#equivalent syntax in canvec.qplot()
canvec.qplot(nts("21h1"), layers=c("waterbody", "contour", "river", "road"))
canvec.qplot(bbox=makebbox(45.1, -64.35, 45.05, -64.4), 
            layers=c("waterbody", "contour", "river", "building", "road"))

#method returns plot data argument so data does not need to be loaded each time. 
#this will not work when changing nts sheets.
plotdata <- canvec.qplot(nts("21h1"), layers=c("waterbody", "contour", "river"))
plotdata <- canvec.qplot(bbox=makebbox(45.1, -64.35, 45.05, -64.4), 
                          layers=c("waterbody", "contour", "river"),
                          data=plotdata)

#easy exporting with human readable names
canvec.export(nts("21h01"), "~/canvecdata", layers=c("road", "river"))
}

}
