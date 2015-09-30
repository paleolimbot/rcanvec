\name{rcanvec-package}
\alias{rcanvec-package}
\alias{rcanvec}
\docType{package}
\title{
What the package does (short line)
   package title   
}
\description{
More about what it does (maybe more than one line)
   A concise (1-5 lines) description of the package   
}
\details{
\tabular{ll}{
Package: \tab rcanvec\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-09-26\cr
License: \tab What license is it under?\cr
}
   An overview of how to use the package, including the most important functions   
}
\author{
Who wrote it

Maintainer: Who to complain to <yourfault@somewhere.net>
   The author and/or maintainer of the package   
}
\references{
   Literature or other references for background information   
}
   Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation   
   directory   
\keyword{ package }
\seealso{
   Optional links to other man pages, e.g.   
   \code{\link[<pkg>:<pkg>-package]{<pkg>}}   
}
\examples{

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
plot(lakes, col="lightblue", border="lightblue")
plot(rivers, add=TRUE, col="lightblue")
plot(buildings, add=TRUE, pch=".")

#zoomed in
plot(lakes, col="lightblue", border="lightblue", 
     xlim=c(-64.4,-64.35), ylim=c(45.05,45.1))
plot(contours, add=TRUE, col="brown", lwd=0.2)
plot(rivers, add=TRUE, col="lightblue")
plot(buildings, add=TRUE, pch=".")
plot(roads, add=TRUE, lwd=0.5)

#equivalent syntax in canvec.qplot()
canvec.qplot(nts("21h1"), contour=TRUE, building=TRUE, road=TRUE)
canvec.qplot(bbox=makebbox(45.1, -64.35, 45.05, -64.4), contour=TRUE, building=TRUE, road=TRUE)

#method returns plot data argument so data does not need to be loaded each time. this will not work
#when changing nts sheets.
plotdata <- canvec.qplot(nts("21h1"), contour=TRUE, building=TRUE, road=TRUE)
plotdata <- canvec.qplot(bbox=makebbox(45.1, -64.35, 45.05, -64.4), contour=TRUE, building=TRUE, road=TRUE,
                         data=plotdata)

#easy exporting with human readable names
canvec.export(nts("21h01"), "~/canvecdata", layerids=c("road", "river"))
# ..../canvec_021h01_shp/021h01_17_0_TR_1760009_1.dbf to ~/canvecdata/021H01_road.dbf 
# ..../canvec_021h01_shp/021h01_17_0_TR_1760009_1.prj to ~/canvecdata/021H01_road.prj 
# ..../canvec_021h01_shp/021h01_17_0_TR_1760009_1.shp to ~/canvecdata/021H01_road.shp 
# ..../canvec_021h01_shp/021h01_17_0_TR_1760009_1.shx to ~/canvecdata/021H01_road.shx 
# ..../canvec_021h01_shp/021h01_17_0_HD_1470009_1.dbf to ~/canvecdata/021H01_river.dbf 
# ..../canvec_021h01_shp/021h01_17_0_HD_1470009_1.prj to ~/canvecdata/021H01_river.prj 
# ..../canvec_021h01_shp/021h01_17_0_HD_1470009_1.shp to ~/canvecdata/021H01_river.shp 
# ..../canvec_021h01_shp/021h01_17_0_HD_1470009_1.shx to ~/canvecdata/021H01_river.shx
}
