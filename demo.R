#demo of rcanvec package

source("R/rcanvec.R")
source("R/nts.R")
source("R/canvec.R")

#download data
canvec.download(nts('21h'))

#load data
buildings <- canvec.load(nts("21h"), "building")
lakes <- canvec.load(nts("21h"), "waterbody")
rivers <- canvec.load(nts('21h'), "river")
roads <- canvec.load(nts('21h'), "road")
contours <- canvec.load(nts('21h'), "contour")

#plot data
plot(lakes, col="lightblue", border="lightblue")
plot(rivers, add=T, col="lightblue")
plot(buildings, add=T, pch=".")

#zoomed in
plot(NULL, xlim=c(-64.4,-64.35), ylim=c(45.05,45.1))
plot(lakes, add=T, col="lightblue", border="lightblue")
plot(contours, add=T, col="brown", lwd=0.2)
plot(rivers, add=T, col="lightblue")
plot(buildings, add=T, pch=".")
plot(roads, add=T, lwd=0.5)

#equivalent syntax in canvec.qplot()
plotdata <- canvec.qplot(nts("21h"))
canvec.qplot(nts("21h"), data=plotdata, xlim=c(-64.4,-64.35), ylim=c(45.05,45.1))

