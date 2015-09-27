#demo of rcanvec package

source("R/rcanvec.R")
source("R/nts.R")
source("R/canvec.R")

#load data
buildings <- canvec.load(nts("21h"), "building")
lakes <- canvec.load(nts("21h"), "waterbody")
rivers <- canvec.load(nts('21h'), "river")

#plot data
plot(lakes, xlim=c(-64.4,-64.35), ylim=c(45.05,45.1), col="lightblue", lty=0)
plot(rivers, add=T, col="lightblue")
plot(buildings, add=T, pch=".")
