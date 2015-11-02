# make a map of Alta Lake using rcanvec

# Load Libraries ====
library(rcanvec)
library(prettymapr)

# Find extents ====
# go to http://openstreetmap.org, go to your location,
# click "export" (top left of screen) and use makebbox(n, e, s, w)
alta <- makebbox(50.1231, -122.9592, 50.1034, -123.0060)
# or use the "searchbbox()" function in prettymapr
alta <- searchbbox("alta lake BC", source="google")

# Load and plot data ====
pdata <- canvec.qplot(bbox=alta, plotdata=FALSE)
canvec.qplot(bbox=alta, data=pdata)

#use prettymapr to plot a scalebar and north arrow
addscalebar()
addnortharrow()

#or use prettymapr to make it look prettier
prettymap(canvec.qplot(bbox=alta, data=pdata))

#use zoombbox() to zoom in or out, don't plot contours
prettymap(canvec.qplot(bbox=zoombbox(alta, 0.8), data=pdata))

# Write output to disk ====

pdf(file="altamap.pdf", height=8, width=6.5)
prettymap(canvec.qplot(bbox=zoombbox(alta, 0.8), data=pdata))
dev.off()

svg(filename="altamap.svg", height=8, width=6.5)
prettymap(canvec.qplot(bbox=zoombbox(alta, 0.8), data=pdata))
dev.off()

png(filename="altamap.png", height=8, width=6.5, units="in", res=300)
prettymap(canvec.qplot(bbox=zoombbox(alta, 0.8), data=pdata))
dev.off()
