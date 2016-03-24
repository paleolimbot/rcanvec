# CanVec Data in R
[![](http://cranlogs.r-pkg.org/badges/rcanvec)](https://cran.r-project.org/package=rcanvec)

An R package to access and plot CanVec and CanVec+ data for rapid basemap creation in Canada

**NOTE**: If you're getting all kinds of warnings about how your sheet may not exist, you need to upgrade to the newest version of `rcanvec`! GeoGratis changed the download directory for sheets organized in this way such that an update is required to access the information. 

{ranvec} provides an interface to the National Topographic System (NTS), which is
the way in which a number of freely available Canadian datasets 
are organized. CanVec and CanVec+ datasets, which include all data used
to create Canadian topographic maps, are two such datasets that are useful
in creating vector-based maps for locations across Canada. This packages searches
CanVec data by location, plots it using pretty defaults, and exports it to
human-readable shapefiles for use in another GIS.

The gist of it:
```R
library(rcanvec)
library(prettymapr)

canvec.qplot(bbox=searchbbox("wolfville ns"))

#or use {prettymapr} to make it look nice
prettymap(canvec.qplot(bbox=searchbbox("wolfville ns"))
```
![example](https://cloud.githubusercontent.com/assets/10995762/10892282/e17317c6-8178-11e5-8c9d-b7136796bb66.png)


Find more information on the [CRAN package page](https://cran.r-project.org/package=rcanvec) or [view the manual](https://cran.r-project.org/web/packages/rcanvec/rcanvec.pdf).
