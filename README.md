# rcanvec
An R package to access and plot CanVec and CanVec+ data for rapid basemap creation in Canada

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

canvec.qplot(bbox=searchbbox("wolfville ns", source="google"))

#or use {prettymapr} to make it look nice
prettymap(canvec.qplot(bbox=searchbbox("wolfville ns", source="google")))
```
![example](https://cloud.githubusercontent.com/assets/10995762/10892282/e17317c6-8178-11e5-8c9d-b7136796bb66.png)


See the [PDF Manual](https://github.com/paleolimbot/rcanvec/files/15517/rcanvec_0.1.3-manual.pdf) for more details, or see the [tutorial](tutorial/rcanvec_tutorial.md).
