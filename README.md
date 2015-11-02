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
```
library(rcanvec)
library(prettymapr)

canvec.qplot(bbox=searchbbox("wolfville ns", source="google"))

#or use {prettymapr} to make it look nice
prettymap(canvec.qplot(bbox=searchbbox("wolfville ns", source="google")))
```

See the [PDF Manual](https://github.com/paleolimbot/rcanvec/files/15517/rcanvec_0.1.3-manual.pdf) for more details, or see the [tutorial](https://github.com/paleolimbot/rcanvec/blob/master/tutorial/rcanvec_tutorial.md).