##### Plotting the current distribution of livestock

## Main question: how do we work in eastings and northings?

plot(livestock$Easting, livestock$Northing)

counties <- readShapePoly(paste0(folder, '/county_lddg_totals/county_LDDG_totals.shp'))
summary(counties)
plot(counties, col ='light yellow')

points(livestock$Easting, livestock$Northing)

## need to change livestock to be a SpatialPointsDataFrame but does that mean I need to know its coordinates?
livestock<-livestock[complete.cases(livestock),]
coordinates(livestock) <- ~Easting+Northing

bubble(livestock[,2:4], zcol='Number.Cattle')
