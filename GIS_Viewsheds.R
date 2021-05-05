####Interactive visualisation of environmental data in decision support systems



options("rgdal_show_exportToProj4_warnings"="none")
library(sf)
library(raster)
library(mapview)


## Import raster elevation data Ordnance Survey projection
pan50m <- raster("gis_data/pan50m.tif")
plot(pan50m)

# the default colours are the wrong way round, so we can
# use the terrain.colors option to set low elevation to green, high to brown, 
# with 30 colour categories
plot(pan50m, col=terrain.colors(30))

#OS projection system, convert lat and long first
ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude
pan50m_ll <- projectRaster(pan50m, crs=ll_crs)
mapview(pan50m_ll)

# create a hillshade map from elevation data, which can also provide an attractive
#visualisation that highlights the structure of the elevation as if caught by sunlight.:

hs = hillShade(slope = terrain(pan50m, "slope"), aspect = terrain(pan50m, "aspect"))
plot(hs, col = gray(0:100 / 100), legend = FALSE)
# overlay with DEM
plot(pan50m, col = terrain.colors(25), alpha = 0.5, add = TRUE)


##Creation of contours from raster DTM
#create a contour map from your dem using rasterToContour.
#pushed the output through st_as_sf() to convert it from sp format to sf 
#vector format, as the latter is becoming more widely used in R.

pan_contours <- rasterToContour(pan50m) %>% st_as_sf()
plot(pan50m)
plot(pan_contours, add=TRUE)

#### Add DTM-derived information to site data
##Wind turbines dataset

wind_turbines <- st_read("gis_data/wind_turbines.shp")
print(wind_turbines)


plot(pan50m)
plot(wind_turbines["WF_Name"], add=TRUE)

##Calculate slope and aspect
dem_slope  <- terrain(pan50m, unit="degrees") # defaults to slope
dem_aspect <- terrain(pan50m, opt="aspect", unit="degrees")
plot(dem_slope)
plot(dem_aspect)

#Add slope and aspect values to wind turbines attributes
wind_turbines$slope <- extract(dem_slope, wind_turbines)
wind_turbines$aspect <- extract(dem_slope, wind_turbines)

####Create a viewshed
##Viewshed concept

source("LOS.R")
#to your script several additional functions will be created, the key one being viewshed()

##Using the viewshed function: separate windfarms
#First display the windfarms (as latitude-longitude) in mapview and identify
#the name of a wind-turbine roughly in the middle of the western windfarm:

# Convert to latitude-longitude; EPSG code 4326
wind_turbines_ll <- st_transform(wind_turbines, 4326)
mapview(wind_turbines_ll)

#viewshed western area
west_windfarm <- dplyr::filter(wind_turbines, Turb_ID == "CC7")

# Change to coarser 500m elevation map for speed
pan500m <- aggregate(pan50m, fact=5) # fact=5 is the number of cells aggregated together

# Extract just the geometry for a single mast, and pass to viewshed function.
# Adding a 5km maximum radius
# Takes 1 to 2 minutes to run viewshed depending on your PC
west_windfarm_geom <- st_geometry(west_windfarm)[[1]]
west_viewshed <- viewshed(dem=pan500m, windfarm=west_windfarm_geom,
                          h1=1.5, h2=49, radius=5000)


# Display results
plot(pan500m)
plot(west_viewshed, add=TRUE, legend=FALSE, col="red")

#Viewshed for eastern area
# Get the OM7 turbine
east_windfarm <- dplyr::filter(wind_turbines, Turb_ID == "OM7")

# Extract geometry and calculate viewshed
east_windfarm_geom <- st_geometry(east_windfarm)[[1]]
east_viewshed <- viewshed(dem=pan500m, windfarm=east_windfarm_geom,
                          h1=1.5, h2=54, radius=5000)

# Display results
plot(pan500m)
plot(west_viewshed, add=TRUE, legend=FALSE, col="red")
plot(east_viewshed, add=TRUE, legend=FALSE, col="blue")
#plots viewsheds for both east and west areas

#Merge East and West viewsheds
west_viewshed <- extend(west_viewshed, pan500m) # could use 50m
east_viewshed <- extend(east_viewshed, pan500m)
both_viewshed <- merge(west_viewshed, east_viewshed)
plot(pan500m, col=terrain.colors(25))
plot(both_viewshed, legend=FALSE, add=TRUE, col="red")

##Which settlements can see the viewshed?

settlements <- st_read("gis_data/settlements.shp") #read in data

# Reclassify viewshed map into one class
#Should you ever need to reclassify a raster map in R, use the reclassify function, 
#which takes a map, and a simple matrix of reclassification codes or ranges.

#Convert viewshed map into polygon map
both_viewshed_poly <- rasterToPolygons(both_viewshed) %>% st_as_sf()
plot(both_viewshed_poly)
print(both_viewshed_poly, n=5)

#Dissolve viewshed polygons into a single polygon
# If you are able to install rgeos then simply use:
library(rgeos)

both_viewshed_poly <- rasterToPolygons(both_viewshed, dissolve=TRUE) %>% st_as_sf()
plot(both_viewshed_poly)
print(both_viewshed_poly, n=5)

#An alternative is to group and summarise based on the layer column, 
#using functions from dplyr which you probably already have installed:

#both_viewshed_poly <- both_viewshed_poly %>% 
  #dplyr::group_by(layer) %>%
 # dplyr::summarize()
#plot(both_viewshed_poly)

#print(both_viewshed_poly, n=5)

#Clip the settlements map with the dissolved viewshed map
settlements_my_viewshed <- st_intersection(settlements, both_viewshed_poly)

both_viewshed_poly <- st_transform(both_viewshed_poly, 27700)
settlements_my_viewshed <- st_intersection(settlements, both_viewshed_poly)

plot(pan500m)
plot(settlements_my_viewshed, add=TRUE, legend=FALSE, col="red")

print(settlements_my_viewshed)


settlements_my_viewshed<- st_transform(settlements_my_viewshed, 4326)
mapview(settlements_my_viewshed)
