#Geospatial Data

library(sf)
library(raster)
library(tidyverse)
library(spData)
library(spDataLarge)
library(tmap)
library(leaflet)
library(rgdal)
library(janitor)
library(mapproj)
library(broom)
library(gstat)
library(automap)

#global
cur_yr = 2021
year <- 2021 # most recent year of data
map_path <- paste0('maps/', year) # folder to hold all maps for a given year
dir.create(map_path) # creates YEAR subdirectory inside maps folder
output_path <- paste0('output/', year) # output and results
dir.create(output_path)


#Import shapefiles
shapefile("data/shapefiles/cf_SE_stats_area.shp") -> stat_area
shapefile("data/shapefiles/p4_se_alaska.shp") -> se_alaska
shapefile("data/shapefiles/jnu_strata/JuneauNAD83.shp") -> jnu_strata
shapefile("data/shapefiles/barlow_strata/BarlownNAD83.shp") -> barlow_strata

#Read shapefile for info
st_read("data/shapefiles/cf_SE_stats_area.shp")
st_read("data/shapefiles/p4_se_alaska.shp")
st_read("data/shapefiles/jnu_strata/JuneauNAD83.shp")
st_read("data/shapefiles/barlow_strata/BarlownNAD83.shp")

#Shapefile must be converted to be used with tmap and dplyr functions
stat_area_sf <-st_as_sf(stat_area)
se_alaska_sf <- st_as_sf(se_alaska)
jnu_strata_sf <-st_as_sf(jnu_strata)
barlow_strata_sf <-st_as_sf(barlow_strata)

#Check coordinate system
st_crs(stat_area_sf)
st_crs(se_alaska_sf)
st_crs(jnu_strata_sf)
st_crs(barlow_strata_sf)

#Write new shapefile
st_write(stat_area_sf, "data/shapefiles", 
         driver = "ESRI Shapefile",
         delete_layer = TRUE) #add delete_layer = TRUE to overwrite shapefile

st_write(se_alaska_sf, "data/shapefiles",
         driver = "ESRI Shapefile",
         delete_layer = TRUE)

st_write(jnu_strata_sf, "data/shapefiles/jnu_strata",
         driver = "ESRI Shapefile",
         delete_layer = TRUE)


#Import survey data
read.csv("data/survey/rkc survey.csv") %>%
  clean_names() %>% 
  filter(species == "Red king crab") -> rkc_survey


#Clean up and summarise data
rkc_survey %>%
  filter(longitude_decimal_degrees != 0,
         latitude_decimal_degrees != 0,
         species == "Red king crab" ) %>%
  group_by(i_year, location, pot_no, 
           latitude_decimal_degrees, 
           longitude_decimal_degrees) %>%
  summarise(total_rkc = sum(number_of_specimens))-> rkc_survey_summary

#Convert dataframe into a spatial dataframe (sf)

rkc_survey_summary_sf <- st_as_sf(rkc_survey_summary, 
                                  coords = c("longitude_decimal_degrees",
                                             "latitude_decimal_degrees"))
str(rkc_survey_summary_sf)

#Set coordinate system for spatial data frame
st_crs(rkc_survey_summary_sf) <- 4326

st_crs(rkc_survey_summary_sf)

#Save as new shapefile
st_write(rkc_survey_summary_sf, "data/shapefiles", 
         driver = "ESRI Shapefile",
         delete_layer = TRUE) #add delete_layer = TRUE to overwrite shapefile


#Create bounding box for survey areas

#JNU Area
jnu_bb <- st_bbox(stat_area_sf %>% 
                    filter(STAT_AREA %in% c(11155, 11150, 11143, 11141, 11140 )))

#Excursion Inlet
ex_bb <- st_bbox(stat_area_sf %>%
                   filter(STAT_AREA %in% c(11480)))

#Deadman's Reach
dm_bb <- st_bbox(stat_area_sf %>%
                   filter(STAT_AREA %in% c(11356, 11355)))

#Pybus Bay
py_bb <- st_bbox(stat_area_sf %>%
                   filter(STAT_AREA %in% c(11022)))

#Gambier Bay
gm_bb <-st_bbox(stat_area_sf %>%
                  filter(STAT_AREA %in% c(11023)))

#Seymour Canal
sy_bb <-st_bbox(stat_area_sf %>%
                  filter(STAT_AREA %in% c(11115, 11114)))

#Lynn Sisters
ls_bb <-st_bbox(stat_area_sf %>%
                  filter(STAT_AREA %in% c(11510)))

#Create maps of each area 
#use tm_facets to conducte a facet wrap by variable of interest
tm_shape(stat_area_sf, bbox = jnu_bb) +
  tm_graticules(lines = FALSE) +
  tm_fill(col = "gray") +
  tm_borders() +
  tm_shape(jnu_strata_sf) +
  tm_fill(col = "GRIDCODE",
          palette = "YlOrBr",
          alpha = 0.7,
          title = "Density Strata") + 
  tm_shape(barlow_strata_sf) +
  tm_fill(col = "GRIDCODE",
          palette = "YlOrBr",
          alpha = 0.7,
          title = "Density Strata") +
  tm_shape(rkc_survey_summary_sf) +
  tm_bubbles("total_rkc",
             col = "dodgerblue1",
             alpha = 0.7) +
  tm_facets(by = "i_year",
            nrow = 1) -> jnu_area

jnu_area

tmap_save(jnu_area, paste0(map_path, '/jnu_rkc_bubble.png'), width = 10, height = 8, units = "in", dpi = 400)


tmap_leaflet(jnu_area)

tm_shape(stat_area_sf, bbox = ex_bb) +
  tm_graticules(lines = FALSE) +
  tm_fill(col = "gray") +
  tm_borders() +
  tm_shape(rkc_survey_summary_sf) +
  tm_bubbles("total_rkc",
             col = "total_rkc",
             alpha = 0.7) 

tm_shape(stat_area_sf, bbox = dm_bb) +
  tm_graticules(lines = FALSE) +
  tm_fill(col = "gray") +
  tm_borders() +
  tm_shape(rkc_survey_summary_sf) +
  tm_bubbles("total_rkc",
             col = "total_rkc",
             alpha = 0.7) 

tm_shape(stat_area_sf, bbox = py_bb) +
  tm_graticules(lines = FALSE) +
  tm_fill(col = "gray") +
  tm_borders() +
  tm_shape(rkc_survey_summary_sf) +
  tm_bubbles("total_rkc",
             col = "total_rkc",
             alpha = 0.7) 

tm_shape(stat_area_sf, bbox = gm_bb) +
  tm_graticules(lines = FALSE) +
  tm_fill(col = "gray") +
  tm_borders() +
  tm_shape(rkc_survey_summary_sf) +
  tm_bubbles("total_rkc",
             col = "total_rkc",
             alpha = 0.7) 

tm_shape(stat_area_sf, bbox = sy_bb) +
  tm_graticules(lines = FALSE) +
  tm_fill(col = "gray") +
  tm_borders() +
  tm_shape(rkc_survey_summary_sf) +
  tm_bubbles("total_rkc",
             col = "total_rkc",
             alpha = 0.7,
             scale = 3) 

tm_shape(stat_area_sf, bbox = ls_bb) +
  tm_graticules(lines = FALSE) +
  tm_fill(col = "gray") +
  tm_borders() +
  tm_shape(rkc_survey_summary_sf) +
  tm_bubbles("total_rkc",
             col = "total_rkc",
             alpha = 0.7,
             scale = 3) 


#ggplot2 approach to maps

#JNU Area
ggplot() + 
  geom_sf(data = se_alaska_sf,
          fill = "antiquewhite") +
  geom_sf(data = stat_area_sf,
          fill = "white") + 
  geom_sf(data = rkc_survey_summary_sf,
          aes(size = total_rkc,
              color = total_rkc),
          alpha = 0.6) +
  coord_sf(crs = st_crs(4326),
           xlim = c(-135, -134.2),
           ylim = c(58.16, 58.6)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~i_year, ncol = 2) -> jnu_rkc_map

ggsave(paste0(map_path, '/jnu_rkc_bubble.png'),
       width = 8, height = 10, units = "in", dpi = 200)

#Variogram model for krigging 
#(https://towardsdatascience.com/building-kriging-models-in-r-b94d7c9750d8
rkc_survey %>% 
  filter(location == "Juneau",
         longitude_decimal_degrees != 0,
         latitude_decimal_degrees != 0,
         species == "Red king crab" ) %>%
  group_by(i_year, location, pot_no, 
           latitude_decimal_degrees, 
           longitude_decimal_degrees) %>%
  summarise(total_rkc = sum(number_of_specimens)) -> jnu_survey


coordinates(jnu_survey) <- c("longitude_decimal_degrees",
                             "latitude_decimal_degrees")

proj4string(jnu_survey) <- CRS("+proj=longlat +datumWGS84")


lzn.vgm <- (variogram(total_rkc~1, jnu_survey))

print(lzn.vgm)

lzn.fit = fit.variogram(lzn.vgm, model = vgm("Ste"))

print(lzn.fit)

lzn.fit = fit.variogram(lzn.vgm, model = vgm(1140, "Ste", 3.3))

plot(lzn.vgm, lzn.fit)





v_mod_OK<-automap::autofitVariogram(total_rkc~1, as (jnu_survey_sf, "Spatial"))$var_model

plot(automap::autofitVariogram(total_rkc~1, as(jnu_survey_sf, "Spatial")))

jnu_grd <- makegrid(jnu_strata, n = 10000)
colnames(jnu_grd) <-c('longitude_decimal_degrees', 'latitude_decimal_degrees')

jnu_grd_pts <- SpatialPoints(coord = jnu_grd,
                             proj4string = CRS(proj4string(jnu_strata)))

jnu_grd_pts_in <- jnu_grd_pts[jnu_strata, ]

plot(jnu_grd_pts_in)

# Ordinary Kriging
OK <- krige(
  total_rkc~1,                       # Z is our variable and "~1" means "depends on mean"
  as(jnu_survey_sf, "Spatial"), # input data in {sp} format
  jnu_grd_pts_in,                # locations to interpolate at
  model = v_mod_OK           # the variogram model fitted above
)




jnu_grd_pts_in %>% as.data.frame %>%
  ggplot(aes(x = longtitude_decimal_degrees, y = latitude_decimal_degrees)) +
  geom_point()


#Krig data
jnu_grd_pts_in2 <- SpatialPoints(coord = jnu_grd_pts_in,
                                 proj4string = CRS(proj4string(jnu_survey)))

lzn.kriged <- autoKrige(total_rkc~1, jnu_survey_prj, jnu_grd_pts_in_prj, 
                        model = v_mod_OK) 

summary(lzn.kriged)

#Reproject data
jnu_survey_prj <- spTransform(jnu_survey, CRS("+proj=longlat +datum=WGS84 +nodefs"))
jnu_grd_pts_in_prj <- spTransform(jnu_grd_pts_in, CRS("+proj=longlat +datum=WGS84 +nodefs"))


lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x = longitude_decimal_degrees, y= latitude_decimal_degrees)) +
  geom_tile(aes(fill = var1.pred)) +
  coord_equal() +
  scale_fill_gradient(low = "yellow", high = "red")


autoKrige(total_rkc~1, jnu_grd_pts_in, juneau_survey)

data("jnu_survey.grid")

#Create grid
grd<- makegrid(jnu_strata, n = 10000)
colnames(grd) <- c("x1", "x2")

grd_pts <- SpatialPoints(
  coords = grd,
  proj4string = CRS(proj4string(jnu_strata))
)

grd_pts_in <- grd_pts[jnu_strata, ]


ggplot(as.data.frame(coordinates(grd_pts_in))) +
  geom_point(aes(x1, x2)) -> plot1

jnu_survey <- SpatialPoints(
  proj4string = CRS(proj4string(jnu_strata))
)

ggplot(as.data.frame(coordinates(jnu_survey))) +
  geom_point(aes(longitude_decimal_degrees, latitude_decimal_degrees)) -> plot2

plot1
plot2

#Krig data
lzn.kriged <- krige(total_rkc~1, jnu_survey, grd_pts_in, model = lzn.fit) 
