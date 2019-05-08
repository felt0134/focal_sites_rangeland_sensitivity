#miscelanous
california_data<-subset(rangeland_npp_covariates_deviations_1,region.x=='california_annuals')
head(california_data)

#sensitivity to annual rainfall
slope_spatial_cali <- california_data %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~mm.x, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_cali)
cali_coef_only<- slope_spatial_cali[ -c(3) ] #isolate coefficient so only slope is graphed
head(cali_coef_only)

#change to raster
cali_npp_mm_slope_raster<-rasterFromXYZ(cali_coef_only)
plot(cali_npp_mm_slope_raster)
us<-getData("GADM", country='USA', level=1,download=TRUE) #download onto hardrive
us<-readRDS('/Users/andrewfelton/Desktop/USU/GADM_2.8_USA_adm1.rds') #take directly from hardrive 
states_california <- us[us$NAME_1 %in% c('California'),]
plot(states_california)
counties <- getData(name="GADM", country="USA", level=2)
ca_counties <- subset(counties, NAME_1=="California")
plot(ca_counties)
break_mm_cali_npp_mm_slope<-quantile(cali_coef_only$coef,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)
npp_mean_allsites
plot(us)
library(sp)
library(colorspace)
library(latticeExtra)
test.plot<-spplot(cali_npp_mm_slope_raster,#scales = list(draw = TRUE),
                  at=break_mm_cali_npp_mm_slope,
                  asp=0.1,
                  col.regions =
                    rev(heat_hcl(length(break_mm_cali_npp_mm_slope)-1)),
                  main="Forage sensitivity to annual rainfall (grams per mm)") +
  latticeExtra::layer(sp.polygons(ca_counties, lwd = 0.75))

#mean forage productivity
library(dplyr)
mean_npp_site<- california_data[,c("x","y","npp.y")]
head(cali_mean_npp)
summary(cali_mean_npp)
#change to raster
cali_npp_mean_raster<-rasterFromXYZ(mean_npp_site)
plot(cali_npp_mean_raster)
break_mean_npp_cali_npp_mean_npp_slope<-quantile(mean_npp_site$npp.y,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)
npp_mean_allsites
plot(us)

spplot(cali_npp_mean_raster,#scales = list(draw = TRUE),
                  at=break_mean_npp_cali_npp_mean_npp_slope,
                  asp=0.1,
                  col.regions =
                    rev(terrain_hcl(length(break_mean_npp_cali_npp_mean_npp_slope)-1)),
                  main="Average forage production (grams per m^2)") +
  latticeExtra::layer(sp.polygons(ca_counties, lwd = 0.75))

#variation in forage production
CV <- function(x){
  cv.func<-(sd(x,na.rm = T)/mean(x,na.rm = T))*100
  
  return(cv.func)
}
var_npp_site<- aggregate(npp.x~x+y,CV,data=california_data)
head(var_npp_site)
summary(var_npp_site)
#change to raster
cali_npp_var_raster<-rasterFromXYZ(var_npp_site)
plot(cali_npp_var_raster)
break_var_npp_cali_npp_var_npp_slope<-quantile(var_npp_site$npp.x,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)
npp_mean_allsites
plot(us)

spplot(cali_npp_var_raster,#scales = list(draw = TRUE),
       at=break_var_npp_cali_npp_var_npp_slope,
       asp=0.1,
       col.regions =
         rev(heat_hcl(length(break_var_npp_cali_npp_var_npp_slope)-1)),
       main="Variability in forage production (%CV)") +
  latticeExtra::layer(sp.polygons(ca_counties, lwd = 0.75))


#jan-march soil moisture
slope_spatial_cali_soil_moisture <- california_data %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~jan_march_swc, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_cali_soil_moisture)
cali_coef_only_soil_moisture<- slope_spatial_cali_soil_moisture[ -c(3) ] #isolate coefficient so only slope is graphed
head(cali_coef_only_soil_moisture)

#change to raster
cali_npp_soil_moisture_slope_raster<-rasterFromXYZ(cali_coef_only_soil_moisture)
plot(cali_npp_soil_moisture_slope_raster)
us<-getData("GADM", country='USA', level=1,download=TRUE) #download onto hardrive
us<-readRDS('/Users/andrewfelton/Desktop/USU/GADM_2.8_USA_adm1.rds') #take directly from hardrive 
states_california <- us[us$NAME_1 %in% c('California'),]
plot(states_california)
counties <- getData(name="GADM", country="USA", level=2)
ca_counties <- subset(counties, NAME_1=="California")
plot(ca_counties)
break_soil_moisture_cali_npp_soil_moisture_slope<-quantile(cali_coef_only$coef,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)
npp_mean_allsites
plot(us)

test.plot<-spplot(cali_npp_soil_moisture_slope_raster,#scales = list(draw = TRUE),
                  at=break_soil_moisture_cali_npp_soil_moisture_slope,
                  asp=0.1,
                  col.regions =
                    rev(heat_hcl(length(break_soil_moisture_cali_npp_soil_moisture_slope)-1)),
                  main="Forage sensitivity to soil moisture (Jan-March)") +
  latticeExtra::layer(sp.polygons(ca_counties, lwd = 0.75))