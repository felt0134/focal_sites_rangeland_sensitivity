hot_deserts_data<-subset(rangeland_npp_covariates_deviations_1,region.x=='hot_deserts')
head(hot_deserts_data)

#get slopes for water year precip
slope_spatial_hot_deserts <- hot_deserts_data %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~mm.x, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_hot_deserts)
hot_deserts_coef_only<- slope_spatial_hot_deserts[ -c(3) ] #isolate coefficient so only slope is graphed
head(hot_deserts_coef_only)

#sensitivity

#change to raster
hot_deserts_npp_mm_slope_raster<-rasterFromXYZ(hot_deserts_coef_only)
plot(hot_deserts_npp_mm_slope_raster)
us<-getData("GADM", country='USA', level=1,download=TRUE) #download onto hardrive
us<-readRDS('/Users/andrewfelton/Desktop/USU/GADM_2.8_USA_adm1.rds') #take directly from hardrive 
states_new_mexico <- us[us$NAME_1 %in% c('New Mexico'),]
plot(states_new_mexico)
counties <- getData(name="GADM", country="USA", level=2)
nm_counties <- subset(counties, NAME_1=="New Mexico")
plot(nm_counties)
break_mm_hot_deserts_npp_mm_slope<-quantile(hot_deserts_coef_only$coef,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)

#slope
nm_npp_sensitivity<-crop(hot_deserts_npp_mm_slope_raster,extent(nm_counties))
plot(nm_npp_sensitivity)

npp_mean_allsites
plot(us)

nm.sens.plot<-spplot(nm_npp_sensitivity,#scales = list(draw = TRUE),
                  at=break_mm_hot_deserts_npp_mm_slope,
                  asp=0.1,
                  col.regions =
                    rev(heat_hcl(length(break_mm_hot_deserts_npp_mm_slope)-1)),
                  main="NPP sensitivity to annual rainfall (grams per mm)") +
  latticeExtra::layer(sp.polygons(nm_counties, lwd = 1))

#San juan county sensitivity
nm_counties_san_juan <- subset(nm_counties, NAME_2=="San Juan")
plot(nm_counties_san_juan)
san_juan_npp_sensitivity<-crop(nm_npp_sensitivity,extent(nm_counties_san_juan ))
san_juan_npp_sensitivity_mask<-mask(san_juan_npp_sensitivity,nm_counties_san_juan)
plot(san_juan_npp_sensitivity)

nm.san.juan.sens.plot<-spplot(san_juan_npp_sensitivity_mask,#scales = list(draw = TRUE),
                     at=break_mm_hot_deserts_npp_mm_slope,
                     asp=0.1,
                     col.regions =
                       rev(heat_hcl(length(break_mm_hot_deserts_npp_mm_slope)-1)),
                     main="San Juan, NM NPP sensitivity to annual rainfall (grams per mm)") +
  latticeExtra::layer(sp.polygons(nm_counties_san_juan , lwd = 1))


#Hidalgo county sensitivity
nm_counties_hidalgo <- subset(nm_counties, NAME_2=="Hidalgo")
plot(nm_counties_hidalgo)
hidalgo_npp_sensitivity<-crop(nm_npp_sensitivity,extent(nm_counties_hidalgo ))
hidalgo_npp_sensitivity_mask<-mask(hidalgo_npp_sensitivity,nm_counties_hidalgo)
plot(hidalgo_npp_sensitivity_mask)

nm.hidalgo.sens.plot<-spplot(hidalgo_npp_sensitivity_mask,#scales = list(draw = TRUE),
                              at=break_mm_hot_deserts_npp_mm_slope,
                              asp=0.1,
                              col.regions =
                                rev(heat_hcl(length(break_mm_hot_deserts_npp_mm_slope)-1)),
                              main="Hidalgo, NM NPP sensitivity to annual rainfall (grams per mm)") +
  latticeExtra::layer(sp.polygons(nm_counties_hidalgo , lwd = 1))


#combine hidalgo and san juan datasets

#Turn into dataframe

#San Juan
san_juan_sens_df<-rasterToPoints(san_juan_npp_sensitivity_mask)
head(san_juan_sens_df)
san_juan_sens_df_2<-data.frame(san_juan_sens_df)
head(hidalgo_sens_df_2)
san_juan_sens_df_2$county<-'san_juan'
summary(san_juan_sens_df_2)

#hidalgo
hidalgo_sens_df<-rasterToPoints(hidalgo_npp_sensitivity_mask)
head(hidalgo_sens_df)
hidalgo_sens_df_2<-data.frame(hidalgo_sens_df)
head(hidalgo_sens_df_2)
hidalgo_sens_df_2$county<-'hidalgo'
summary(hidalgo_sens_df_2)

#combine the two datasets
san_juan_hidalgo_sens<-rbind(san_juan_sens_df_2,hidalgo_sens_df_2)
head(san_juan_hidalgo_sens)
summary(san_juan_hidalgo_sens)
#make nice plot of histograms of slopes
ggplot(san_juan_hidalgo_sens ,aes(x=coef,fill=county)) +
  geom_histogram(bins=50,color='black') +
  #geom_density(color='black',alpha=0.75) +
  scale_fill_manual(values=c('hidalgo'='red','san_juan'='lightblue'),
                    labels=c('hidalgo'='Hidalgo','san_juan'='San Juan')) +
  xlab(bquote('NPP Sensitivity ('*g/m^2/mm*')')) +
  ylab('Count') +
  #scale_x_continuous(expand = c(0,0),limits=c(0,0.85)) +
  #scale_y_continuous(expand = c(0,0),limits=c(0,400)) +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    legend.position = c(0.72,0.75),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
