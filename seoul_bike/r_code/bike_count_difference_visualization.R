rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library(sp)
library(rgeos)
library(rgdal)
library(raster)
library(tmap)
library(units)
library(sf)
library(leaflet)

pvalues_spring = fread('D:\\bike\\대여소별 pvalue\\주중-계절별\\양측검정\\pvalues_spring_weekday.csv')
pvalues_summer = fread('D:\\bike\\대여소별 pvalue\\주중-계절별\\양측검정\\pvalues_summer_weekday.csv')
pvalues_fall = fread('D:\\bike\\대여소별 pvalue\\주중-계절별\\양측검정\\pvalues_fall_weekday.csv')
pvalues_winter = fread('D:\\bike\\대여소별 pvalue\\주중-계절별\\양측검정\\pvalues_winter_weekday.csv')

areas <- readOGR('D:\\bike\\shape\\map_image\\TL_SCCO_SIG_W.shp', encoding = 'utf-8')

#### Spring ####
p_spring <- SpatialPointsDataFrame(pvalues_spring[,c(3,2)], pvalues_spring[,4:ncol(pvalues_spring)], proj4string = CRS("+init=EPSG:4326"))

tmap_mode("plot")
r1_spring <- tm_shape(areas) + 
  tm_borders() +
  tmap_options(check.and.fix = TRUE) + 
  tm_shape(p_spring) + 
  tm_dots(col = 'p1', palette = 'viridis', breaks=c(0,0.05,1),
          scale = 2.5,  title = 'p1_spring') +
  tmap_options(check.and.fix = TRUE) + 
  tm_compass()+
  tm_layout(legend.text.size = 0.5,
            legend.title.size = 0.5,
            frame = TRUE)

r3_spring <-  tm_shape(areas) + 
  tm_borders() +
  tmap_options(check.and.fix = TRUE) + 
  tm_shape(p_spring) + 
  tm_dots(col = 'p3', palette = 'viridis', breaks=c(0,0.05,1),
          scale = 2.5,  title = 'p3_spring') +
  tmap_options(check.and.fix = TRUE) + 
  tm_compass()+
  tm_layout(legend.text.size = 0.5,
            legend.title.size = 0.5,
            frame = TRUE)

rb_p1_spring <-  tm_shape(areas) + 
  tm_borders() +
  tmap_options(check.and.fix = TRUE) + 
  tm_shape(p_spring) + 
  tm_dots(col = 'bonferroni_p1', palette = 'viridis', breaks=c(0,0.05,1),
          scale = 2.5,  title = 'bonferroni_p1_spring') +
  tmap_options(check.and.fix = TRUE) + 
  tm_compass()+
  tm_layout(legend.text.size = 0.5,
            legend.title.size = 0.5,
            frame = TRUE)

rc_p1_spring <-  tm_shape(areas) + 
  tm_borders() +
  tmap_options(check.and.fix = TRUE) + 
  tm_shape(p_spring) + 
  tm_dots(col = 'sidak_p1', palette = 'viridis', breaks=c(0,0.05,1),
          scale = 2.5,  title = 'sidak_p1_spring') +
  tmap_options(check.and.fix = TRUE) + 
  tm_compass()+
  tm_layout(legend.text.size = 0.5,
            legend.title.size = 0.5,
            frame = TRUE)

rbh_p1_spring <-  tm_shape(areas) + 
  tm_borders() +
  tmap_options(check.and.fix = TRUE) + 
  tm_shape(p_spring) + 
  tm_dots(col = 'bh_p1', palette = 'viridis', breaks=c(0,0.05,1),
          scale = 2.5,  title = 'bh_p1_spring') +
  tmap_options(check.and.fix = TRUE) + 
  tm_compass()+
  tm_layout(legend.text.size = 0.5,
            legend.title.size = 0.5,
            frame = TRUE)


rb_p3_spring <-  tm_shape(areas) + 
  tm_borders() +
  tmap_options(check.and.fix = TRUE) + 
  tm_shape(p_spring) + 
  tm_dots(col = 'bonferroni_p3', palette = 'viridis', breaks=c(0,0.05,1),
          scale = 2.5,  title = 'bonferroni_p3_spring') +
  tmap_options(check.and.fix = TRUE) + 
  tm_compass()+
  tm_layout(legend.text.size = 0.5,
            legend.title.size = 0.5,
            frame = TRUE)

rc_p3_spring <-  tm_shape(areas) + 
  tm_borders() +
  tmap_options(check.and.fix = TRUE) + 
  tm_shape(p_spring) + 
  tm_dots(col = 'sidak_p3', palette = 'viridis', breaks=c(0,0.05,1),
          scale = 2.5,  title = 'sidak_p3_spring') +
  tmap_options(check.and.fix = TRUE) + 
  tm_compass()+
  tm_layout(legend.text.size = 0.5,
            legend.title.size = 0.5,
            frame = TRUE)

rbh_p3_spring <-  tm_shape(areas) + 
  tm_borders() +
  tmap_options(check.and.fix = TRUE) + 
  tm_shape(p_spring) + 
  tm_dots(col = 'bh_p3', palette = 'viridis', breaks=c(0,0.05,1),
          scale = 2.5,  title = 'bh_p3_spring') +
  tmap_options(check.and.fix = TRUE) + 
  tm_compass()+
  tm_layout(legend.text.size = 0.5,
            legend.title.size = 0.5,
            frame = TRUE)

tmap_save(tm = r1_spring, filename = 'D:\\bike\\대여소별 pvalue\\주중-계절별\\주중-계절별(양측검정) plot\\spring\\r1_spring_weekday.png')
tmap_save(tm = r3_spring, filename = 'D:\\bike\\대여소별 pvalue\\주중-계절별\\주중-계절별(양측검정) plot\\spring\\r3_spring_weekday.png')
tmap_save(tm = rb_p1_spring, filename = 'D:\\bike\\대여소별 pvalue\\주중-계절별\\주중-계절별(양측검정) plot\\spring\\rb_p1_spring_weekday.png')
tmap_save(tm = rc_p1_spring, filename = 'D:\\bike\\대여소별 pvalue\\주중-계절별\\주중-계절별(양측검정) plot\\spring\\rc_p1_spring_weekday.png')
tmap_save(tm = rbh_p1_spring, filename = 'D:\\bike\\대여소별 pvalue\\주중-계절별\\주중-계절별(양측검정) plot\\spring\\rbh_p1_spring_weekday.png')
tmap_save(tm = rb_p3_spring, filename = 'D:\\bike\\대여소별 pvalue\\주중-계절별\\주중-계절별(양측검정) plot\\spring\\rb_p3_spring_weekday.png')
tmap_save(tm = rc_p3_spring, filename = 'D:\\bike\\대여소별 pvalue\\주중-계절별\\주중-계절별(양측검정) plot\\spring\\rc_p3_spring_weekday.png')
tmap_save(tm = rbh_p3_spring, filename = 'D:\\bike\\대여소별 pvalue\\주중-계절별\\주중-계절별(양측검정) plot\\spring\\rbh_p3_spring_weekday.png')
