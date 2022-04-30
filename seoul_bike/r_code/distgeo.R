rm(list=ls())

library(dplyr)
library(data.table)
library(geosphere)

pvalues_spring = fread('D:\\bike\\모비율 pvalue\\계절별\\proportion_pvalue_spring.csv')
pvalues_summer = fread('D:\\bike\\모비율 pvalue\\계절별\\proportion_pvalue_summer.csv')
pvalues_fall = fread('D:\\bike\\모비율 pvalue\\계절별\\proportion_pvalue_fall.csv')
pvalues_winter = fread('D:\\bike\\모비율 pvalue\\계절별\\proportion_pvalue_winter.csv')

pvalues_spring4 = pvalues_spring %>% select(c('place','latitude','longitude'))
pvalues_summer4 = pvalues_summer %>% select(c('place','latitude','longitude'))
pvalues_fall4 = pvalues_fall %>% select(c('place','latitude','longitude'))
pvalues_winter4 = pvalues_winter %>% select(c('place','latitude','longitude'))

dist_spring = data.frame(matrix(ncol=nrow(pvalues_spring), nrow=nrow(pvalues_spring)-1))
for (i in 1:nrow(pvalues_spring4)){
  remo = pvalues_spring4[-i,]
  sdist=c()
  for (j in 1:nrow(remo)){
    dist = distGeo(c(pvalues_spring4[i,]$longitude,pvalues_spring4[i,]$latitude),c(remo[j,]$longitude,remo[j,]$latitude))/1000
    sdist = c(sdist,dist)
  }
  dist_spring[,i]=sdist
}

fwrite(dist_spring,'D:\\bike\\모비율 pvalue\\LAWS\\dist_proportion_spring.csv')

dist_summer = data.frame(matrix(ncol=nrow(pvalues_summer), nrow=nrow(pvalues_summer)-1))
for (i in 1:nrow(pvalues_summer4)){
  remo = pvalues_summer4[-i,]
  sdist=c()
  for (j in 1:nrow(remo)){
    dist = distGeo(c(pvalues_summer4[i,]$longitude,pvalues_summer4[i,]$latitude),c(remo[j,]$longitude,remo[j,]$latitude))/1000
    sdist = c(sdist,dist)
  }
  dist_summer[,i]=sdist
}

fwrite(dist_summer,'D:\\bike\\모비율 pvalue\\LAWS\\dist_proportion_summer.csv')


dist_fall = data.frame(matrix(ncol=nrow(pvalues_fall), nrow = nrow(pvalues_fall)-1))
for (i in 1:nrow(pvalues_fall4)){
  remo = pvalues_fall4[-i,]
  sdist=c()
  for (j in 1:nrow(remo)){
    dist = distGeo(c(pvalues_fall4[i,]$longitude,pvalues_fall4[i,]$latitude),c(remo[j,]$longitude,remo[j,]$latitude))/1000
    sdist = c(sdist,dist)
  }
  dist_fall[,i]=sdist
}

fwrite(dist_fall,'D:\\bike\\모비율 pvalue\\LAWS\\dist_proportion_fall.csv')

dist_winter = data.frame(matrix(ncol=nrow(pvalues_winter), nrow = nrow(pvalues_winter)-1))
for (i in 1:nrow(pvalues_winter4)){
  remo = pvalues_winter4[-i,]
  sdist=c()
  for (j in 1:nrow(remo)){
    dist = distGeo(c(pvalues_winter4[i,]$longitude,pvalues_winter4[i,]$latitude),c(remo[j,]$longitude,remo[j,]$latitude))/1000
    sdist = c(sdist,dist)
  }
  dist_winter[,i]=sdist
}

fwrite(dist_winter,'D:\\bike\\모비율 pvalue\\LAWS\\dist_proportion_winter.csv')