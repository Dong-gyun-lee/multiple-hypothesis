library(dplyr)
library(data.table)

count_before_spring=fread('C:\\Users\\uos\\Desktop\\대학원\\bike\\베이지안\\count\\count_before_spring.csv')
count_after_spring=fread('C:\\Users\\uos\\Desktop\\대학원\\bike\\베이지안\\count\\count_after_spring.csv')
count_before_summer=fread('C:\\Users\\uos\\Desktop\\대학원\\bike\\베이지안\\count\\count_before_summer.csv')
count_after_summer=fread('C:\\Users\\uos\\Desktop\\대학원\\bike\\베이지안\\count\\count_after_summer.csv')
count_before_fall=fread('C:\\Users\\uos\\Desktop\\대학원\\bike\\베이지안\\count\\count_before_fall.csv')
count_after_fall=fread('C:\\Users\\uos\\Desktop\\대학원\\bike\\베이지안\\count\\count_after_fall.csv')



spring_d = cbind(count_before_spring[,c(1:3)],count_after_spring[,c(4:95)]-count_before_spring[,c(4:95)])
summer_d = cbind(count_before_summer[,c(1:3)],count_after_summer[,c(4:95)]-count_before_summer[,c(4:95)])
fall_d = cbind(count_before_fall[,c(1:3)],count_after_fall[,c(4:93)]-count_before_fall[,c(4:93)])


plot(apply(spring_d[,4:95],2,mean),type='l')

plot(apply(summer_d[,4:95],2,mean),type='l')
plot(apply(fall_d[,4:93],2,mean),type='l')

###
hist(apply(spring_d[,4:95],2,mean),xlab='평균 이용량',main='전체 일별 평균 이용량')

QQplot<-function(x){
  Quantile<-(c(1:length(x))-3/8)/(length(x)+1/4)
  z<-qnorm(Quantile,0,1)
  x<-sort(x)
  return(plot(z,x,main="QQplot"))
}
QQplot(apply(spring_d[,4:95],2,mean))

hist(as.numeric(spring_d[3,c(4:95)]),xlab='이용량',main='첫 번째 대여소 이용량')
hist(as.numeric(spring_d[5,c(4:95)]),xlab='이용량',main='두 번째 대여소 이용량')


a = as.numeric(spring_d[1,c(4:95)])
hist(a)


hist(apply(summer_d[,4:95],2,mean))
hist(apply(fall_d[,4:93],2,mean))

###
hist(apply(spring_d[,4:95],1,mean))
mean(apply(spring_d[,4:95],1,mean))
sd(apply(spring_d[,4:95],1,mean))^2


hist(apply(spring_d[,4:95],1,mean),breaks=10)

###
spring_parameter = count_before_spring[,c(1:3)]
spring_parameter$mu=apply(spring_d[,4:95],1,mean)
spring_parameter$variance=apply(spring_d[,4:95],1,sd)^2
spring_parameter$sd=apply(spring_d[,4:95],1,sd)

spring_parameter = spring_parameter %>% mutate(post_mu = (92*mu/variance-12.63978/154.2494)/(92/variance+1/154.2494),post_variance=(92/variance+1/154.2494)^-1)

spring_parameter = spring_parameter %>% mutate(post_mu = (92*mu/variance-12.63978/154.2494)/(92/variance+1/154.2494),post_variance=(92/variance+1/154.2494)^-1)


spring_parameter = spring_parameter %>% mutate(p1=pnorm((-12.63978-post_mu)/sqrt(post_variance)),
                                               p2 = pnorm((0-post_mu)/sqrt(post_variance))-pnorm((-12.63978-post_mu)/sqrt(post_variance)),
                                               p3 = 1-pnorm((0-post_mu)/sqrt(post_variance)),
                                               sum = p1+p2+p3)


spring_parameter = spring_parameter %>% mutate(p1=pnorm((-3-post_mu)/sqrt(post_variance)),
                            p2 = pnorm((0-post_mu)/sqrt(post_variance))-pnorm((-3-post_mu)/sqrt(post_variance)),
                            p3 = pnorm((3-post_mu)/sqrt(post_variance))-pnorm((0-post_mu)/sqrt(post_variance)),
                            p4 = 1-pnorm((3-post_mu)/sqrt(post_variance)),
                            sum = p1+p2+p3+p4)

qnorm(0.4,0,12)
qnorm(0.6,0,12)

ch = c()
for (i in 1:nrow(spring_parameter)){
  ch[i] = which.max(c(spring_parameter[i,]$p1,spring_parameter[i,]$p2,spring_parameter[i,]$p3))
}
ch

spring_parameter$ch = ch

spring_parameter[,c(1,2,3,9:14)]

fwrite(spring_parameter,'C:\\Users\\uos\\Desktop\\대학원\\bike\\베이지안\\count\\spring_parameter2.csv')



sqrt(mean(spring_parameter$post_variance))



#### 시각화 ####

                 
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

areas <- readOGR('C:\\Users\\uos\\Desktop\\대학원\\bike\\베이지안\\map\\TL_SCCO_SIG_W.shp', encoding = 'utf-8')

p_spring <- SpatialPointsDataFrame(spring_parameter[,c(3,2)], spring_parameter[,4:ncol(spring_parameter)], proj4string = CRS("+init=EPSG:4326"))

tmap_mode("plot")
bayes_spring <- tm_shape(areas) + 
  tm_borders() +
  tmap_options(check.and.fix = TRUE) + 
  tm_shape(p_spring) + 
  tm_dots(col = 'ch', palette = 'viridis', breaks=c(0.5,1.5,2.5,3.5),
          scale = 2.5,  title = 'bayes_spring') +
  tmap_options(check.and.fix = TRUE) + 
  tm_compass()+
  tm_layout(legend.text.size = 0.5,
            legend.title.size = 0.5,
            frame = TRUE)
tmap_save(tm = bayes_spring, filename = 'C:\\Users\\uos\\Desktop\\대학원\\bike\\베이지안\\결과\\bayes_spring2.png')
