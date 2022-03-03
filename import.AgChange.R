library(tidyverse)
require('rgdal')
library(sf)
library(spdep)
library(mgcv)

polygons = readOGR(dsn='./shapefiles/US_counties_2012_geoid.shp',layer='US_counties_2012_geoid',stringsAsFactors=F,verbose=F)
con = file('./AgCensus_MasterDataFrame.txt','r')
ag1 = readLines(con)
close(con)
ag.data = t(apply(array(ag1),1,function(x){strsplit(x,'\t')[[1]]}))
colnames(ag.data) = ag.data[1,]
ag.data = ag.data[-1,]
counties = polygons
counties@data = data.frame(ag.data[match(polygons@data[,1],ag.data[,grep('FIPS',colnames(ag.data))]),],stringsAsFactors=F)
for (jz in 1:475){
	add.num = as.numeric(counties@data[,jz])
	counties@data[,jz] = add.num
}
print(noquote('Ag Census data successfully imported!'))
print(noquote(paste('Number of counties:',nrow(counties@data),sep=' ')))
print(noquote(paste('Number of variables:',ncol(counties@data),sep=' ')))


counties2 <- counties %>% st_as_sf()

counties2 %>%
  group_by(FIPS) %>%
  summarize(n=n()) %>%
  filter(n>1)

counties3 <- counties2 %>%
  pivot_longer(
    cols=c(1:475),
    names_to = c(".value", "Year"),
    names_pattern = "(^.{0,3})(.{4}$)"
  ) %>% select(Year,geometry,FIPS,Crn)

counties3_sp <- as(counties3, 'Spatial')
counties2_sp <- as(counties2, 'Spatial')

crn <- counties3 

nb <- poly2nb(counties2_sp, row.names = counties2$FIPS)
names(nb) <- attr(nb, "region.id")

counties4 <- counties3 %>% mutate(FIPS = factor(FIPS), Year = as.integer(Year)) %>%
  filter(Year == 2017)



counties1year <- counties2 %>% st_as_sf() %>% filter(year == 2017)

names(counties2)




m1 <- bam(Crn ~ s(FIPS, bs = 'mrf', xt = list(nb = nb)), # define MRF smooth
          data = counties4,discrete=TRUE,nthreads=23,
          family = betar())

## rank 300 MRF
m2 <- bam(Crn ~ s(FIPS, bs = 'mrf', k = 300, xt = list(nb = nb)),
          data = counties4,discrete=TRUE,nthreads=23,
          family = betar())
## rank 30 MRF
m3 <- bam(Crn ~ s(FIPS, bs = 'mrf', k = 30, xt = list(nb = nb)),
          data = counties4,discrete=TRUE,nthreads=23,
          family = betar())



summary(m2)


model <- bam(Crn ~ te(FIPS, bs = c("mrf"),xt = list(FIPS = list(nb = nb))),
                                  family=betar(), data=counties4,discrete=TRUE,nthreads=23)

summary(model)
plot(model)


counties4 <- transform(counties4,
                mrfFull     = predict(model, type = 'response'))

years <- c(1840,1890,1940,1992,2017)
unique(factor(counties4$Year))
counties4 %>%
  filter(Year %in% years) %>%
ggplot() +
  geom_sf(aes(geometry = geometry,fill=mrfFull),size = 0.05) +
  viridis::scale_fill_viridis() +
  facet_wrap(~Year,ncol = 2)
