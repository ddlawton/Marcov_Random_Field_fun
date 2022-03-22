library(tidyverse)
require('rgdal')
library(sf)
library(gganimate)
library(rnaturalearth)


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


counties2 <- counties %>% st_as_sf() %>% as_tibble()

names(counties2)

counties3 <- counties2 %>%
  pivot_longer(
    cols=c(1:475),
    names_to = c(".value", "Year"),
    names_pattern = "(^.{0,3})(.{4}$)"
  )  %>%
  pivot_longer(
    cols=c(12:36),
    names_to = "crop",
    values_to = "area"
  )

counties4 <- counties3 %>% mutate(crop = factor(crop))

crops <- levels(unique((factor(counties3$crop))))

cotton <- counties4 %>% filter(crop == "Ctn")
corn <- counties4 %>% filter(crop == "Crn")
soybeans <- counties4 %>% filter(crop == "Soy")
wheat <- counties4 %>% filter(crop == "Wht")


states <- ne_states(country="United States of America") %>% st_as_sf() %>% 
  as_tibble() %>% filter(name != "Hawaii")




crn_2020 <- corn %>%
  mutate(Year = 2020)  %>%
  ggplot() +
  scale_x_continuous(limits=c(-123,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(aes(geometry=geometry,fill=(area)),color=NA) +
  geom_sf(data=states,aes(geometry=geometry),fill="transparent",color="white") +
  viridis::scale_fill_viridis(na.value = "grey")+
  theme_void()  +
  labs(title = paste0("")) + 
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(legend.position = "bottom")

?ggsave

ggsave(crn_2020,file="output/crn_2020.png", width =  16, height = 8,units="in")


ctn <- cotton %>%
  mutate(Year = as.integer(Year))  %>%
  mutate(area = na_if(area, 0)) %>%
ggplot() +
  scale_x_continuous(limits=c(-123,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(aes(geometry=geometry,fill=(area)),color=NA) +
  geom_sf(data=states,aes(geometry=geometry),fill="transparent",color="white") +
  viridis::scale_fill_viridis(na.value = "grey")+
  theme_void()  +
  labs(title = paste0("Planted area of cotton during ",'{frame_time}')) + 
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(legend.position = "bottom") +
  transition_time(Year) 

crn <- corn %>%
  mutate(Year = as.integer(Year))  %>%
  mutate(area = na_if(area, 0)) %>%
  ggplot() +
  scale_x_continuous(limits=c(-123,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(aes(geometry=geometry,fill=(area)),color=NA) +
  geom_sf(data=states,aes(geometry=geometry),fill="transparent",color="white") +
  viridis::scale_fill_viridis(na.value = "grey")+
  theme_void()  +
  labs(title = paste0("Planted area of corn during ",'{frame_time}')) + 
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(legend.position = "bottom") +
  transition_time(Year) 

soy <- soybeans %>%
  mutate(Year = as.integer(Year))  %>%
  mutate(area = na_if(area, 0)) %>%
  ggplot() +
  scale_x_continuous(limits=c(-123,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(aes(geometry=geometry,fill=(area)),color=NA) +
  geom_sf(data=states,aes(geometry=geometry),fill="transparent",color="white") +
  viridis::scale_fill_viridis(na.value = "grey")+
  theme_void()  +
  labs(title = paste0("Planted area of soybeans during ",'{frame_time}')) + 
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(legend.position = "bottom") +
  transition_time(Year) 


wht <- wheat %>%
  mutate(Year = as.integer(Year))  %>%
  mutate(area = na_if(area, 0)) %>%
  ggplot() +
  scale_x_continuous(limits=c(-123,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(aes(geometry=geometry,fill=(area)),color=NA) +
  geom_sf(data=states,aes(geometry=geometry),fill="transparent",color="white") +
  viridis::scale_fill_viridis(na.value = "grey")+
  theme_void()  +
  labs(title = paste0("Planted area of wheat during ",'{frame_time}')) + 
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(legend.position = "bottom") +
  transition_time(Year) 

anim_save(file = "output/cotton.gif",ctn, width = 1200, height = 600)
anim_save(file = "output/corn.gif",crn, width = 1200, height = 600)
anim_save(file = "output/soybeans.gif",soy, width = 1200, height = 600)
anim_save(file = "output/wheat.gif",wht, width = 1200, height = 600)









plots <- list()
for (i in crops){
  dat <- counties4 %>% filter(crop==i)
  plots[[i]] <- dat %>%
    mutate(Year = as.integer(Year)) %>%
    ggplot() +
    geom_sf(aes_string(geometry="geometry",fill="area"),color=NA) +
    viridis::scale_fill_viridis(na.value = "transparent") +
    theme_void()  +
    labs(title = paste0(i," ",'Year: {frame_time}')) +
    transition_time(Year) 
}

for(i in 1:25){
  anim_save(file = paste("output/",i,".gif",sep=""),plots[[i]], width = 1200, height = 600)
}



plots[[1]]


?anim_save()
anim_save(file="output/Hmnchange.gif", plot)

