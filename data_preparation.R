###### Skript for data preparation

### set data_DIR to directory where the data is stored
data_DIR <- "C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/raw_data"
setwd(data_DIR)

library(lubridate)
library(stringr)
library(sf)
library(raster)

############## this part can be (hopefully) deleted soon ##############
#### for now we receive the data in 7 separate .csv files named out_0.csv ... out_6.csv
#### create main data file
source_csvs<-list.files(".", pattern="out*")

main_dat<-read.csv(source_csvs[1], dec=".")

for (i in 2:length(source_csvs))
  {
  tmp_file<-read.csv(source_csvs[i], dec=".")
  main_dat<-rbind(main_dat,tmp_file)
  rm(tmp_file)
  }

#### create proper date column and calculate day of year and month of year
main_dat$date <- as.Date(main_dat$datum,"%Y-%m-%d")
main_dat$day_of_year <- yday(main_dat$date)
main_dat$month_of_year<-month(main_dat$date)

#### create proper coordinates
main_dat$geometry<-gsub("[()]","",main_dat$geometry)
main_dat$geometry<-gsub("POINT ","",main_dat$geometry)
tmp_test<-str_split_fixed(main_dat$geometry, " ", 2)
tmp_test<-as.data.frame(tmp_test)
names(tmp_test)<-c("X_coord_EPSG_25832", "Y_coord_EPSG_25832")
main_dat<-cbind(main_dat,tmp_test)
rm(tmp_test)

### make the dataframe to spatialpointsdataframe

main_dat$X_coord_EPSG_25832<-as.numeric(main_dat$X_coord_EPSG_25832) ### not sure if this step causes problems
main_dat$Y_coord_EPSG_25832<-as.numeric(main_dat$Y_coord_EPSG_25832)

#### there are 6 "Inf" Values in the data frame --> remove them
main_dat<-subset(main_dat,!main_dat$X_coord_EPSG_25832=="Inf")
### copy dataframe
spatial_main_dat<-main_dat
### define coordinate columns
coordinates(spatial_main_dat)<-~X_coord_EPSG_25832+Y_coord_EPSG_25832
#### set crs to EPSG_25832
crs(spatial_main_dat)= "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs +type=crs"

### get map of Germany
Germany <- getData("GADM",country="DEU",level=0)
### transform and plot map of Germany
Germany_trnsfrmd = spTransform(Germany,"+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs +type=crs")
#plot(Germany_trnsfrmd,col = 'red')

#### use small sample data set to test if coordinates are transformed correctly
tmp_main<-spatial_main_dat[sample(nrow(main_dat), 10000), ]
#plot(tmp_main, add=T)

#### do a spatial overly to see how many points do not fall within the border of Germany
tmp_spatial_test<-over(spatial_main_dat,Germany_trnsfrmd)
### check how many points are not within Germany
sum(is.na(tmp_spatial_test$GID_0))   ### 39707

tmp_spatial_test<-tmp_spatial_test[c(1)]
names(tmp_spatial_test)<-c("location")
main_dat<-cbind(main_dat,tmp_spatial_test)

unique(main_dat$location)
points_outside_Ger<-main_dat[is.na(main_dat$location),]


coordinates(points_outside_Ger)<-~X_coord_EPSG_25832+Y_coord_EPSG_25832
#### set crs to EPSG_25832
crs(points_outside_Ger)= "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs +type=crs"
#plot(points_outside_Ger, add=T)

tmp_points_outside<-as.data.frame(points_outside_Ger)
#write.csv(tmp_points_outside,"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/temporal_results/points_outside_GER.csv", row.names=FALSE)


#### check some data properties
names(main_dat)
length(unique(main_dat$idMessstelle__messstellenCode))    #7111
length(unique(main_dat$idMessstelle__messstellenName))    #11704

#### check distribution of sampling points across Germany

tmp_dat<-main_dat[sample(nrow(main_dat), 100000), ]

max(tmp_dat$Y_coord_EPSG_25832[tmp_dat$Y_coord_EPSG_25832 != max(tmp_dat$Y_coord_EPSG_25832)])
tmp_dat<-tmp_dat[tmp_dat$Y_coord_EPSG_25832<6083998,]

min(tmp_dat$Y_coord_EPSG_25832[tmp_dat$Y_coord_EPSG_25832 != min(tmp_dat$Y_coord_EPSG_25832)])

max(tmp_dat$idMessstelle__xKoordinate)
tmp_dat<-tmp_dat[tmp_dat$idMessstelle__xKoordinate<46115364,]

plot(tmp_dat$X_coord_EPSG_25832,tmp_dat$Y_coord_EPSG_25832)


length(unique(main_dat$idMessstelle__wasserkoerperCode__wasserkoerperCode))




plot(tmp_dat$idMessstelle__xKoordinate,tmp_dat$idMessstelle__yKoordinate)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84")
