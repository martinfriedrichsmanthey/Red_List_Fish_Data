###### Skript for data preparation

### set data_DIR to directory where the data is stored
data_DIR <- "C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/raw_data"
setwd(data_DIR)

library(lubridate)
library(stringr)
library(sf)
library(raster)
library(dplyr)

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


############## sum up events
#### create smaler data set only with neccessary columns
tmp_main<-subset(main_dat, select=c("id", "art__art", "individuenzahlGesamt", "idMessstelle__id", "date"))
### create new id
tmp_main$new_id<-paste0(tmp_main$idMessstelle__id,"_", tmp_main$date)
tmp_main$new_id<-gsub("-","_",tmp_main$new_id)
#### remove first row (was an X due to the csv writing process)
tmp_main<-tmp_main[-1]
### need for more specific ID
tmp_main$new_id_spec<-paste0(tmp_main$art__art,"_",tmp_main$new_id)
tmp_main$new_id_spec<-gsub(" ","_",tmp_main$new_id_spec)
##### just keep the rows needed
tmp_main<-tmp_main[c(2,6)]
#### sum them up
tmp_main<-tapply(tmp_main$individuenzahlGesamt, tmp_main$new_id_spec, FUN=sum)
tmp_main<-as.data.frame(tmp_main)
### make row names to column
tmp_main$new_id_spec <- row.names(tmp_main)

names(tmp_main)[1]<-"individuenzahlGesamt"
tmp_main<-distinct(tmp_main, new_id_spec, .keep_all = T)

### create same ID in main data set
main_dat$new_id_spec<-paste0(main_dat$idMessstelle__id,"_", main_dat$date)
main_dat$new_id_spec<-paste0(main_dat$art__art,"_",main_dat$new_id_spec)
main_dat$new_id_spec<-gsub("-","_",main_dat$new_id_spec)
main_dat$new_id_spec<-gsub(" ","_",main_dat$new_id_spec)
#### remove old indivuenzahlGesamt column (and number of juveniles (not needed for the moment))
main_dat<-main_dat[-c(9,10)]
main_dat<-distinct(main_dat, new_id_spec, .keep_all = T)
main_dat<-merge(main_dat,tmp_main, by="new_id_spec")
rm(tmp_main)
#write.csv(main_dat,"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/temporal_results/main_data_summed_up.csv", row.names=FALSE)


##### plots about occurrence of species within a year

plot_DIR<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/exploratory_plots/"
specs<-unique(main_dat$art__art)

#### create plots to see when during the year individuals have been caught
for (i in 1:length(specs))
  {
  tmp_specs<-subset(main_dat, main_dat$art__art==specs[i])
  max<-max(tmp_specs$individuenzahlGesamt)
  min<-min(tmp_specs$individuenzahlGesamt)
  png(file= paste0(plot_DIR,specs[i],".png"))
  plot(tmp_specs$day_of_year, tmp_specs$individuenzahlGesamt, main=paste0(specs[i]))
  mtext(paste0("maximum value: ", max, "  //  minimum value: ", min, "  // sampling points: ", dim(tmp_specs)[1]), side=3)
  dev.off()
  }

#### same plots as before but removing the five highest values (the Reuse in Geestacht is producing some very high numbers (30000 bream etc.))
for (i in 1:length(specs))
{
  tmp_specs<-subset(main_dat, main_dat$art__art==specs[i])
  tmp_specs<-tmp_specs[order(tmp_specs$individuenzahlGesamt),]  ### to remove 5 highest values
  if(dim(tmp_specs)[1]>6){tmp_specs<-head(tmp_specs, -5)} #### 5 highest values removed
  max<-max(tmp_specs$individuenzahlGesamt)
  min<-min(tmp_specs$individuenzahlGesamt)
  png(file= paste0(plot_DIR,"five_values_removed_",specs[i],".png"))
  plot(tmp_specs$day_of_year, tmp_specs$individuenzahlGesamt, main=paste0(specs[i], "  5 highest values removed"))
  mtext(paste0("maximum value: ", max, "  //  minimum value: ", min, "  // sampling points: ", dim(tmp_specs)[1]), side=3)
  dev.off()
}


