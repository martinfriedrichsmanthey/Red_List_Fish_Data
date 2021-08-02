###### Skript for data preparation

### set data_DIR to directory where the data is stored
data_DIR <- "C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/raw_data"
setwd(data_DIR)
###### libraries ####
library(lubridate)
library(stringr)
library(sf)
library(raster)
library(dplyr)
library(gert)
###### [OLD] data comes in .csv format ##############
#### for now we receive the data in 7 separate .csv files named out_0.csv ... out_6.csv
#### create main data file
#source_csvs<-list.files(".", pattern="out*")

#main_dat<-read.csv(source_csvs[1], dec=".")

#for (i in 2:length(source_csvs))
  #{
  #tmp_file<-read.csv(source_csvs[i], dec=".")
  #main_dat<-rbind(main_dat,tmp_file)
  #rm(tmp_file)
  #}

###### data comes now in a .txt format, one .txt for each federal state (bremen is merged with lower saxony, baden-württemberg didn't deliver data) ######
#### the "agg" stands for "aggregated" meaning that indivudiuals per event on the same site are summed up
source_txt<-list.files(".", pattern="agg*")

main_dat<-read.delim2(source_txt[1], header = TRUE, sep = ",", dec = ".")

for (i in 2:length(source_txt))
{
  tmp_file<-read.delim2(source_txt[i], header = TRUE, sep = ",", dec = ",")
  main_dat<-rbind(main_dat,tmp_file)
  rm(tmp_file)
}
###### create unique ID for each Messstelle ####
# this has to be done, because MfN is assigning IDs for each federal state separately. Therefore ID 1 occurs 14 times etc.
main_dat$unique_ID<-paste0(main_dat$probenameID,"_",main_dat$excelFileName__bundesland__bundesland)
###### in some cases "---" has to be changed to NA ########
main_dat$idMessstelle__messstellenName<-ifelse(main_dat$idMessstelle__messstellenName=="---",NA,main_dat$idMessstelle__messstellenName)
main_dat$idMessstelle__messstellenCode<-ifelse(main_dat$idMessstelle__messstellenCode=="---",NA,main_dat$idMessstelle__messstellenCode)
###### create proper date column and calculate day of year and month of year, year #####
main_dat$date <- as.Date(main_dat$datum,"%Y-%m-%d")
main_dat$day_of_year <- yday(main_dat$date)
main_dat$month_of_year<-month(main_dat$date)
main_dat$year<-year(main_dat$date)
###### spatial distribution of sampling points per years ########
options(digits=20) ### need to set the number of digits
main_dat$x_EPSG.25832<-as.numeric(main_dat$x_EPSG.25832) 
main_dat$y_EPSG.25832<-as.numeric(main_dat$y_EPSG.25832) 
### copy dataframe
spatial_main_dat<-main_dat
### define coordinate columns
coordinates(spatial_main_dat)<-~x_EPSG.25832+y_EPSG.25832
#### set crs to EPSG_25832
crs(spatial_main_dat)= "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs +type=crs"
### get map of Germany
Germany <- getData("GADM",country="DEU",level=0)
### transform and plot map of Germany
Germany_trnsfrmd = spTransform(Germany,"+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs +type=crs")
rm(Germany)
### create plots
plot_DIR<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/exploratory_plots/"
years<-unique(spatial_main_dat$year)
for (i in 1:length(years))
{
  tmp_years<-subset(spatial_main_dat, spatial_main_dat$year==years[i])
  png(file= paste0(plot_DIR,years[i],".png"))
  plot(Germany_trnsfrmd,col = 'red', main=paste0(years[i]))
  plot(tmp_years, add=T)
  rm(tmp_years)
  dev.off()
}
###### remove everything before 2004 ######
main_dat<-main_dat[main_dat$year>2003,]  #### ~ 21.000 entries removed
###### remove everything except Electrofishing ####
main_dat<-main_dat[main_dat$befischungMethode__befischungMethode=="Elektrobefischung",]  #### ~ 10.000 entries removed
###### clean species column ####
main_dat<-main_dat[!main_dat$art__art=="Keine fische",]  ### ~ 600 entries with no fish removed
main_dat<-main_dat[!main_dat$art__art=="Lampetra spec.",]  ### ~ 700 entries with Lampetra spec. removed
###### check how many sites have no information on "befischte Strecke" or "befischte Zeit" or "befischte Fläche" ###########
main_dat$befischteStrecke<-as.numeric(main_dat$befischteStrecke)
main_dat<-main_dat[!is.na(main_dat$befischteStrecke),]  ### ~ 10.000 observations have no information on Befischungsstrecke (and only a very few of them have information on other effort measures (~ 600 to 800))
###### create new project column #######
#unique(main_dat$projekt__projekt) ### 421 different projects
main_dat$project_category<-ifelse(grepl("WRRL",main_dat$projekt__projekt)==TRUE,"WRRL",main_dat$projekt__projekt)
main_dat$project_category<-ifelse(grepl("FFH",main_dat$project_category)==TRUE,"FFH",main_dat$project_category)
toMatch<-c("WRRL","FFH")
main_dat$project_category<-ifelse(grepl(paste0(toMatch, collapse="|"),main_dat$project_category)==FALSE,"other",main_dat$project_category)
rm(toMatch)
###### [OLD] clean species column remove species with less than 10 occurences ########
#main_dat_ger<-main_dat_ger[main_dat_ger$art__art %in% names(which(table(main_dat$art__art) > 10)), ]
####still some cleaning needed
#main_dat_ger<-subset(main_dat_ger,!main_dat_ger$art__art=="Acipenser sturio")
#main_dat_ger<-subset(main_dat_ger,!main_dat_ger$art__art=="Alosa alosa")
#main_dat_ger<-subset(main_dat_ger,!main_dat_ger$art__art=="Lampetra spec.")
#main_dat_ger<-subset(main_dat_ger,!main_dat_ger$art__art=="Keine fische")
#main_dat_ger<-subset(main_dat_ger,!main_dat_ger$art__art=="Coregonus spec.")
#length(unique(main_dat_ger$art__art)) # 74 species left
###### [run only if you need the plots!] plots with abundance ~ yearday #####
plot_DIR<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/exploratory_plots/"
specs<-unique(main_dat$art__art)

####create plots to see when during the year individuals have been caught
#for (i in 1:length(specs))
#{
#  tmp_specs<-subset(main_dat, main_dat$art__art==specs[i])
#  max<-max(tmp_specs$individuenzahlGesamt)
#  min<-min(tmp_specs$individuenzahlGesamt)
#  png(file= paste0(plot_DIR,specs[i],".png"))
#  plot(tmp_specs$day_of_year, tmp_specs$individuenzahlGesamt, main=paste0(specs[i]), xlim =c(0,365))
#  mtext(paste0("maximum value: ", max, "  //  minimum value: ", min, "  // sampling points: ", dim(tmp_specs)[1]), side=3)
#  dev.off()
#  rm(tmp_specs, max,min)
#}

#### same plots with log abundance
#for (i in 1:length(specs))
#{
#  tmp_specs<-subset(main_dat, main_dat$art__art==specs[i])
#  max<-max(tmp_specs$individuenzahlGesamt)
#  min<-min(tmp_specs$individuenzahlGesamt)
#  png(file= paste0(plot_DIR,"log_",specs[i],".png"))
#  plot(tmp_specs$day_of_year, log(tmp_specs$individuenzahlGesamt), main=paste0("log ",specs[i]), xlim =c(0,365))
#  mtext(paste0("maximum value: ", max, "  //  minimum value: ", min, "  // sampling points: ", dim(tmp_specs)[1]), side=3)
#  dev.off()
#  rm(tmp_specs, max,min)
#}

#### same plots with abundance/effort
#for (i in 1:length(specs))
#{
#  tmp_specs<-subset(main_dat, main_dat$art__art==specs[i])
#  max<-max(tmp_specs$individuenzahlGesamt)
#  min<-min(tmp_specs$individuenzahlGesamt)
#  png(file= paste0(plot_DIR,"effort_",specs[i],".png"))
#  plot(tmp_specs$day_of_year, tmp_specs$individuenzahlGesamt/tmp_specs$befischteStrecke, main=paste0("effort ",specs[i]), xlim =c(0,365))
#  mtext(paste0("maximum value: ", max, "  //  minimum value: ", min, "  // sampling points: ", dim(tmp_specs)[1]), side=3)
#  dev.off()
#  rm(tmp_specs, max,min)
#}

#### log plots with mean abundance (because several federal state have sampled the same day in the year)
for (i in 1:length(specs))
{
  tmp_specs<-subset(main_dat, main_dat$art__art==specs[i])
  tmp_agg<-aggregate(individuenzahlGesamt ~ day_of_year, tmp_specs, mean)
  max<-max(tmp_specs$individuenzahlGesamt)
  min<-min(tmp_specs$individuenzahlGesamt)
  png(file= paste0(plot_DIR,"log_mean_",specs[i],".png"))
  plot(tmp_agg$day_of_year, log(tmp_agg$individuenzahlGesamt), main=paste0("log mean ",specs[i]), xlim =c(0,365))
  mtext(paste0("maximum value: ", max, "  //  minimum value: ", min, "  // sampling points: ", dim(tmp_specs)[1]), side=3)
  dev.off()
  rm(tmp_specs, max,min, tmp_agg)
}


rm(specs)
###### plots with abundance ~ year ####
plot_DIR<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/exploratory_plots/"
specs<-unique(main_dat$art__art)

#### create plots to see when during the year individuals have been caught
for (i in 1:length(specs))
{
  tmp_specs<-subset(main_dat, main_dat$art__art==specs[i])
  png(file= paste0(plot_DIR,"year_",specs[i],".png"))
  boxplot(tmp_specs$individuenzahlGesamt~tmp_specs$year, main=paste0(specs[i]))
  dev.off()
  rm(tmp_specs)
}

#### same plots on log scale
for (i in 1:length(specs))
{
  tmp_specs<-subset(main_dat, main_dat$art__art==specs[i])
  png(file= paste0(plot_DIR,"log_year_",specs[i],".png"))
  boxplot(log(tmp_specs$individuenzahlGesamt)~tmp_specs$year, main=paste0("log ",specs[i]))
  dev.off()
  rm(tmp_specs)
}

#### same plots on with effort scale
for (i in 1:length(specs))
{
  tmp_specs<-subset(main_dat, main_dat$art__art==specs[i])
  png(file= paste0(plot_DIR,"effort_year_",specs[i],".png"))
  boxplot(tmp_specs$individuenzahlGesamt/tmp_specs$befischteStrecke~tmp_specs$year, main=paste0("effort ",specs[i]))
  dev.off()
  rm(tmp_specs)
}

#### same plots on with log/effort scale
for (i in 1:length(specs))
{
  tmp_specs<-subset(main_dat, main_dat$art__art==specs[i])
  png(file= paste0(plot_DIR,"log_effort_year_",specs[i],".png"))
  boxplot(log(tmp_specs$individuenzahlGesamt/tmp_specs$befischteStrecke)~tmp_specs$year, main=paste0("log_effort ",specs[i]))
  dev.off()
  rm(tmp_specs)
}
rm(specs)
###### 
tmp_ID_data<-data.frame(matrix(ncol = 2, nrow = 0))
colnames(tmp_ID_data) <- c('uniqueID', 'n_years')
IDs<-unique(main_dat$unique_ID)

for(i in 1:length(IDs))
{
tmp<-subset(main_dat,main_dat$unique_ID==IDs[i])
tmp_ID_data[i,1]<-paste0(IDs[i])
tmp_ID_data[i,2]<-length(unique(tmp$year))
}

rm(list=ls(pattern="tmp*"))
### check for combinations of messstelle and methode
tmp_unique<-unique(main_dat[c("methodeZusatz__methodeZusatz","unique_ID")])
#### all messstellen which have been sampled with different methods
tmp_non_unique<-tmp_unique[duplicated(tmp_unique$unique_ID),]

### remove all methods except "Elektrobefischung"
main_dat_ger_2004<-subset(main_dat_ger_2004, main_dat_ger_2004$befischungMethode__befischungMethode=="Elektrobefischung")
# dim(main_dat_ger_2004) ### 202453     37

### check how often a messtelle has been sampled
tmp_unique<-unique(main_dat_ger_2004[c("idMessstelle__messstellenName", "year")])

toMatch<-names(Filter(function(x) x<2, table(tmp_unique$idMessstelle__messstellenName)))

#### remove alle Messstellen, die nur 1 mal besampled wurden

tmp_test<-main_dat_ger_2004[!grepl(paste0(toMatch[1]),main_dat_ger_2004$idMessstelle__messstellenName),]
for (i in 2:length(toMatch))
  {
  tmp_test<-tmp_test[!grepl(paste0(toMatch[i]),tmp_test$idMessstelle__messstellenName),]
  }
### dabei fliegen circa 60.000 Datenpunkte raus


tmp_unique_id<-unique(main_dat_ger_2004[c("idMessstelle__id", "year")])
toMatch_id<-names(Filter(function(x) x<2, table(tmp_unique_id$idMessstelle__id)))
toMatch_id<-paste0("^",toMatch_id,"$")

tmp_test_id<-main_dat_ger_2004[!grepl(paste0(toMatch_id[1]),main_dat_ger_2004$idMessstelle__id),]
for (i in 2:length(toMatch))
{
  tmp_test<-tmp_test[!grepl(paste0(toMatch[i]),tmp_test$idMessstelle__messstellenName),]
}

tmp_unique_id<-main_dat_ger_2004[with(main_dat_ger_2004, ave(idMessstelle__id,FUN=length))>1, ]
table(tmp_unique_id$idMessstelle__id)
?ave
