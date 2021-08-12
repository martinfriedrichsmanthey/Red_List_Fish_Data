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
  rm(tmp_file, source_txt,i)
}
###### in some cases "---" has to be changed to NA ########
main_dat$idMessstelle__messstellenName<-ifelse(main_dat$idMessstelle__messstellenName=="---",NA,main_dat$idMessstelle__messstellenName)
main_dat$idMessstelle__messstellenCode<-ifelse(main_dat$idMessstelle__messstellenCode=="---",NA,main_dat$idMessstelle__messstellenCode)
###### create unique ID for each Messstelle ####
#### replace NAs in Messstellenname with IDs from Messstellencode
if(sum(is.na(main_dat$idMessstelle__messstellenName)))
{
main_dat$idMessstelle__messstellenName<-ifelse(is.na(main_dat$idMessstelle__messstellenName)==TRUE,main_dat$idMessstelle__messstellenCode,main_dat$idMessstelle__messstellenName)
}
# this has to be done, because MfN is assigning IDs for each federal state separately. Therefore ID 1 occurs 14 times etc.
main_dat$unique_ID<-paste0(main_dat$idMessstelle__messstellenName,"_",main_dat$excelFileName__bundesland__bundesland)

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
###### remove all unique Ids which have only been sampled ones #####
tmp_ID_data<-data.frame(matrix(ncol = 2, nrow = 0))
colnames(tmp_ID_data) <- c('unique_ID_Messstelle', 'n_years')
IDs<-unique(main_dat$unique_ID)

for(i in 1:length(IDs))
{
tmp<-subset(main_dat,main_dat$unique_ID==IDs[i])
tmp_ID_data[i,1]<-paste0(IDs[i])
tmp_ID_data[i,2]<-length(unique(tmp$year))
}

table(tmp_ID_data$n_years)
#   1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
#5627 2145 1381 1195  898  268  112   70   22    7   14   13    4    2    2

single_IDs<-tmp_ID_data[tmp_ID_data$n_years==1,]
main_dat<-subset(main_dat, (!main_dat$unique_ID %in% single_IDs$unique_ID_Messstelle))   ### ~ 23.000 entries removed (NAs kept so far)

rm(list=ls(pattern="tmp*"), single_IDs, IDs,i)
###### fill missing species for Messstelle/Year ####

main_dat_copy<-main_dat ### copy main data set to see if the approach works

unique_IDs<-unique(main_dat$unique_ID) #### list all unique ID (unique sampling points)  ### 3792 (including NAs)

for (i in 1:length(unique_IDs))
  {
  tmp_1<-subset(main_dat,main_dat$unique_ID==paste0(unique_IDs[i]))  #### subset main data set for the first ID

  tmp_specs_all<-unique(tmp_1$art__art)  ### list all species that have been caught over all years at this ID
  tmp_years_all<-unique(tmp_1$year) ### list all years this particular ID has been sampled
  print(paste0("ID ", i, " started"))
  for (k in 1:length(tmp_years_all))
    {
    print(paste0("year ", k, " started"))
    tmp_year<-subset(tmp_1,tmp_1$year==tmp_years_all[k])  ### subset the ID data set for the first available year

    tmp_missing_spec<-tmp_specs_all[!tmp_specs_all %in% unique(tmp_year$art__art)]   ### create list of species which are missing for this year (compared to the whole list for this sampling point)
    if(length(tmp_missing_spec)>0)
      {
      tmp_new_dat<-tmp_year[c(1:length(tmp_missing_spec)),]  ### create small data set with the length according to the number of missing species
      tmp_new_dat$individuenzahlGesamt=0  #### set individual number to 0
      tmp_new_dat$individuenzahlJuvenil=0 #### set the number of juveniles to 0
      tmp_new_dat$art__art<-tmp_missing_spec ### set the species name to the missing species for this year on this sampling point
      main_dat_copy<-rbind(main_dat_copy,tmp_new_dat) ### combine with the main data set
      }
    rm(tmp_year, tmp_missing_spec, tmp_new_dat)
    print(paste0("year ", k, " done"))
    }
  rm(tmp_1, tmp_specs_all, tmp_years_all)
  print(paste0("ID ", i, " done"))
}

main_dat<- main_dat_copy
rm(main_dat_copy)
####  Dianas solution
#### this fills all species to each Messstelle, what is actually not a real problem
#makeComplete<-function(df){#to add zeros for absenses in a data frame
#  df2<-expand.grid(SiteID=unique(df$SiteID),Year=unique(df$Year),Species=unique(df$Species))
#  df3<-merge(df2,df,by=c("Species","SiteID","Year"),all.x=T)
#  df3$Count[is.na(df3$Count)]<-0
#  return(df3)
#}

###### plots with filled species list #####
plot_DIR<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/exploratory_plots/"
specs<-unique(main_dat$art__art)

#### create plots log abundance ~ year plots for all species
for (i in 1:length(specs))
{
  tmp_specs<-subset(main_dat,main_dat$art__art==specs[i])
  png(file= paste0(plot_DIR,"filled_list_year_",specs[i],".png"))
  boxplot(log(tmp_specs$individuenzahlGesamt+1)~tmp_specs$year, main=paste0("filled list ",specs[i]))
  dev.off()
  rm(tmp_specs)
}

###### clean "methodeZusatz__methodeZusatz" column ####
unique(main_dat$methodeZusatz__methodeZusatz)
#[1] NA                     "Bootsfischerei"       "Watfischerei"         "Uferbefischung"       ""                    
#[6] "Wat & Bootsfischerei" "Bootsfischerei Ufer"
table(main_dat$methodeZusatz__methodeZusatz)
#   ""       Bootsfischerei  Bootsfischerei Ufer       Uferbefischung Wat & Bootsfischerei         Watfischerei 
#24371                46224                  244                12674                  854                52973 
sum(is.na(main_dat$methodeZusatz__methodeZusatz)) #### 107666
#### insert NAs for empty columns
main_dat$methodeZusatz__methodeZusatz<-ifelse(grepl("f", main_dat$methodeZusatz__methodeZusatz)==FALSE, NA, main_dat$methodeZusatz__methodeZusatz)

main_dat$methodeZusatz__methodeZusatz<-ifelse(grepl("Wat & Bootsfischerei", main_dat$methodeZusatz__methodeZusatz)==TRUE, "both methods", main_dat$methodeZusatz__methodeZusatz)
main_dat$methodeZusatz__methodeZusatz<-ifelse(grepl("Bootsfischerei", main_dat$methodeZusatz__methodeZusatz)==TRUE, "boat", main_dat$methodeZusatz__methodeZusatz)
main_dat$methodeZusatz__methodeZusatz<-ifelse(grepl("fisch", main_dat$methodeZusatz__methodeZusatz)==TRUE, "waders", main_dat$methodeZusatz__methodeZusatz)
##### create cleaned data set for further analyses ###########
clean_data<-subset(main_dat, select=c("art__art", "individuenzahlGesamt", "excelFileName__bundesland__bundesland", "methodeZusatz__methodeZusatz", "befischteStrecke", "idMessstelle__wasserkoerperCode__gewaesserName", "idMessstelle__wasserkoerperCode__gewaesserkategorie__gewaesserkategorie", "x_EPSG.25832", "y_EPSG.25832", "unique_ID", "date", "day_of_year", "month_of_year","year", "project_category"))
names(clean_data)
names(clean_data)<-c("species", "n_individuals", "federal_state", "method", "effort_m","waterbody_name", "waterbody_type", "x_EPSG_25832", "y_EPSG_25832", "unique_ID", "date", "year_day", "year_month", "year", "project_category")
clean_data<-clean_data[clean_data$effort_m>10,]  #### if effort is less than 10 metres smething is wrong ;)
write.csv(clean_data, "C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/clean_data/clean_data.csv", row.names =F)
rm(main_dat_copy, main_dat, i, k, unique_IDs)
