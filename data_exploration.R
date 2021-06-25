data_DIR<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/temporal_results/"
plot_DIR<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/exploratory_plots/"
setwd(data_DIR)

main_dat<-read.csv("main_data_summed_up.csv")

##### plots about occurrence of species within a year

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

