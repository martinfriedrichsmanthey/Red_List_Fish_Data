##### trend figures
##### general settings #####
### directory to modelling results
### .rds Files
results_dir<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/modelling_results/Modell ohne Bundesländer/"
setwd(results_dir)

library(ggplot2)
library(brms)

### rename model outputs
### !!! it is important to order alls the files and names!!!

names<-read.csv("C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/clean_data/clean_data.csv")
names<-unique(names$species)
names<-names[order(names)]

#### rename log files first
old_file_names<-list.files("./Log-Files/")
old_file_names<-old_file_names[order(nchar(old_file_names), old_file_names)]
new_file_names<-gsub("data_analysis_Rote_Liste_Fische","",old_file_names)
new_file_names<-paste0(names,new_file_names)

file.rename(paste0("./Log-Files/",old_file_names),paste0("./Log-Files/",new_file_names))

#### rename factor models
old_file_names<-list.files("./7508757/", pattern="factor")
old_file_names<-old_file_names[order(nchar(old_file_names), old_file_names)]
new_file_names<-gsub("-output","",old_file_names)
new_file_names<-paste0(names,new_file_names)

file.rename(paste0("./7508757/",old_file_names),paste0("./7508757/",new_file_names))

#### and the index files
old_file_names<-list.files("./7508757/", pattern="index")
old_file_names<-old_file_names[order(nchar(old_file_names), old_file_names)]
new_file_names<-gsub("-output","",old_file_names)
new_file_names<-paste0(names,new_file_names)

file.rename(paste0("./7508757/",old_file_names),paste0("./7508757/",new_file_names))

##### plot trend for all species ####
index_mods<-list.files("./7508757/", pattern="index_*")
results_dat<-data.frame(species=NA, estimate=NA,Q2.5=NA, Q97.5=NA)

for (i in 1:length(index_mods))
  {
  print(paste0("start with ",i))
  tmp_mod<-readRDS(paste0("./7508757/",index_mods[i]))
  results_dat[i,1]<-names[i]
  results_dat[i,2]<-exp(fixef(tmp_mod)[2])
  results_dat[i,3]<-exp(fixef(tmp_mod)[8])
  results_dat[i,4]<-exp(fixef(tmp_mod)[11])
  print(paste0("done with ",i))
  }

### check for divergent transitions (rHat above 1.1.)

results_dat$divergent_transitions <-NA

log_files<-list.files("./Log-Files/")

for (i in 1:length(log_files))
  {
  tmp<-read.delim(paste0("./Log-files/",log_files[i]))
  results_dat[i,5]<-ifelse(grepl("divergent transitions",tmp)==TRUE,paste0("YES"),paste0("NO"))
  }

##### remove species with rHat above 1.1
unique(results_dat$species[results_dat$divergent_transitions=="YES"])
#[1] "Acipenser ruthenus"           "Alburnus mento"               "Ameiurus melas"               "Ameiurus nebulosus"           "Babka gymnotrachelus"        
#[6] "Ballerus ballerus"            "Ballerus sapa"                "Cobitis elongatoides"         "Coregonus arenicolus"         "Coregonus oxyrinchus"        
#[11] "Cottus microstomus"           "Cottus perifretum"            "Ctenopharyngodon idella"      "Eudontomyzon vladykovi"       "Gymnocephalus baloni"        
#[16] "Gymnocephalus schraetser"     "Hypophthalmichthys molitrix"  "Lampetra fluviatilis"         "Misgurnus anguillicaudatus"   "Neogobius fluviatilis"       
#[21] "Neogobius melanostomus"       "Osmerus eperlanus"            "Petromyzon marinus"           "Platichthys flesus"           "Pomatoschistus microps"      
#[26] "Ponticola kessleri"           "Romanogobio belingi"          "Romanogobio carpathorossicus" "Romanogobio uranoscopus"      "Romanogobio vladykovi"       
#[31] "Sabanejewia balcanica"        "Sabanejewia baltica"          "Salvelinus fontinalis"        "Salvelinus umbla"             "Telestes souffia"            
#[36] "Umbra pygmaea"                "Vimba vimba"                  "Zingel streber"               "Zingel zingel" 


p<-ggplot() +
  theme_classic()+
  geom_pointrange(data=results_dat[results_dat$divergent_transitions=="NO",], mapping=aes(x=species, y=estimate, ymin=Q2.5, ymax=Q97.5), size=.8, color="darkgreen")+
  theme(axis.text.x = element_text(angle=90))+
  geom_hline(yintercept=1, linetype="dashed")+
  theme(text = element_text(size = 20))

plot(p)

ggsave("C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/result_visualisation/Ergebnisse ohne Bundesländer/trend_all_species.png", plot = p, dpi=300)

##### plot estimates for each year for each species #####

factor_mods<-list.files(".", pattern="factor_*")
names<-gsub("factor_","",factor_mods)
names<-gsub(".rds","", names)


#setup parallel backend to use many processors
library(foreach)
library(doParallel)
cores=detectCores()
cl <- makeCluster(cores[1]) #not to overload your computer
registerDoParallel(cl)

p<-foreach(i=1:length(factor_mods), .packages=c("ggplot2", "brms")) %dopar% 
#for (i in 1:length(factor_mods))
{
  tmp_mod<-readRDS(factor_mods[i])

  tmp_dat<-as.data.frame(fixef(tmp_mod))
  tmp_dat<- head(tmp_dat, - 1)
  year<-rownames(tmp_dat)
  year<-gsub("factor_year","",year)
  tmp_dat$year<-year
  tmp_dat$factor_year<-as.factor(tmp_dat$year)
  tmp_dat$trans_Estimate<-exp(tmp_dat$Estimate)
  tmp_dat$trans_Q2.5<-exp(tmp_dat$Q2.5)
  tmp_dat$trans_Q97.5<-exp(tmp_dat$Q97.5)

  p<-ggplot() +
    theme_classic()+
    geom_pointrange(data=tmp_dat, mapping=aes(x=factor_year, y=trans_Estimate, ymin=trans_Q2.5, ymax=trans_Q97.5), size=.8, color="darkgreen") +
    theme(text = element_text(size = 20))+
    theme(axis.text.x = element_text(angle=90))+
    labs(title = paste0(names[i]), x = "Year", y = "Estimate")

  ggsave(paste0("C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/result_visualisation/",names[i],".png"), plot = p, dpi=300)

  rm(year,tmp_dat,p,tmp_mod)
}
#### predict new values
newdata <- data.frame(effort_m = 100, year_day = 250, factor_year=factor(c("2004", "2005", "2006", "2007", "2008", "2009", "2010", 
                                                                           "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                                                                           "2018", "2019", "2020")), 
                                                      index_year=c(2004:2020))

new_vals<-as.data.frame(predict(tmp_mod_factor,newdata = newdata, re_formula=NA))


new_vals$trns_Estimate<-10^new_vals$Estimate
new_vals$trns_Est.Error<-10^new_vals$Est.Error
new_vals$trns_Q2.5<-10^new_vals$Q2.5
new_vals$trns_Q97.5<-10^new_vals$Q97.5
new_vals$year<-newdata$factor_year
new_vals

ggplot(data=new_vals, aes(x=year, y=trns_Estimate, group=1)) +
  geom_line()+
  geom_point()
