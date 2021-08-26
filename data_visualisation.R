##### trend figures
##### general settings #####
### directory to modelling results
### .rds Files
results_dir<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/modelling_results/"
setwd(results_dir)

library(ggplot2)
library(brms)

##### plot coefficients for each species ####
index_mods<-list.files(".", pattern="index_*")
names<-gsub("index_","",index_mods)
names<-gsub(".rds","", names)
results_dat<-data.frame(species=NA, estimate=NA,Q2.5=NA, Q97.5=NA)

for (i in 1:length(index_mods))
  {
  tmp_mod<-readRDS(index_mods[i])
  results_dat[i,1]<-names[i]
  results_dat[i,2]<-exp(fixef(tmp_mod)[2])
  results_dat[i,3]<-exp(fixef(tmp_mod)[8])
  results_dat[i,4]<-exp(fixef(tmp_mod)[11])
  }

##### remove species with very large CI's
results_dat<-subset(results_dat,!results_dat$species=="Coregonus arenicolus")
results_dat<-subset(results_dat,!results_dat$species=="Acipenser ruthenus")
results_dat<-subset(results_dat,!results_dat$species=="Alburnus mento")
results_dat<-subset(results_dat,!results_dat$species=="Hypophthalmichthys molitrix")
results_dat<-subset(results_dat,!results_dat$species=="Babka gymnotrachelus")





ggplot() +
  theme_classic()+
  geom_pointrange(data=results_dat, mapping=aes(x=species, y=estimate, ymin=Q2.5, ymax=Q9.5))+
  theme(axis.text.x = element_text(angle=90))+
  geom_hline(yintercept=1, linetype="dashed")







factor_mods<-list.files(".", pattern="factor_*")
index_mods<-list.files(".", pattern="index_*")
factor_mods

tmp_mod_factor<-readRDS(factor_mods[7])
tmp_mod_index<-readRDS(index_mods[7])

tmp_mod_factor
fixef(tmp_mod_index)

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
