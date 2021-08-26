##### trend figures
##### general settings #####
### directory to modelling results
### .rds Files
results_dir<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/modelling_results/"
setwd(results_dir)

library(ggplot2)


##### plot trends based on factor models #####
factor_mods<-list.files(".", pattern="factor_*")
tmp_mod<-readRDS(factor_mods[1])

#### predict new values



newdata <- data.frame(effort_m = 100, year_day = 250, factor_year=factor(c("2005", "2007", "2008", "2009", "2010", 
                                                                           "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                                                                           "2018", "2019", "2020")))

predict(tmp_mod,newdata = newdata, re_formula=NA)