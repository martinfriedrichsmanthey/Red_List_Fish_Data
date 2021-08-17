#### fish population trends for Red List in Germany
##### initial settings ####
### path to data
dat_dir<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/clean_data/"
setwd(dat_dir)

### packages
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()) #8
##### load data and first models ####
main_dat<-read.csv("clean_data.csv")
main_dat$waterbody_type<-ifelse(main_dat$waterbody_type=="",NA,main_dat$waterbody_type)


specs<- unique(main_dat$species) ### list all species

tmp_spec<- subset(main_dat,main_dat$species==specs[1])

mod_1<-brm(n_individuals ~ year + year_day + unique_ID + (1|federal_state), data= tmp_spec, family=negbinomial())

?brm
