#### fish population trends for Red List in Germany
##### initial settings ####
### path to data
dat_dir<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/clean_data/"
save_dir<-"C:/Users/zf53moho/Documents/NFDI4BioDiv/Data/Fish Data/Fischdaten_Datenbank/Red_List_Fish_Data/modelling_results/"
setwd(dat_dir)

### packages
library(brms)
library(rstan)
library(foreach)
library(doParallel)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()) #8
##### load and prepare data ####
main_dat<-read.csv("clean_data.csv")

#centre year, month_year and day_year
main_dat$factor_year<- as.factor(main_dat$year)
main_dat$scaled_year <- scale(main_dat$year, scale=F)
main_dat$index_year<-main_dat$year - 2003
main_dat$scaled_year_day <- scale(main_dat$year_day)
main_dat$scaled_year_month <- scale(main_dat$year_month)

specs<- unique(main_dat$species) ### list all species

#### general infos 
#### Rhat should be less than 1.1
#### we could predict trends for each site and species and plot them
#### plot the random slopes for each species to see the variation among sites
      # sd(scaled_year)                0.19      0.03     0.14     0.25
#### use the predict function to predict the amount of species for a certain day with certain effort within a year
#### pirateplots  to visualise the data
#### include seasonality --> I(year_day^2) ---> models do not converge
#### the -1 in front of the model formula removes the intercept 
#### factor year only for visualization
#### the offset(log(effort_m)) indicates that we assume that we would catch the twice the amount of species if we double the effort


#setup parallel backend to use many processors
#cores=detectCores()
#cl <- makeCluster(cores[1]-3) #not to overload your computer
#registerDoParallel(cl)

#p<-foreach(i=1:length(specs), .packages="brms") %dopar% 
for (i in 1:length(specs))
  {
  tmp_spec<-subset(main_dat,main_dat$species==specs[i])
  
    #### model to find trends per year for each species
    if(file.exists(paste0(save_dir,"factor_",specs[i],".rds"))==FALSE)
    {
    mod_factor_year <- brm(n_individuals ~ -1 + factor_year + year_day + offset(log(effort_m)) +(1|unique_ID), data = tmp_spec, family = negbinomial())  
    saveRDS(mod_factor_year, file = paste0(save_dir,"factor_",specs[i],".rds"))
    rm(mod_factor_year)
    }
    #### model for overall trend
    if(file.exists(paste0(save_dir,"index_",specs[i],".rds"))==FALSE)
    {
    mod_index_year <- brm(n_individuals ~ index_year + year_day + offset(log(effort_m)) + (1+scaled_year|unique_ID), data = tmp_spec, family = negbinomial())
    saveRDS(mod_index_year, file = paste0(save_dir,"index_",specs[i],".rds"))
    rm(mod_index_year)
    }
  rm(tmp_spec)
  }



mod_factor_year <- brm(n_individuals ~ -1 + factor_year + year_day + offset(log(effort_m)) +(1|unique_ID), data = tmp_spec, family = negbinomial())  
saveRDS(mod_factor_year, file = paste0(save_dir,"factor_",specs[3],".rds"))


mod_index_year <- brm(n_individuals ~ index_year + year_day + offset(log(effort_m)) + (1+scaled_year|unique_ID), data = tmp_spec, family = negbinomial())
saveRDS(mod_index_year, file = paste0(save_dir,"index_",specs[3],".rds"))


#### simplest model for 1 species and all years

#Family: negbinomial 
#Links: mu = log; shape = identity 
#Formula: n_individuals ~ scaled_year + year_day + year_day^2 + (1 + scaled_year | unique_ID) 
#Data: tmp_spec (Number of observations: 1066) 
#Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#total post-warmup samples = 4000
#
#Group-Level Effects: 
#  ~unique_ID (Number of levels: 259) 
#                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#sd(Intercept)                  2.54      0.14     2.27     2.83 1.00     1047     1957
#sd(scaled_year)                0.79      0.12     0.56     1.03 1.00     1028     2148
#cor(Intercept,scaled_year)    -0.65      0.11    -0.84    -0.42 1.00     1100     2148
#
#Population-Level Effects: 
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept       2.01      0.31     1.42     2.61 1.00     2381     3021
#scaled_year     1.03      0.10     0.84     1.22 1.00     2469     2771
#year_day        0.00      0.00     0.00     0.01 1.00     5124     3414
#
#Family Specific Parameters: 
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#shape     0.47      0.03     0.41     0.53 1.01     1209     3017
#
#Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
#and Tail_ESS are effective sample size measures, and Rhat is the potential
#scale reduction factor on split chains (at convergence, Rhat = 1).

#### simplest model for 1 species and all years
fit1_all_years_method <- brm(n_individuals ~ scaled_year + year_day + offset(log(effort_m)) + (1+scaled_year|unique_ID), data = tmp_spec, family = negbinomial())  ##### random slopes for scaled year (1+scaled...)  
fit1_all_years_method
plot(fit1_all_years_method)
fixef(fit1_all_years_method)
















#fit1_all_years_factor_seasonality <- brm(n_individuals ~ -1 + factor_year + year_day + I(year_day^2) + offset(log(effort_m)) +(1|unique_ID), data = tmp_spec, family = negbinomial())  #### the -1 removes the intercept #### factor year only for visualization
#fit1_all_years_factor_seasonality
#plot(fit1_all_years_factor_seasonality)
#fixef(fit1_all_factor_seasonality)
##### models with seasonality do not converge






















### model for eals
tmp_anguilla<- subset(main_dat,main_dat$species==specs[7])
tmp_anguilla_all_years <- brm(n_individuals ~ scaled_year, data = tmp_anguilla, family = negbinomial())
tmp_anguilla_all_years

#Family: negbinomial 
#Links: mu = log; shape = identity 
#Formula: n_individuals ~ scaled_year 
#Data: tmp_anguilla (Number of observations: 12197) 
#Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#total post-warmup samples = 4000
#
#Population-Level Effects: 
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept       2.28      0.02     2.24     2.31 1.00     3979     2176
#scaled_year    -0.33      0.02    -0.36    -0.29 1.00     3857     2893
#
#Family Specific Parameters: 
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#shape     0.27      0.00     0.26     0.27 1.00     4301     3320
#
#Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
#and Tail_ESS are effective sample size measures, and Rhat is the potential
#scale reduction factor on split chains (at convergence, Rhat = 1).

