library(brms)
library(rstan)

# try to get SLURM_CPUS_PER_TASK from submit script, otherwise fall back to 1
cpus_per_task = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
 
rstan_options(auto_write = TRUE)
options(mc.cores = cpus_per_task)
 
##### load and prepare data ####
main_dat<-read.csv("/home/friedrim/clean_data.csv")

args<-commandArgs(trailingOnly=TRUE)
factor_output<-args[1]
index_output<-args[2]

#centre year, month_year and day_year
main_dat$factor_year<- as.factor(main_dat$year)
main_dat$scaled_year <- scale(main_dat$year, scale=F)
main_dat$index_year<-main_dat$year - 2003
main_dat$scaled_year_day <- scale(main_dat$year_day)
main_dat$scaled_year_month <- scale(main_dat$year_month)

specs<- unique(main_dat$species) ### list all species
specs<-specs[order(specs)]

i <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))

tmp_spec<-subset(main_dat,main_dat$species==specs[i])
  
#### model to find trends per year for each species
print(specs[i])   
mod_factor_year <- brm(n_individuals ~ -1 + factor_year + year_day + offset(log(effort_m)) + federal_state+ (1|unique_ID), data = tmp_spec, family = negbinomial(),chains = 4)  
saveRDS(mod_factor_year, file = factor_output)
rm(mod_factor_year)
    
#### model for overall trend
print(specs[i])   
mod_index_year <- brm(n_individuals ~ index_year + year_day + offset(log(effort_m)) + federal_state +(1+scaled_year|unique_ID), data = tmp_spec, family = negbinomial(),chains = 4)
saveRDS(mod_index_year, file = index_output)
rm(mod_index_year)
    