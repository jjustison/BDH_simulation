  
######
##Simulate under the whole gambit of hybrid proportions
######
rm(list = ls()) ## Start with fresh environment
set.seed(487) ##arbitrary seed. Largest 10-happy prime below 500

file_name_base<-paste('../data/simplex',sep='')



gen_dist<-NULL

source('./common_pars.R') ##Load common parameters

dir.create(file_name_base)
save.image(file=paste(file_name_base,'/parameters.Rdata',sep='')) ## Save settings before running
for(rw in 1:nrow(dat)){
  file_name<-paste(file_name_base,'/row_',rw,sep = '')
  dir.create(file_name)

  my_props<-dat[rw,]

  hyb_prop_func<-function() my_props
  save.image(file=paste(file_name,'/parameters.Rdata',sep=''))
  source('./sim_runs.R')
  source('./net_analysis.R')

}


######
##Simulate under the whole gambit of hybrid proportions with genetic distance dependence
## linear genetic distance dependence - slope of 2
######
rm(list = ls()) ## Start with fresh environment
set.seed(51934) ##arbitrary seed. Number of digits in the largest sexy prime pair

slope<-2
gen_dist<-make.linear.decay(slope)

file_name_base<-paste('../data/simplex_gen_dist_',slope,sep='')

source('./common_pars.R')

dir.create(file_name_base)
save.image(file=paste(file_name_base,'/parameters.Rdata',sep=''))
for(rw in 1:nrow(dat)){
  file_name<-paste(file_name_base,'/row_',rw,sep = '')
  dir.create(file_name)

  my_props<-dat[rw,]

  hyb_prop_func<-function() my_props
  save.image(file=paste(file_name,'/parameters.Rdata',sep=''))
  source('./sim_runs.R')
  source('./net_analysis.R')

}


######
##Simulate under the whole gambit of hybrid proportions with genetic distance dependence
## linear genetic distance dependence - slope of 0.5
######
rm(list = ls()) ## Start with fresh environment
set.seed(1361) ##arbitrary seed. 3rd mills prime



slope<-0.5
gen_dist<-make.linear.decay(slope)

file_name_base<-paste('../data/simplex_gen_dist_',slope,sep='')

source('./common_pars.R')

dir.create(file_name_base)
save.image(file=paste(file_name_base,'/parameters.Rdata',sep=''))
for(rw in 1:nrow(dat)){
  file_name<-paste(file_name_base,'/row_',rw,sep = '')
  dir.create(file_name)
  
  my_props<-dat[rw,]
  
  hyb_prop_func<-function() my_props
  save.image(file=paste(file_name,'/parameters.Rdata',sep=''))
  source('./sim_runs.R')
  source('./net_analysis.R')
  
}



######
##Simulate under the whole gambit of hybrid proportions with genetic distance dependence
## linear genetic distance dependence - slope of 4
######
rm(list = ls()) ## Start with fresh environment
set.seed(1361) ##arbitrary seed. 3rd mills prime



slope<-4
gen_dist<-make.linear.decay(slope)

file_name_base<-paste('../data/simplex_gen_dist_',slope,sep='')

source('./common_pars.R')

dir.create(file_name_base)
save.image(file=paste(file_name_base,'/parameters.Rdata',sep=''))
for(rw in 1:nrow(dat)){
  file_name<-paste(file_name_base,'/row_',rw,sep = '')
  dir.create(file_name)
  
  my_props<-dat[rw,]
  
  hyb_prop_func<-function() my_props
  save.image(file=paste(file_name,'/parameters.Rdata',sep=''))
  source('./sim_runs.R')
  source('./net_analysis.R')
  
}


######
##Simulate under the whole gambit of hybrid proportions sampling fraction of 0.75
######
rm(list = ls()) ## Start with fresh environment
set.seed(31172165) ##arbitrary seed. Exponent of largest known Einstein prime


rho <- 0.75

file_name_base<-paste('../data/simplex_sampling_frac_',rho,sep='')


source('./common_pars.R')

dir.create(file_name_base)
save.image(file=paste(file_name_base,'/parameters.Rdata',sep=''))
for(rw in 1:nrow(dat)){
  file_name<-paste(file_name_base,'/row_',rw,sep = '')
  dir.create(file_name)
  
  my_props<-dat[rw,]
  
  hyb_prop_func<-function() my_props
  save.image(file=paste(file_name,'/parameters.Rdata',sep=''))
  source('./sim_runs.R')
  source('./net_analysis.R')
  
}
 
######
## Simulate under a single hybrid proportion but change the sampling fraction
## hybrid proportion of 1/3 for all types
######
rm(list = ls()) ## Start with fresh environment
set.seed(65537) ##arbitrary seed. Exponent of largest known Fermat prime 


library(SiPhyNetwork)

hyb_prop_func<-function() c(1/3,1/3,1/3)

file_name_base<-paste('../data/all_equal_hybs_sampling_frac',sep='')


##make a vector with the sampling fraction
fracs <- seq(0.25,1,by=0.025)

source('./common_pars.R')

dir.create(file_name_base)
save.image(file=paste(file_name_base,'/parameters.Rdata',sep=''))
for(rho in fracs){
  file_name<-paste(file_name_base,'/frac_',rho,sep = '')
  dir.create(file_name)

  save.image(file=paste(file_name,'/parameters.Rdata',sep=''))
  source('./sim_runs.R')
  source('./net_analysis_rec_only.R') ##sampling only concerns us with reconstructed phys
  
}


######
## Simulate under a single hybrid proportion but change the gen dist slope
## hybrid proportion of 1/3 for all types
######
rm(list = ls()) ## Start with fresh environment
set.seed(127) ##arbitrary seed. Exponent of largest known double Mersenne prime 



hyb_prop_func<-function() c(1/3,1/3,1/3)

file_name_base<-paste('../data/all_equal_hybs_gen_dist',sep='')

#simulation params
slopes<-seq(0.25,2,by=0.05)

source('./common_pars.R')

dir.create(file_name_base)
save.image(file=paste(file_name_base,'/parameters.Rdata',sep=''))
for(slope in slopes){
  file_name<-paste(file_name_base,'/dist_',slope,sep = '')
  dir.create(file_name)
  
  gen_dist<-make.linear.decay(1/slope)
  
  
  save.image(file=paste(file_name,'/parameters.Rdata',sep=''))
  source('./sim_runs.R')
  source('./net_analysis.R')
  
}


######
##Simulate under the whole gambit of hybrid proportions - double time to 20 sec
######
rm(list = ls()) ## Start with fresh environment
set.seed(487) ##arbitrary seed. Largest 10-happy prime below 500

file_name_base<-paste('../data/simplex_double_time',sep='')



gen_dist<-NULL

source('./common_pars.R') ##Load common parameters

nreps<-10000
timeout_time<-20

dir.create(file_name_base)
save.image(file=paste(file_name_base,'/parameters.Rdata',sep='')) ## Save settings before running
for(rw in 1:nrow(dat)){
  file_name<-paste(file_name_base,'/row_',rw,sep = '')
  dir.create(file_name)
  
  my_props<-dat[rw,]
  
  hyb_prop_func<-function() my_props
  save.image(file=paste(file_name,'/parameters.Rdata',sep=''))
  source('./sim_runs.R')
  source('./net_analysis.R')
  
}



