  
######
##Simulate under the whole gambit of hybrid proportions
######

library(SiPhyNetwork)
nreps<-10000
timeout_time<-15
path<-'../data'

file_name_base<-paste(path,'/simplex',sep='')
dir.create(file_name_base)
#simulation params
age <-1
hyb_fxn<-make.beta.draw(1,1)

gen_dist<-NULL

lambda_func<-function() 4
mu_func<-function() 2
nu_func<-function() 0.1

##Make a data.frame with the hybrid proportions
nlayers<-11
npoints<-61

hyb_pos<-rep(NA,npoints)
hyb_neu<-rep(NA,npoints)
hyb_neg<-rep(NA,npoints)

i<-0
layer_space<-1/(nlayers-1)
for(div in seq(from=-1,to=1,length.out=nlayers)){
  leftover<-1-abs(div)

  hyb_zero<-seq(0,leftover,by=layer_space)

  leftover_leftover<-leftover-hyb_zero
  hyb_plus<- (div+1-hyb_zero)/2
  hyb_minus<-hyb_plus-div

  nvals<-length(hyb_minus)

  hyb_pos[(i+1):(i+nvals)]<-hyb_plus
  hyb_neg[(i+1):(i+nvals)]<-hyb_minus
  hyb_neu[(i+1):(i+nvals)]<-hyb_zero

  i<-i+nvals
}
dat<-data.frame(hyb_pos,hyb_neg,hyb_neu)

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
## linear genetic distance dependence - slope of 1
######

library(SiPhyNetwork)
nreps<-10000
timeout_time<-15
path<-'../data'

slope<-1
gen_dist<-make.linear.decay(slope)

file_name_base<-paste(path,'/simplex_gen_dist_',slope,sep='')
dir.create(file_name_base)
#simulation params
age <-1
hyb_fxn<-make.beta.draw(1,1)



lambda_func<-function() 4
mu_func<-function() 2
nu_func<-function() 0.1

##Make a data.frame with the hybrid proportions
nlayers<-11
npoints<-61

hyb_pos<-rep(NA,npoints)
hyb_neu<-rep(NA,npoints)
hyb_neg<-rep(NA,npoints)

i<-0
layer_space<-1/(nlayers-1)
for(div in seq(from=-1,to=1,length.out=nlayers)){
  leftover<-1-abs(div)

  hyb_zero<-seq(0,leftover,by=layer_space)

  leftover_leftover<-leftover-hyb_zero
  hyb_plus<- (div+1-hyb_zero)/2
  hyb_minus<-hyb_plus-div

  nvals<-length(hyb_minus)

  hyb_pos[(i+1):(i+nvals)]<-hyb_plus
  hyb_neg[(i+1):(i+nvals)]<-hyb_minus
  hyb_neu[(i+1):(i+nvals)]<-hyb_zero

  i<-i+nvals
}
dat<-data.frame(hyb_pos,hyb_neg,hyb_neu)

# save.image(file=paste(file_name_base,'/parameters.Rdata',sep=''))


for(rw in 1:nrow(dat)){
  file_name<-paste(file_name_base,'/row_',rw,sep = '')
  dir.create(file_name)

  my_props<-dat[rw,]

  hyb_prop_func<-function() my_props
  # save.image(file=paste(file_name,'/parameters.Rdata',sep=''))
  # source('./sim_runs.R')
  source('./net_analysis.R')

}


######
##Simulate under the whole gambit of hybrid proportions with genetic distance dependence
## linear genetic distance dependence - slope of 0.5
######

library(SiPhyNetwork)
nreps<-10000
timeout_time<-15
path<-'../data'

slope<-0.5
gen_dist<-make.linear.decay(slope)

file_name_base<-paste(path,'/simplex_gen_dist_',slope,sep='')
dir.create(file_name_base)
#simulation params
age <-1
hyb_fxn<-make.beta.draw(1,1)



lambda_func<-function() 4
mu_func<-function() 2
nu_func<-function() 0.1

##Make a data.frame with the hybrid proportions
nlayers<-11
npoints<-61

hyb_pos<-rep(NA,npoints)
hyb_neu<-rep(NA,npoints)
hyb_neg<-rep(NA,npoints)

i<-0
layer_space<-1/(nlayers-1)
for(div in seq(from=-1,to=1,length.out=nlayers)){
  leftover<-1-abs(div)
  
  hyb_zero<-seq(0,leftover,by=layer_space)
  
  leftover_leftover<-leftover-hyb_zero
  hyb_plus<- (div+1-hyb_zero)/2
  hyb_minus<-hyb_plus-div
  
  nvals<-length(hyb_minus)
  
  hyb_pos[(i+1):(i+nvals)]<-hyb_plus
  hyb_neg[(i+1):(i+nvals)]<-hyb_minus
  hyb_neu[(i+1):(i+nvals)]<-hyb_zero
  
  i<-i+nvals
}
dat<-data.frame(hyb_pos,hyb_neg,hyb_neu)

# save.image(file=paste(file_name_base,'/parameters.Rdata',sep=''))


for(rw in 1:nrow(dat)){
  file_name<-paste(file_name_base,'/row_',rw,sep = '')
  dir.create(file_name)
  
  my_props<-dat[rw,]
  
  hyb_prop_func<-function() my_props
  # save.image(file=paste(file_name,'/parameters.Rdata',sep=''))
  # source('./sim_runs.R')
  source('./net_analysis.R')
  
}


######
##Simulate under the whole gambit of hybrid proportions with varying sampling fraction
######sampling fraction of 0.75

library(SiPhyNetwork)
nreps<-10000
timeout_time<-15
path<-'../data'

rho <- 0.75

file_name_base<-paste(path,'/simplex_sampling_frac_',rho,sep='')
dir.create(file_name_base)
#simulation params
age <-1
hyb_fxn<-make.beta.draw(1,1)



lambda_func<-function() 4
mu_func<-function() 2
nu_func<-function() 0.1

##Make a data.frame with the hybrid proportions
nlayers<-11
npoints<-61

hyb_pos<-rep(NA,npoints)
hyb_neu<-rep(NA,npoints)
hyb_neg<-rep(NA,npoints)

i<-0
layer_space<-1/(nlayers-1)
for(div in seq(from=-1,to=1,length.out=nlayers)){
  leftover<-1-abs(div)
  
  hyb_zero<-seq(0,leftover,by=layer_space)
  
  leftover_leftover<-leftover-hyb_zero
  hyb_plus<- (div+1-hyb_zero)/2
  hyb_minus<-hyb_plus-div
  
  nvals<-length(hyb_minus)
  
  hyb_pos[(i+1):(i+nvals)]<-hyb_plus
  hyb_neg[(i+1):(i+nvals)]<-hyb_minus
  hyb_neu[(i+1):(i+nvals)]<-hyb_zero
  
  i<-i+nvals
}
dat<-data.frame(hyb_pos,hyb_neg,hyb_neu)

save.image(file=paste(file_name_base,'/parameters.Rdata',sep=''))


for(rw in 59:nrow(dat)){
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

library(SiPhyNetwork)
nreps<-100000
timeout_time<-15
path<-'../data'

hyb_prop_func<-function() c(1/3,1/3,1/3)

file_name_base<-paste(path,'/all_equal_hybs_sampling_frac',sep='')
dir.create(file_name_base)
#simulation params
age <-1
hyb_fxn<-make.beta.draw(1,1)



lambda_func<-function() 4
mu_func<-function() 2
nu_func<-function() 0.1

##make a vector with the sampling fraction
fracs <- seq(0.25,1,by=0.025)

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

library(SiPhyNetwork)
nreps<-10000
timeout_time<-15
path<-'../data'

hyb_prop_func<-function() c(1/3,1/3,1/3)

file_name_base<-paste(path,'/all_equal_hybs_gen_dist',sep='')
dir.create(file_name_base)
#simulation params
age <-1
hyb_fxn<-make.beta.draw(1,1)

rho<-1

lambda_func<-function() 4
mu_func<-function() 2
nu_func<-function() 0.1

# slopes<-seq(0.25,2,by=0.05)
slopes<-seq(1.4,2,by=0.05)
save.image(file=paste(file_name_base,'/parameters.Rdata',sep=''))


for(slope in slopes){
  file_name<-paste(file_name_base,'/dist_',slope,sep = '')
  dir.create(file_name)
  
  gen_dist<-make.linear.decay(1/slope)
  
  
  save.image(file=paste(file_name,'/parameters.Rdata',sep=''))
  source('./sim_runs.R')
  source('./net_analysis.R') ##sampling only concerns us with reconstructed phys
  
}


