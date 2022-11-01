library(SiPhyNetwork)

#Simulation settings

nreps<-20000
timeout_time<-10
path<-'../data'

#simulation params
age <-1

lambda_func<-function() 4
mu_func<-function() 2
nu_func<-function() 0.1

hyb_fxn<-make.beta.draw(10,10)


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
