############################
### Load 4 core datasets ###
############################
library(tidyr)
library(dplyr)

path<-'../data'


##Make data frame with coordinates on simplex

## Make the simplex data frame - dat
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
simp_dat<-data.frame(hyb_pos,hyb_neg,hyb_neu)


#################################################################
### make generic functions for compiling and summarizing data ###
#################################################################

compile_dataset<-function(paths,iters,iterName="iter",hasComplete=T,nreps=20000){
  nsims<-length(paths)
  
  recon_mult<-(hasComplete+1)
  col_names<- c('rets'	,'tips','level','tree_child','tree_based','fu_stable','normal','gen_no','degen_no','neu_no','recon',iterName)
  my_frame<-as.data.frame(matrix(NA,nrow = nreps*recon_mult*nsims,ncol=length(col_names)))
  colnames(my_frame)<- col_names
  count<-0
  
  sims<-1:nsims
  for(sim in sims){
    pth<-paths[sim]
    it<-iters[sim]
    file_name<-paste(pth,'/phydata.csv',sep = '')
    
    dat <- read.csv(file =file_name)[,-1]
    nrows<-nreps
    
    if(hasComplete){ ##split reconstructed data if needed
      to_bind<-dat[,11:20]
      dat<-dat[,-(11:20)]
      colnames(to_bind)<-colnames(dat)
      dat<-rbind(dat,to_bind)
    }
    
    recon<-c(rep(F,hasComplete*nrows),rep(T,nrows))
    row_row<-rep(it,recon_mult*nrows)
    
    dat<-cbind(dat,recon,row_row)
    
    my_frame[((count*recon_mult*nrows)+1):((count+1)*recon_mult*nrows),]<-dat
    count<-count+1
  }
  my_frame$rets[is.nan(my_frame$rets)]<-0
  my_frame<-as_tibble(my_frame)
  return(my_frame)
}

summarize_data<-function(my_frame,returnRecon,iter='iter',filterRets=F){
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
  simp_dat<-data.frame(hyb_pos,hyb_neg,hyb_neu)
  
  
  
  
  if(filterRets){
    my_frame<- my_frame %>% filter(rets!=0)
  }
  summed<- my_frame %>%
    group_by_at(c(iter,'recon')) %>%
    summarize(tc = sum(tree_child)/n(),
              tb = sum(tree_based)/n(),
              fus= sum(fu_stable)/n(),
              nor= sum(normal)/n(),
              ratio = mean(rets/tips),
              ave_rets = mean(rets,na.rm=T),
              rets0 = sum(rets==0)/n(),
              tips = mean(tips),
              ave_level = mean(level,na.rm=T),
              level1 = sum(level<=1)/n(),
              gen_prop = mean((gen_no/rets),na.rm=T),
              degen_prop = mean((degen_no/rets),na.rm=T),
              neu_prop = mean((neu_no/rets),na.rm=T)
              # g2 = sum(gen_no,na.rm = T)/sum(rets,na.rm=T),
              # d2 = sum(degen_no,na.rm=T)/sum(rets,na.rm=T),
              # n2 = sum(neu_no,na.rm=T)/sum(rets,na.rm=T)
              )
  if(iter=='row'){
    simp_vals<-simp_dat[summed$row,] ##Add the hyb prop rates to df
    row.names(simp_vals)<-NULL
    summed<- cbind(summed,simp_vals)
  }
  dat<- summed%>% filter(recon==returnRecon)
  return(dat)
}

##############################################
##### Load Simplex and the recon simplex #####
##############################################

rows<-1:61
simplex_paths<-paths<-paste('../data/simplex/row_',rows,sep='')

simp_frame<-compile_dataset(paths=simplex_paths,iters=rows,iterName="row",hasComplete = T)

# simp_props<-simp_frame %>%
#   group_by(recon) %>%
#   mutate(ret0= sum(rets==0)/n(),
#          level1 = sum(level<=1)/n()) %>%
#   select(c("tree_child","tree_based","fu_stable","normal",'ret0','level1','recon')) %>%
#   summarise(across(everything(),list(mean=mean,min=min,max=max))) %>%
#   mutate(ID=c('c','r')) %>%
#   select(-1)


c_dat<-summarize_data(my_frame = simp_frame, returnRecon = F, iter='row')
r_dat<-summarize_data(my_frame = simp_frame, returnRecon = T, iter='row')

filt_c <-summarize_data(my_frame = simp_frame, returnRecon = F, iter='row',filterRets = T)
filt_r <-summarize_data(my_frame = simp_frame, returnRecon = T, iter='row',filterRets = T)


##################################################
####### Load recon with incomplete sampling ######
##################################################

rows<-1:61
simplex_paths<-paste('../data/simplex_sampling_frac_0.75/row_',rows,sep='')

my_frame<-compile_dataset(paths=simplex_paths,iters=rows,iterName="row",hasComplete = T)





r_i_dat<-summarize_data(my_frame = my_frame, returnRecon = T, iter='row')
filt_r_i <-summarize_data(my_frame = my_frame, returnRecon = T, iter='row',filterRets = T)



rm(my_frame)

#############################################
###### Load Complete with gen dist dep ######
#############################################

rows<-1:61
simplex_paths<-paste('../data/simplex_gen_dist_2/row_',rows,sep='')

my_frame<-compile_dataset(paths=simplex_paths,iters=rows,iterName="row",hasComplete = T)

c_gd_dat<-summarize_data(my_frame = my_frame, returnRecon = F, iter='row')
filt_c_gd <-summarize_data(my_frame = my_frame, returnRecon = F, iter='row',filterRets = T)
rm(my_frame)

############################################
##### Load incomplete Sampling Dataset #####
############################################

fracs<-seq(0.25,0.95,by=0.025)
incom_paths<-paste('../data/all_equal_hybs_sampling_frac/frac_',fracs,sep='')

incom_frame<-compile_dataset(paths=incom_paths,iters=fracs,iterName="frac",hasComplete = F)

if_dat  <-summarize_data(my_frame = incom_frame, returnRecon = T, iter='frac')
filt_if <-summarize_data(my_frame = incom_frame, returnRecon = T, iter='frac',filterRets = T)



#################################
##### Load Gen dist Dataset #####
#################################

dists<-seq(0,2,by=0.05)
incom_paths<-paste('../data/all_equal_hybs_gen_dist/dist_',dists,sep='')

gd_frame<-compile_dataset(paths=incom_paths,iters=dists,iterName="stre",hasComplete = T)

gd_dat<-summarize_data(my_frame = gd_frame, returnRecon = F, iter='stre')
filt_gd <-summarize_data(my_frame = gd_frame, returnRecon = F, iter='stre',filterRets = T)



