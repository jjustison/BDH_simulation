library(Ternary)
library(tidyr)
library(dplyr)
path<-'../data'
file_name_base<-paste(path,'/all_equal_hybs_sampling_frac/',sep='')

fracs <- seq(0.25,1,by=0.025)
nsims<-length(fracs) ##The total number of different simulations


col_names<- c('rets','tips','level','tree_child','tree_based','fu_stable','gen_no','degen_no','neu_no','recon','row_num')
summed_dat<-as.data.frame(matrix(NA,nrow = nsims,ncol=length(col_names)))


count<-0
for(frac in fracs){
  file_name<-paste(file_name_base,'frac_',frac,'/phydata.csv',sep = '')
  
  
  
  dat <- read.csv(file =file_name)[,-1]

  nreps<-nrow(dat)
  
  summed_dat$frac[frac]<-frac
  summed_dat$tc[frac]<-sum(dat$tree_child)/nreps
  summed_dat$tb[frac]<-sum(dat$tree_based)/nreps
  summed_dat$fus[frac]<-sum(dat$fu_stable)/nreps
  summed_dat$ratio[frac]<- mean(dat$rets/dat$tips)
  summed_dat$tips[frac]<-mean(dat$tips)
  summed_dat$rets[frac]<-mean(dat$rets)
  summed_dat$level[frac]<-mean(dat$level)
  summed_dat$level1[frac]<-sum(dat$level>1)/nreps
  
}


my_frame$rets[is.nan(my_frame$rets)]<-0
my_frame<-as_tibble(my_frame)

summed<- my_frame %>% 
  group_by(row_num,recon) %>%
  summarize(tc = sum(tree_child)/n(),
            tb = sum(tree_based)/n(),
            fus= sum(fu_stable)/n(),
            ratio = mean(rets/tips),
            tips = mean(tips),
            gen = mean(gen_no/rets,na.rm=T),
            degen = mean(degen_no/rets,na.rm=T),
            neu = mean(neu_no/rets,na.rm=T))
recon_dat <- summed %>% filter(recon==T)
complete_dat <- summed %>% filter(recon==F)


plot(recon_dat$row_num,recon_dat$tc)
plot()

