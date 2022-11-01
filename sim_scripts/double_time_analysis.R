library(fst)
file_name_base<-paste('../data/simplex',sep='')



gen_dist<-NULL

source('./common_pars.R') ##Load common parameters

all_runs<-as.data.frame(matrix(dat=NA,nrow=nrow(dat),ncol=3,))
colnames(all_runs)<-c('1','2','3') 

for(rw in 1:nrow(dat)){
  print(rw)
  file_name<-paste(file_name_base,'/row_',rw,'/sim_data.fst',sep = '')
  run_fst<-read.fst(file_name)
  run_stats<-table(run_fst$status)
  
  statuses<-c('1','2','3')
  for(st in statuses){
    if(!(st %in% names(run_stats))){
      run_stats[st]<-0
    }
  }
  
  all_runs[rw,]<-run_stats
  
}


## look at double time results
file_name_base<-paste('../data/simplex_double_time',sep='')
rw<-61
file_name<-paste(file_name_base,'/row_',rw,'/sim_data.fst',sep = '')
run_fst<-read.fst(file_name)
run_stats<-table(run_fst$status)



