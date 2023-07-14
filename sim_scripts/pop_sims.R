library(GillespieSSA)

dir_ext<-'./pop_sims'
dir.create(dir_ext)

tf <- 10 # Final time
simName <- "Logistic growth" 
x0 <- c(N = 2)
nu <- matrix(c(+1, -1),ncol = 2)
a  <- c('b*N+h*N*(N-1)/2', "d*N+e*N*(N-1)/2")

n_timeouts<-5000
nreps_cond<-5000


nu_hyb<- 0.2
s<-seq(-0.1,0.1,by=0.01)
# s<-seq(0,0.1,by=0.01)
write.csv(s,file = paste(dir_ext,'/s_vals.csv',sep=''))

n_int<-1000 ## Number of intervals for census
timeout_count<-0

pops<-list()
pops_cond<-list()
for(i in 1:length(s)){
#for(i in 1:5){
  div<-s[i]
  nu_neg<- (nu_hyb-div)/2
  nu_pos<-nu_hyb-nu_neg
  parms <- c(b = 4, d = 3, h=nu_pos,e=nu_neg) 
  
  
  ##Running average of population sizes
  ave_pop<-rep(0,n_int)
  n_ave <-rep(0,n_int)
  
  ##Running average of population sizes conditioned on survival
  ave_pop_cond<-rep(0,n_int)
  n_ave_cond <-rep(0,n_int)
  
  timeout_count <-0
  while(n_ave_cond[n_int]<=nreps_cond & timeout_count< n_timeouts){
    print(c(s=div,cond_reps=n_ave_cond[n_int],timeout=timeout_count))
    out <- ssa(
      x0 = x0,
      a = a,
      nu = nu,
      parms = parms,
      tf = tf,
      method = ssa.d(),
      simName = simName,
      verbose = FALSE,
      maxWallTime = 4
    ) 
    
    ##Convert the simulation to appropriate intervals
    out$data<-rbind(out$data,c(Inf,NA)) ##Add arbitrary end
    times<-seq(0,tf,length.out=n_int) ## time intervals we want to record
    valid_times_ind <- rep(TRUE, length(times)) ##Assume all times are present in the simulation
    
    if('maxWallTime' %in% out$stats$terminationStatus){ ## if the simulation didn't finish
      out_last<-out$data[nrow(out$data)-1,1]
      ##truncate times such that it only goes until the last recorded time
      valid_times_ind <-(times<=out_last) ##These are the time periods where the simulation actually ran
      times<-times[valid_times_ind]
      timeout_count<-timeout_count+1
    }
    pop_size<- c(2,rep(NA,sum(valid_times_ind)-1))
    dat_ind<-1
    dat_time<-out$data[dat_ind,1]
    for(ind in 2:length(times)){
      t <- times[ind]
      while(t>dat_time){ ##Move dat_ind to the next event before 't'
        dat_ind<-dat_ind+1
        dat_time<-out$data[dat_ind,1]
      }
      pop_size[ind]<-out$data[dat_ind-1,2] ##Get the population before the dat_ind event
      
      if(pop_size[ind]==0){ ##if current pop size is 0 then so will the rest of it
        pop_size[ind:length(times)]<- 0
        break
      }
    }

    ## Compute running average at each time point
    cond<-valid_times_ind
    n_ave[cond]<-n_ave[cond]+1
    x1 <- 1/n_ave[cond]
    x2 <- 1 - x1
    ave_pop[cond] <- (x1 * pop_size[cond]) + (x2 * ave_pop[cond])
    
    ##Compute the running average conditioned on survival
    cond<-(pop_size>0 )
    n_ave_cond[valid_times_ind][cond]<-(n_ave_cond[valid_times_ind])[cond]+1
    x1 <- 1/n_ave_cond[valid_times_ind][cond]
    x2 <- 1 - x1
    ave_pop_cond[valid_times_ind][cond] <- ((x1 * pop_size[cond]) + (x2 * ave_pop_cond[valid_times_ind][cond]))
  }
  pops[[i]]<-ave_pop
  pops_cond[[i]]<-ave_pop_cond
  times<-seq(0,tf,length.out=n_int)
  write.csv(x= data.frame(times=times,
                          pop=pops[[i]],
                          reps=n_ave),
            file = paste(dir_ext,'/pops_',i,'.csv',sep=''))
  write.csv(x= data.frame(times=times,
                          pop=pops_cond[[i]],
                          reps=n_ave_cond),
            file = paste(dir_ext,'/pops_cond_',i,'.csv',sep=''))
}
 





########################################
### Double Time Negative Simulations ###
########################################

## Simulations are the same as above, Except:
    ##We only simulate the negative 's' values -0.1 to -0.01
    ##The time of the simulation is doubled from 10 to 20





dir_ext<-'../data/pop_sims_double_time'
dir.create(dir_ext)

tf <- 40 # Final time
simName <- "Logistic growth" 
x0 <- c(N = 2)
nu <- matrix(c(+1, -1),ncol = 2)
a  <- c('b*N+h*N*(N-1)/2', "d*N+e*N*(N-1)/2")

n_timeouts<-5000
nreps_cond<-5000


nu_hyb<- 0.2
s<-seq(-0.1,-0.03,by=0.01)
# s<-seq(0,0.1,by=0.01)
write.csv(s,file = paste(dir_ext,'/s_vals.csv',sep=''))

n_int<-1000 ## Number of intervals for census
timeout_count<-0

pops<-list()
pops_cond<-list()
for(i in 1:length(s)){
  #for(i in 1:5){
  div<-s[i]
  nu_neg<- (nu_hyb-div)/2
  nu_pos<-nu_hyb-nu_neg
  parms <- c(b = 4, d = 3, h=nu_pos,e=nu_neg) 
  
  
  ##Running average of population sizes
  ave_pop<-rep(0,n_int)
  n_ave <-rep(0,n_int)
  
  ##Running average of population sizes conditioned on survival
  ave_pop_cond<-rep(0,n_int)
  n_ave_cond <-rep(0,n_int)
  
  timeout_count <-0
  while(n_ave_cond[n_int]<=nreps_cond & timeout_count< n_timeouts){
    print(c(s=div,cond_reps=n_ave_cond[n_int],timeout=timeout_count))
    out <- ssa(
      x0 = x0,
      a = a,
      nu = nu,
      parms = parms,
      tf = tf,
      method = ssa.d(),
      simName = simName,
      verbose = FALSE,
      maxWallTime = 5
    ) 
    
    ##Convert the simulation to appropriate intervals
    out$data<-rbind(out$data,c(Inf,NA)) ##Add arbitrary end
    times<-seq(0,tf,length.out=n_int) ## time intervals we want to record
    valid_times_ind <- rep(TRUE, length(times)) ##Assume all times are present in the simulation
    
    if('maxWallTime' %in% out$stats$terminationStatus){ ## if the simulation didn't finish
      out_last<-out$data[nrow(out$data)-1,1]
      ##truncate times such that it only goes until the last recorded time
      valid_times_ind <-(times<=out_last) ##These are the time periods where the simulation actually ran
      times<-times[valid_times_ind]
      timeout_count<-timeout_count+1
    }
    pop_size<- c(2,rep(NA,sum(valid_times_ind)-1))
    dat_ind<-1
    dat_time<-out$data[dat_ind,1]
    for(ind in 2:length(times)){
      t <- times[ind]
      while(t>dat_time){ ##Move dat_ind to the next event before 't'
        dat_ind<-dat_ind+1
        dat_time<-out$data[dat_ind,1]
      }
      pop_size[ind]<-out$data[dat_ind-1,2] ##Get the population before the dat_ind event
      
      if(pop_size[ind]==0){ ##if current pop size is 0 then so will the rest of it
        pop_size[ind:length(times)]<- 0
        break
      }
    }
    
    ## Compute running average at each time point
    cond<-valid_times_ind
    n_ave[cond]<-n_ave[cond]+1
    x1 <- 1/n_ave[cond]
    x2 <- 1 - x1
    ave_pop[cond] <- (x1 * pop_size[cond]) + (x2 * ave_pop[cond])
    
    ##Compute the running average conditioned on survival
    cond<-(pop_size>0 )
    n_ave_cond[valid_times_ind][cond]<-(n_ave_cond[valid_times_ind])[cond]+1
    x1 <- 1/n_ave_cond[valid_times_ind][cond]
    x2 <- 1 - x1
    ave_pop_cond[valid_times_ind][cond] <- ((x1 * pop_size[cond]) + (x2 * ave_pop_cond[valid_times_ind][cond]))
  }
  pops[[i]]<-ave_pop
  pops_cond[[i]]<-ave_pop_cond
  times<-seq(0,tf,length.out=n_int)
  write.csv(x= data.frame(times=times,
                          pop=pops[[i]],
                          reps=n_ave),
            file = paste(dir_ext,'/pops_',i,'.csv',sep=''))
  write.csv(x= data.frame(times=times,
                          pop=pops_cond[[i]],
                          reps=n_ave_cond),
            file = paste(dir_ext,'/pops_cond_',i,'.csv',sep=''))
}













