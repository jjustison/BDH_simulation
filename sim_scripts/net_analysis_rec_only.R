get_hyb_nos<-function(phy,tol=1e-8){
  
  if(nrow(phy$reticulation)>0){ ##only go thru all the work if we have reticulations
    node_times<-node.depth.edgelength(phy)
    hyb_nodes<-phy$reticulation[,2]
    edges<-rbind(phy$edge,phy$reticulation) ##put all edges together in a single data.frame
    
    pos_hyb<-0
    neg_hyb<-0
    neu_hyb<-0
    
    
    for(nd in hyb_nodes){
      hyb_time <- node_times[nd]
      hyb_parents<-edges[(edges[,2]==nd),1]
      if(length(hyb_parents)!=2){
        stop('something went wrong')
      }
      
      parent_times<-node_times[hyb_parents]
      same_times<- sum(abs(parent_times-hyb_time) < tol) ##count the number of parent nodes that occur at the same time as the hybrid node
      
      if(same_times==0){ ##degenerative hybridization
        neg_hyb <- neg_hyb+1
      } else if(same_times==1){ ##Neutral hybridization
        neu_hyb <- neu_hyb+1
      } else if(same_times==2){ ## Generative Hybridization
        pos_hyb <- pos_hyb+1
      } else{
        stop('something went wrong 2: Attack of the Clones')
      }
    }
    val<- c(pos_hyb,neg_hyb,neu_hyb)
    if( sum(val) != nrow(phy$reticulation)){
      stop('something went wrong 3: The Return of the King')
    }
    
  } else{ ##There are no hybridizations. Report NA for each of the counts
    val<-rep(NA,3)
  }
  return(val)
}

library(SiPhyNetwork)
print(paste(file_name,'phy analysis'))

phys<-readRDS(paste(file_name,'/phys.rds',sep=''))
col_names<-c('rets','tips','level','tree_child','tree_based','fu_stable','normal','gen_no','degen_no','neu_no')
my_dat<-data.frame(matrix(NA,nrow = length(phys),ncol = length(col_names)))
colnames(my_dat)<-col_names

###Obtain the Reconstructed network and record summary stats
phys<-lapply(phys,reconstructedNetwork)

##Now record certain aspects of the phylogeny
my_dat$rets<-(unlist(lapply(phys,function(x) nrow(x$reticulation))))
my_dat$tips<-unlist(lapply(phys,function(x) length(x$tip.label)))

##Record the types of hybridization that are observed
my_dat[,c('gen_no','degen_no','neu_no')] <- do.call(rbind,lapply(phys,get_hyb_nos))

##Record classes of networks
my_dat$level<-unlist(lapply(phys,getNetworkLevel))
my_dat$tree_child<-unlist(lapply(phys,isTreeChild))
my_dat$tree_based<-my_dat$tree_child ##Anything that is tree child is also tree based
my_dat$tree_based[!my_dat$tree_based]<-unlist(lapply(phys[!my_dat$tree_based],isTreeBased)) ##Check the remaining networks to see if they're tree based
my_dat$fu_stable[my_dat$tree_child]  <- T ##Anything that is tree child is also fu stable
my_dat$fu_stable[!my_dat$tree_based] <- F ##Anything that is NOT tree based will NOT be fu stable
my_dat$fu_stable[is.na(my_dat$fu_stable)] <- unlist(lapply(phys[is.na(my_dat$fu_stable)],isFUstable)) ##Check the remaining Networks
my_dat$normal[!my_dat$normal] <- F ##Anything that is NOT tree child will NOT be normal
my_dat$normal[is.na(my_dat$normal)] <- unlist(lapply(phys[is.na(my_dat$normal)],isNormal)) ##Check the remaining Networks



write.csv(my_dat,file = paste(file_name,'/phydata.csv',sep=''))
rm(my_dat,phys)



