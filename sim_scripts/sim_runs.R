library(SiPhyNetwork)
library(R.utils)
library(ape)



##Efficiently growing list
##https://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time-o1
linkedList <- function() {
  head <- list(0)
  length <- 0

  methods <- list()

  methods$add <- function(val) {
    length <<- length + 1
    head <<- list(head, val)
  }

  methods$as.list <- function() {
    b <- vector('list', length)
    h <- head
    for(i in length:1) {
      b[[i]] <- head[[2]]
      head <- head[[1]]
    }
    return(b)
  }
  methods
}



##set up recording ##elements
status<-linkedList()
mus<-linkedList()
lambdas<-linkedList()
nus<-linkedList()
hybprops<-linkedList()
phys<-linkedList()

##deal with variables that may or may not have been specified
if(!exists('rho')){ ##sampling fraction
  rho<-1 ##if not specified then assume a value of 1
}
if(!exists('gen_dist')){ ##Genetic Distance dependence 
  gen_dist <- NULL
}

ncomplete<-0
while(ncomplete < nreps){
  mu<-mu_func()
  lambda<-lambda_func()
  nu<-nu_func()
  hyb_prop<-hyb_prop_func()




  tryCatch(
    expr={
      withTimeout(
        expr ={
          net<- sim.bdh.age(numbsim = 1, age=age,
                            lambda = lambda, mu=mu,
                            nu = nu, hybprops = hyb_prop,
                            hyb.inher.fxn = hyb_fxn,
                            frac = rho,mrca = T,
                            complete = T,
                            hyb.rate.fxn = gen_dist)[[1]]
        },timeout = timeout_time,
        onTimeout = "error"
      )


      if('evonet' %in% class(net)){ ##Check whether the simulation resulted in a phylogeny
        status$add(1)
        phys$add(net)
        ncomplete<-ncomplete+1
        print(ncomplete)

      } else{
        status$add(2)
      }
      ##save prior results
      mus$add(mu)
      lambdas$add(lambda)
      nus$add(nu)
      hybprops$add(hyb_prop)


    },
    error=function(e){
      status$add(3)

      ##save prior results
      mus$add(mu)
      lambdas$add(lambda)
      nus$add(nu)
      hybprops$add(hyb_prop)
    }
  )
}
rm(mu,nu,lambda,hyb_prop,net,ncomplete)


lambdas<-unlist(lambdas$as.list())
mus<-unlist(mus$as.list())
nus<-unlist(nus$as.list())
hybprops<-(hybprops$as.list())
nu_pos<-unlist(lapply(hybprops, function(x) x[1]))
nu_neg<-unlist(lapply(hybprops, function(x) x[2]))
nu_zero<-unlist(lapply(hybprops, function(x) x[3]))

status<-unlist(status$as.list()) #result of each run. 1 if good. 2 if extinct/one species. 3 if timeout

run_data<-data.frame(status,lambdas,mus,nus,nu_pos,nu_neg,nu_zero)

rm(lambdas,mus,nus,hybprops,nu_pos,nu_neg,nu_zero,status)


##Now record certain aspects of the phylogeny
phys<-phys$as.list()
empty_col<-rep(NA,nrow(run_data)) ##used to make a column with recorded info
rets<-(unlist(lapply(phys,function(x) nrow(x$reticulation))))
  empty_col[run_data$status==1]<-rets
  rets<-empty_col
tips<-unlist(lapply(phys,function(x) length(x$tip.label)))
  empty_col[run_data$status==1]<-tips
  tips<-empty_col



# ##Record classes of networks
# level<-unlist(lapply(phys,getNetworkLevel))
#   empty_col[run_data$status==1]<-level
#   level<-empty_col
# tree_child<-unlist(lapply(phys,isTreeChild))
#   empty_col[run_data$status==1]<-tree_child
#   tree_child<-empty_col
# tree_based<-unlist(lapply(phys,isTreeBased))
#   empty_col[run_data$status==1]<-tree_based
#   tree_based<-empty_col
# #fu_stable <-unlist(lapply(phys,isFUstable))
# #  empty_col[run_data$status==1]<-fu_stable
# #  fu_stable<-empty_col


##record

#run_data<-cbind(run_data,tips,rets,level,tree_child,tree_based)
run_data<-cbind(run_data,tips,rets)
#rm(tips,rets,level,tree_child,tree_based)
rm(tips,rets)

library(fst)
write.fst(run_data,paste(file_name,'/sim_data.fst',sep=''),compress = 100)
saveRDS(phys,paste(file_name,'/phys.rds',sep=''))
#write.net(nets,paste(path,'/',file_name,'_phys.tree',sep=''))

rm(run_data)
rm(phys,empty_col)

