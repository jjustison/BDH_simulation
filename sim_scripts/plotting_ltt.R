library(ggplot2)
library(tidyr)
path<- '../data/pop_sims'



##Make empty data.frames to populate the data
nrows_dat<-1000 ## The number of timepoints that we recorded the population size
temp_col<-rep(NA,nrows_dat)
ave_pop <- data.frame(time=nrows_dat,
                      size=nrows_dat,
                      reps=nrows_dat,
                      div=nrows_dat)
ave_cond_pop <- data.frame(time=nrows_dat,
                      size=nrows_dat,
                      reps=nrows_dat,
                      div=nrows_dat)

s_vals<-read.csv(file=paste(path,'/s_vals.csv',sep = ''))

for(i in s_vals[,1]){
  s<-s_vals[i,2]
  
  dat<-read.csv(file=paste(path,'/pops_',i,'.csv',sep=''))
  dat<-cbind(dat,div=rep(s,nrow(dat)))
  ave_pop[(1+(nrows_dat*(i-1))):(nrows_dat*i),]<-dat[,2:5]
  
  dat<-read.csv(file=paste(path,'/pops_cond_',i,'.csv',sep=''))
  dat<-cbind(dat,div=rep(s,nrow(dat)))
  ave_cond_pop[(1+(nrows_dat*(i-1))):(nrows_dat*i),]<-dat[,2:5]
  
  
}

my_dat<- ave_cond_pop %>%
  #dplyr::filter(div>=0) %>%
  dplyr::filter(reps>4500)



cols_pal<- colorRampPalette(
  colors = c('#88CCEE','#44AA99','#117733'))
line_col<-cols_pal(21)


ltt.plot<-ggplot(data=my_dat,mapping=aes(x=time,y=size,group=div))+
  xlim(0,10) +ylim(0,180) +
  geom_line(aes(color=div),size=2)+
  scale_color_gradient2(low='#88CCEE',mid='#44AA99',high = '#117733') +
  theme(
    #legend.position = 'none',
    legend.title=element_text(size=15),
    legend.key.size = unit(0.1,'npc'),
    legend.text = element_text(size=15),
    axis.title = element_text(size=15))+
  xlab("Time") +
  ylab("Average Number of Lineages")+
  guides(color=guide_colorbar(title= expression("s = ("*nu['+']*- nu['-']*')')),
         fill='none')


leg <- ggdraw(get_legend(ltt.plot))

ltt.plot<-ltt.plot+theme(legend.position='none')

ltt.plot
ggsave('cond_ltt.png',width = 10,height = 10,dpi=600)

leg
ggsave('cond_ltt_legend.png',width = 10,height = 10,dpi=600)

library(cowplot)



my_dat <- ave_pop %>%
  dplyr::filter(reps>4500)
ltt.plot<-ggplot(data=my_dat,mapping=aes(x=time,y=size,group=div))+
  xlim(0,10) +ylim(0,100) +
  geom_line(aes(color=div),size=2)+
  scale_color_gradient2(low='#88CCEE',mid='#44AA99',high = '#117733') +
  theme(
    #legend.position = 'none',
    legend.title=element_text(size=15),
    legend.key.size = unit(0.1,'npc'),
    legend.text = element_text(size=15),
    axis.title = element_text(size=15))+
  xlab("Time") +
  ylab("Average Number of Lineages")+
  guides(color=guide_colorbar(title= expression("s = ("*nu['+']*- nu['-']*')')),
         fill='none')

leg <- ggdraw(get_legend(ltt.plot))

ltt.plot<-ltt.plot+theme(legend.position='none')

ltt.plot
ggsave('ave_ltt.png',width = 10,height = 10,dpi=600)

leg
ggsave('ave_ltt_legend.png',width = 10,height = 10,dpi=600)













path<- '../data/pop_sims_double_time'

##Make empty data.frames to populate the data
nrows_dat<-1000 ## The number of timepoints that we recorded the population size
temp_col<-rep(NA,nrows_dat)
d_ave_pop <- data.frame(time=nrows_dat,
                      size=nrows_dat,
                      reps=nrows_dat,
                      div=nrows_dat)
d_ave_cond_pop <- data.frame(time=nrows_dat,
                           size=nrows_dat,
                           reps=nrows_dat,
                           div=nrows_dat)

s_vals<-read.csv(file=paste(path,'/s_vals.csv',sep = ''))

for(i in s_vals[,1]){
  s<-s_vals[i,2]
  
  dat<-read.csv(file=paste(path,'/pops_',i,'.csv',sep=''))
  dat<-cbind(dat,div=rep(s,nrow(dat)))
  d_ave_pop[(1+(nrows_dat*(i-1))):(nrows_dat*i),]<-dat[,2:5]
  
  dat<-read.csv(file=paste(path,'/pops_cond_',i,'.csv',sep=''))
  dat<-cbind(dat,div=rep(s,nrow(dat)))
  d_ave_cond_pop[(1+(nrows_dat*(i-1))):(nrows_dat*i),]<-dat[,2:5]
}




my_dat <- d_ave_pop %>%
  dplyr::filter(reps>4500)
ltt.plot<-ggplot(data=my_dat,mapping=aes(x=time,y=size,group=div))+
  xlim(0,40) +ylim(0,25) +
  geom_line(aes(color=div),size=2)+
  scale_color_gradient2(low='#88CCEE',mid='#44AA99',high = '#117733') +
  theme(
    #legend.position = 'none',
    legend.title=element_text(size=15),
    legend.key.size = unit(0.1,'npc'),
    legend.text = element_text(size=15),
    axis.title = element_text(size=15))+
  xlab("Time") +
  ylab("Average Number of Lineages")+
  guides(color=guide_colorbar(title= expression("s = ("*nu['+']*- nu['-']*')')),
         fill='none')

leg<-ggdraw(get_legend(ltt.plot))
ltt.plot<-ltt.plot+theme(legend.position = 'none')


ltt.plot
ggsave('ave_ltt_quad_time.png',width = 10,height = 10,dpi=600)

leg
ggsave('ave_ltt_quad_time_legend.png',width = 10,height = 10,dpi=600)




