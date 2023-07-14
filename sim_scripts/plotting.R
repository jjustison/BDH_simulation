
library(ggtern)


#write.table(dat,'simplex.txt')


#############################
## Table for simplex props ##
#############################
rownames(simp_dat)<-NULL
xtable(simp_dat)



#################################
##Base Simplex Plot with points##
#################################

ggtern(data = simp_dat,mapping=aes(x=hyb_pos,y=hyb_neg,z=hyb_neu))+
  geom_point(cex=5) +
  
  theme_custom(
    col.T = '#DDCC77',
    col.L = '#CC6677',
    col.R = '#AA4499',
  )+
  theme_nomask() +
  theme(tern.axis.arrow = element_line(size = 4),
        tern.panel.grid.minor.T = element_line(color = "gray80"),
        tern.panel.grid.major.T = element_line(color = "gray80"),
        tern.panel.grid.minor.L = element_line(color = "gray80"),
        tern.panel.grid.major.L = element_line(color = "gray80"),
        tern.panel.grid.minor.R = element_line(color = "gray80"),
        tern.panel.grid.major.R = element_line(color = "gray80"),
        tern.panel.background = element_rect(fill = "gray92"),
        axis.title = element_text(size=25,face="bold"),
        tern.axis.text=element_text(size=13.5),
        tern.axis.text.T=element_text(hjust=1,angle=-30),
        tern.axis.text.R=element_text(hjust=1.5,angle=-90),
        tern.axis.text.L=element_text(hjust=0.15,angle=30),
        tern.axis.arrow.text=element_text(size=25,face="bold"),
        )+
  theme_arrowlarge()+
  labs(x = expression(nu['+']), xarrow='$\\nu_+$',
       z = expression(nu['0']), zarrow='$\\nu_0$',
       y = expression(nu['-']), yarrow='$\\nu_-',
  ) + 
  scale_L_continuous(breaks = 0:5 / 5, labels = 0:5/ 50) +
  scale_R_continuous(breaks = 0:5 / 5, labels = 0:5/ 50) +
  scale_T_continuous(breaks = 0:5 / 5, labels = 0:5/ 50) +
  theme_latex(TRUE)+
  theme_rotate(210)
  
#################################
## ggtern points plot function ##
#################################

make.ggtern.points<-function(dat,cols){
  tern.plot<-ggtern(data = dat,mapping=aes(x=hyb_pos,y=hyb_neg,z=hyb_neu))+
    geom_point(cex=15,color=cols) +
    
    theme_custom(
      col.T = '#DDCC77',
      col.L = '#CC6677',
      col.R = '#AA4499',
    )+
    theme_nomask() +
    theme(tern.axis.arrow = element_line(size = 4),
          tern.panel.grid.minor.T = element_line(color = "gray80"),
          tern.panel.grid.major.T = element_line(color = "gray80"),
          tern.panel.grid.minor.L = element_line(color = "gray80"),
          tern.panel.grid.major.L = element_line(color = "gray80"),
          tern.panel.grid.minor.R = element_line(color = "gray80"),
          tern.panel.grid.major.R = element_line(color = "gray80"),
          tern.panel.background = element_rect(fill = "gray92"),
          axis.title = element_text(size=25,face="bold"),
          tern.axis.text=element_text(size=13.5),
          tern.axis.text.T=element_text(hjust=1.25,vjust=1.5,angle=-30,size=15), ##yellow
          tern.axis.text.R=element_text(hjust=0.25,vjust=-2.5,angle=-90,size=15), ##purple
          tern.axis.text.L=element_text(hjust= -0.2,angle=30,size=15), ##red
          tern.axis.arrow.text=element_text(size=25,face="bold"),
    )+
    theme_arrowlarge()+
    labs(x = expression(nu['+']), xarrow='$\\nu_+$',
         z = expression(nu['0']), zarrow='$\\nu_0$',
         y = expression(nu['-']), yarrow='$\\nu_-',
    ) + 
    scale_L_continuous(breaks = 0:5 / 5, labels = 0:5/ 50) +
    scale_R_continuous(breaks = 0:5 / 5, labels = 0:5/ 50) +
    scale_T_continuous(breaks = 0:5 / 5, labels = 0:5/ 50) +
    theme_latex(TRUE)+
    theme_rotate(210)
  return(tern.plot)
}

############################
## plot n save all 4 terns##
############################

just_n_save<-function(file_name){
  ggsave(paste(file_name,'.png',sep=''),width = 10,height = 10,dpi=600)
}

plot_n_save<-function(dsets,dset_names,dir_suffix='',vari,legend_title,lower_q=NULL,upper_q=NULL,digits=3){
  
  pal<-(rcartocolor::carto_pal(7,'Emrld'))
  cols_pal<- colorRampPalette(
    colors = pal)
  point_cols<- cols_pal(20)
  
  dummy_frame<-data.frame(x=1:20,y=1:20,point_cols,point_vals=rep(NA,20))
  
  #### compile all values in one place to get decent range and breaks 
  all_vals<-unlist(lapply(dsets, function(x) x[,vari]))
  dir_name<-paste('./Figures/simplex_',vari,'_plots',dir_suffix,sep='')
  
  
  min_val<-min(all_vals)-(1e-10)
  max_val<-max(all_vals)+(1e-10)
  smallest<-NULL
  largest <-NULL
  l_out <- 21 ##The number of boundaries for our color pallete
  if(!is.null(lower_q)){
    smallest<-min_val
    min_val<-quantile(all_vals,lower_q)
    l_out<-l_out-1
  }
  if(!is.null(upper_q)){
    largest<-max_val
    max_val<-quantile(all_vals,upper_q)
    l_out<-l_out-1
  }
  
  
  val_breaks<-c(smallest,seq(min_val,max_val,length.out=l_out),largest)
  dummy_frame$point_vals<-val_breaks[-1]
  dummy_frame$point_vals[nrow(dummy_frame)]<-dummy_frame$point_vals[1]

  legend_plot<- ggplot(data=dummy_frame,mapping=aes(x=x,y=y,fill=point_vals),limits=c(min_val,max_val))+
    geom_point()+
    scale_fill_gradientn(colours=point_cols,
                         breaks=round(seq(min_val,max_val,length.out=5),digits = digits),
                         limits=round(c(min_val,max_val),digits=digits))+
    theme(
      legend.title=element_text(size=25),
      legend.key.size = unit(0.1,'npc'),
      legend.text = element_text(size=20))
  legend_plot$labels$fill<- legend_title
  legend_plot<-ggdraw(get_legend(legend_plot))
  

  dir.create(dir_name)
  
  print(legend_plot)
  ggsave(paste(dir_name,'/',vari,'_legend.png',sep=''),width = 10,height = 10,dpi=600)
   
  for(i in 1:length(dsets)){
    dset<-dsets[[i]]
    dset_name<-dset_names[i]
    
    dset_cols <-point_cols[as.numeric(cut(unlist(dset[,vari]),breaks=val_breaks))]
    dset_plot <-make.ggtern.points(dset,dset_cols)
    
    print(dset_plot)
    ggsave(paste(dir_name,'/',dset_name,'_',vari,'.png',sep=''),width = 10,height = 10,dpi=600)
  }
 }





##############################
##Line plots with changing s##
##############################

cols_pal<- colorRampPalette(
  colors = c('#88CCEE','#44AA99','#117733'))
line_col<-cols_pal(9)

xvals<-c(c(0,0.1),c(0,0.2),c(0,0.3),c(0,0.4),c(0,0.5),c(0.2,0.6),c(0.4,0.7),c(0.6,0.8),c(0.8,0.9))
yvals<-c(c(0.8,0.9),c(0.6,0.8),c(0.4,0.7),c(0.2,0.6),c(0,0.5),c(0,0.4),c(0,0.3),c(0,0.2),c(0,0.1))
zvals<-c(c(0.2,0),c(0.4,0),c(0.6,0),c(0.8,0),c(1,0),c(0.8,0),c(0.6,0),c(0.4,0),c(0.2,0))

coors<- data.frame(x1=xvals,y1=yvals,z1=zvals)
gps  <- paste('s =', coors$x1-coors$y1)
gps  <- factor(gps,levels = rev(unique(gps))) 
coors$gps<-gps

line_plot<-ggtern(data =coors,aes(x=x1,y=y1,z=z1,fill=gps))+
  geom_line(aes(color=as.factor(gps)),size=8)+
  scale_color_manual(values=rev(line_col))+
  theme_custom(
    col.T = '#DDCC77',
    col.L = '#CC6677',
    col.R = '#AA4499',
  )+
  theme(tern.axis.arrow = element_line(size = 4),
        tern.panel.grid.minor.T = element_line(color = "gray80"),
        tern.panel.grid.major.T = element_line(color = "gray80"),
        tern.panel.grid.minor.L = element_line(color = "gray80"),
        tern.panel.grid.major.L = element_line(color = "gray80"),
        tern.panel.grid.minor.R = element_line(color = "gray80"),
        tern.panel.grid.major.R = element_line(color = "gray80"),
        tern.panel.background = element_rect(fill = "gray92"),
        axis.title = element_text(size=25,face="bold"),
        tern.axis.text=element_text(size=13.5),
        tern.axis.text.T=element_text(hjust=1,angle=-30),
        tern.axis.text.R=element_text(hjust=1.5,angle=-90),
        tern.axis.text.L=element_text(hjust=0.15,angle=30),
        tern.axis.arrow.text=element_text(size=25,face="bold"),
        
  )+
  theme_arrowlarge()+
  labs(x = expression(nu['+']), xarrow='$\\nu_+$',
       z = expression(nu['0']), zarrow='$\\nu_0$',
       y = expression(nu['-']), yarrow='$\\nu_-',
  ) +
  scale_L_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  scale_R_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  scale_T_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  theme_latex(TRUE) +
  theme_rotate(210) +
  
  ##Just Legend Things
  theme(legend.key.size = unit(0.1,'npc'),
        legend.text = element_text(size=20),
        legend.title= element_text(size=25)
  )+
  guides(color=guide_legend(title= expression("s = ("*nu['+']*- nu['-']*')')))

line_plot
  

########################
## Generate LTT plots ##
########################

## NOTE! THESE PLOTS ARE NO LONGER USED
## The function for expected ltt is not correct


# #function for N(T)
# ltt <- function(time,r,s,n0=2){
#   numer <- r-s
#   denom1<- (n0*s+r-s)/n0
#   denom2<-exp(-(r-s)*time)
#   denom<-denom1*denom2-s
#   lineages<-numer/denom
#   return(lineages)
# }
# 
# times<-seq(0,1,by=0.0002)
# divs <-seq(-0.8,0.8,by=0.2)
# ltt_vals<-matrix(nrow = length(times)*length(divs),ncol = 3,data = NA)
# for(i in 1:length(divs)){
#   s<-divs[i]
#   ltt_vals[((i-1)*length(times)+1):((i)*length(times)),1]<-times
#   ltt_vals[((i-1)*length(times)+1):((i)*length(times)),2]<-ltt(times,12,s)
#   ltt_vals[((i-1)*length(times)+1):((i)*length(times)),3]<-rep(s,length(times))
# }
# ltt_vals<-as.data.frame(ltt_vals)
# colnames(ltt_vals)<-c('time','lineages','s')
# ltt_vals<-ltt_vals[ltt_vals$lineages>=0,]
# ltt_vals$s<-(ltt_vals$s)
# 
# ltt.plot<-ggplot(data=ltt_vals,mapping=aes(x=time,y=lineages,fill=factor(s,levels = (unique(s)))))+
#   geom_line() + xlim(0,1) +ylim(0,150) +
#   geom_line(aes(color=factor(s,levels = rev(unique(s)))),size=4)+
#   scale_color_manual(values=rev(line_col)) +
#   theme(
#    #legend.position = 'none',
#     legend.title=element_text(size=15),
#     legend.key.size = unit(0.1,'npc'),
#     legend.text = element_text(size=15),
#     axis.title = element_text(size=15))+
#   xlab("Time") +
#   ylab("Number of Lineages N(t)")+
#   guides(color=guide_legend(title= expression("s = ("*nu['+']*- nu['-']*')')))
# 
# 
# ltt.plot

#################################
## Div rate as function of N ####
#################################
library(reshape2)
N_vals<-seq(2,100,by=0.05)


rates<- data.frame(N_vals,hyb_rate=choose(N_vals,2)*1,
                   spec_rate=N_vals*20,
                   exct_rate=N_vals*8)

net_rates<-data.frame(N_vals,r=rates$spec_rate-rates$exct_rate)
divs <-seq(-0.8,0.8,by=0.2)
for(div in divs){
  net_rates[,as.character(div)]<-rates$hyb_rate*div*2
}
net_rates<-melt(net_rates,id=c('N_vals','r'),variable.name = c('s'))
net_rates$total<-net_rates$r+net_rates$value

rates$tot_rate<- rates$hyb_rate+rates$spec_rate+rates$exct_rate
melty_rates<- data.frame(N=rep(rates$N_vals,4),rates=c(unlist(rates[,2:5])),gps=c(rep('hyb',nrow(rates)),rep('spec',nrow(rates)),rep('exct',nrow(rates)),rep('tot',nrow(rates))))

ggplot(data=melty_rates, mapping = aes(x=N,y=rates,color=gps))+
  geom_line(mapping=aes(linetype=gps),size=2)+
  scale_color_discrete(name="Rates",labels=c(expression(mu),expression(nu),expression(lambda),'Total'))+
  scale_linetype_discrete(name='Rates',labels=c(expression(mu),expression(nu),expression(lambda),'Total'))+
  xlab("Number of Lineages")+
  ylab("Rate") +
  theme(legend.title=element_text(size=15),
        legend.key.size = unit(0.1,'npc'),
        legend.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.key.width = unit(0.1,'npc'),
        legend.position = c(0.15,0.7))

ggplot(data=net_rates,mapping = aes(x=N_vals,y=total,fill=s))+
  geom_hline(yintercept=0,size=2) +
  geom_line(mapping=aes(color=s),size=4)+
  scale_color_manual(values=(line_col))+
  theme(legend.position = 'none')+
  ylim(-200,800)+
  xlab('Number of Lineages')+
  ylab("Total Diversification Rate")
  


########################
## Div Rate over time ##
########################

hyb_div <-rep(NA,nrow(ltt_vals))
spec_div<-rep(NA,nrow(ltt_vals))


ltt_vals$hyb_div<- 2*choose(ltt_vals$lineages,2)*as.numeric(ltt_vals$s)
ltt_vals$bd_div <- ltt_vals$lineages*12
ltt_vals$tot_div<- ltt_vals$hyb_div +ltt_vals$bd_div


##################################################
##############  Load Core Datasets ###############
##################################################

source('Summarize_data.R')

##########################################################
##########################################################

library(rcartocolor)
library(cowplot)
library(gridGraphics)
pal<-(rcartocolor::carto_pal(7,'Emrld'))
cols_pal<- colorRampPalette(
  colors = pal)
point_cols<- cols_pal(20)

dummy_frame<-data.frame(x=1:20,y=1:20,point_cols,point_vals=rep(NA,20))

######################################
### Plot Tree-based of 2 core sets ###
######################################

min_val<-min(c_dat$tb,r_dat$tb)
max_val<-max(c_dat$tb,r_dat$tb)

val_breaks<-c(0,seq(min_val,max_val,length.out=20))
dummy_frame$point_vals<-val_breaks[-1]

c_cols  <-point_cols[as.numeric(cut(c_dat$tb,breaks=val_breaks))]
r_cols  <-point_cols[as.numeric(cut(r_dat$tb,breaks=val_breaks))]
r_i_cols<-point_cols[as.numeric(cut(r_i_dat$tb,breaks=val_breaks))]


c_tb_plot  <-make.ggtern.points(c_dat,c_cols)
r_tb_plot  <-make.ggtern.points(r_dat,r_cols)
r_i_tb_plot<-make.ggtern.points(r_i_dat,r_i_cols)
legend_plot<- ggplot(data=dummy_frame,mapping=aes(x=x,y=y,fill=point_vals))+geom_point()+scale_fill_gradientn(colours=point_cols)+
  theme(
    legend.title=element_text(size=25),
    legend.key.size = unit(0.1,'npc'),
    legend.text = element_text(size=20))
legend_plot$labels$fill<- "Proportion"
legend_plot<-ggdraw(get_legend(legend_plot))



c_tb_plot
ggsave('c_tb.png',width = 10,height = 10,dpi=600)

r_tb_plot
ggsave('r_tb.png',width = 10,height = 10,dpi=600)

r_i_tb_plot


legend_plot
grid.arrange(c_tb_plot,r_tb_plot,r_i_tb_plot,ncol=3)

######################################
### Plot FU Stable of 2 core sets ###
######################################

all_vals<-c(c_dat$fus,r_dat$fus)
summary(all_vals)
min_val<-min(all_vals)
max_val<-max(all_vals)

# val_breaks<-c(0,seq(min_val,0.80,length.out=20))
val_breaks<-c(0,seq(min_val,0.80,length.out=19),1)
dummy_frame$point_vals<-val_breaks[-1]

c_cols  <-point_cols[as.numeric(cut(c_dat$fus,breaks=val_breaks))]
r_cols  <-point_cols[as.numeric(cut(r_dat$fus,breaks=val_breaks))]
r_i_cols<-point_cols[as.numeric(cut(r_i_dat$fus,breaks=val_breaks))]


c_fus_plot  <-make.ggtern.points(c_dat,c_cols)
r_fus_plot  <-make.ggtern.points(r_dat,r_cols)
r_i_fus_plot<-make.ggtern.points(r_i_dat,r_i_cols)
legend_plot<- ggplot(data=dummy_frame,mapping=aes(x=x,y=y,fill=point_vals))+geom_point()+scale_fill_gradientn(colours=point_cols)+
  theme(
    legend.title=element_text(size=25),
    legend.key.size = unit(0.1,'npc'),
    legend.text = element_text(size=20))
legend_plot$labels$fill<- "Proportion"
legend_plot<-ggdraw(get_legend(legend_plot))
  

c_fus_plot
ggsave('c_fus.png',width = 10,height = 10,dpi=600)

r_fus_plot
ggsave('r_fus.png',width = 10,height = 10,dpi=600)

r_i_fus_plot


legend_plot
grid.arrange(c_fus_plot,r_fus_plot,r_i_fus_plot,ncol=3)

######################################
### Plot Tree Child of 2 core sets ###
######################################
all_vals<-c(c_dat$tc,r_dat$tc)
summary(all_vals)
min_val<-min(all_vals)
max_val<-max(all_vals)

val_breaks<-c(0,seq(min_val,0.75,length.out=19),1)
# val_breaks<-c(0,seq(min_val,0.75,length.out=20))
dummy_frame$point_vals<-val_breaks[-1]

c_cols  <-point_cols[as.numeric(cut(c_dat$tc,breaks=val_breaks))]
r_cols  <-point_cols[as.numeric(cut(r_dat$tc,breaks=val_breaks))]
r_i_cols<-point_cols[as.numeric(cut(r_i_dat$tc,breaks=val_breaks))]


c_tc_plot  <-make.ggtern.points(c_dat,c_cols)
r_tc_plot  <-make.ggtern.points(r_dat,r_cols)
r_i_tc_plot<-make.ggtern.points(r_i_dat,r_i_cols)
legend_plot<- ggplot(data=dummy_frame,mapping=aes(x=x,y=y,fill=point_vals))+geom_point()+scale_fill_gradientn(colours=point_cols)+
  theme(
    legend.title=element_text(size=25),
    legend.key.size = unit(0.1,'npc'),
    legend.text = element_text(size=20))
legend_plot$labels$fill<- "Proportion"
legend_plot<-ggdraw(get_legend(legend_plot))


c_tc_plot
ggsave('c_tc.png',width = 10,height = 10,dpi=600)

r_tc_plot
ggsave('r_tc.png',width = 10,height = 10,dpi=600)

r_i_tc_plot


legend_plot
grid.arrange(c_tc_plot,r_tc_plot,r_i_tc_plot,ncol=3)

###################################
### Plot Normal of 2 core sets  ###
###################################
all_vals<-c(c_dat$nor,r_dat$nor)
summary(all_vals)
min_val<-min(all_vals)
max_val<-max(all_vals)

val_breaks<-c(0,seq(min_val,0.65,length.out=19),1)
# val_breaks<-c(0,seq(min_val,0.65,length.out=20))
dummy_frame$point_vals<-val_breaks[-1]

c_cols  <-point_cols[as.numeric(cut(c_dat$nor,breaks=val_breaks))]
r_cols  <-point_cols[as.numeric(cut(r_dat$nor,breaks=val_breaks))]
r_i_cols<-point_cols[as.numeric(cut(r_i_dat$nor,breaks=val_breaks))]


c_nor_plot  <-make.ggtern.points(c_dat,c_cols)
r_nor_plot  <-make.ggtern.points(r_dat,r_cols)
r_i_nor_plot<-make.ggtern.points(r_i_dat,r_i_cols)
legend_plot<- ggplot(data=dummy_frame,mapping=aes(x=x,y=y,fill=point_vals))+geom_point()+scale_fill_gradientn(colours=point_cols)+
  theme(
    legend.title=element_text(size=25),
    legend.key.size = unit(0.1,'npc'),
    legend.text = element_text(size=20))
legend_plot$labels$fill<- "Proportion"
legend_plot<-ggdraw(get_legend(legend_plot))


c_nor_plot
ggsave('c_nor.png',width = 10,height = 10,dpi=600)

r_nor_plot
ggsave('r_nor.png',width = 10,height = 10,dpi=600)

r_i_nor_plot


legend_plot
grid.arrange(c_nor_plot,r_nor_plot,r_i_nor_plot,ncol=3)


#####################
### Plot all vars ###
#####################




vars<-colnames(c_dat)[3:12]

titles<-c('Proportion','Average')
sig_no<-c(3,0)
gp<-c(1,1,1,1,2,2,1,2,2,1)

for(i in 1:length(vars)){
  nm<-vars[i]
  print(nm)
  plot_n_save(dsets=list(c_dat,r_dat,r_i_dat,c_gd_dat),
              dset_names = c('c','r','r_i','c_gd'),
              vari = nm,
              legend_title = titles[gp[i]],
              dir_suffix='',
              digits = sig_no[gp[i]],
              lower_q = 0.05,
              upper_q = 0.95)

  # plot_n_save(dsets=list(c_dat,r_dat),
  #             dset_names = c('c','r','r_i','c_gd'),
  #             vari = nm,
  #             legend_title = titles[gp[i]],
  #             dir_suffix='_core' ,
  #             digits = sig_no[gp[i]],
  #             lower_q = 0.025,
  #             upper_q = 0.975)
  
}



###########################
### Make summary tables ###
###########################

##Add identifier for each dataset
c_dat$ID<-rep('Complete',nrow(c_dat))
r_dat$ID<-rep('Extant-Only',nrow(c_dat))
c_gd_dat$ID<-rep('Distance Dependence',nrow(c_dat))
r_i_dat$ID<-rep('Incomplete Sampling',nrow(c_dat))

all_dat<-rbind(c_dat,r_dat,c_gd_dat,r_i_dat)
all_dat<-all_dat[,-c(1,2,16,17,18)] ##Get rid of unnessecary columns

all_sum <- all_dat %>%
  group_by(ID) %>%
  summarise(across(everything(),list(mean=mean,var=var,min=min,max=max)))

div_sum <- all_sum[,c(1,18:33)]

xtable(class_sum<-all_sum[,c(1,6:13,2:5,14:17)])



###########################################
### Plot recon vs complete hybrid props ###
###########################################

prop_dat<- rbind(c_dat[,c('row','recon','gen_prop','degen_prop','neu_prop')],r_dat[,c('row','recon','gen_prop','degen_prop','neu_prop')])

ggtern(prop_dat, mapping=aes(x=gen_prop,y=degen_prop,z=neu_prop))+
  geom_point(mapping=aes(color=recon),size=5)+
  geom_path(mapping=aes(group=row),arrow = arrow(length = unit(0.3, "cm"),type='closed'),size=1.75)+
  theme_custom(
    col.T = '#DDCC77',
    col.L = '#CC6677',
    col.R = '#AA4499',
  )+
  theme_nomask() +
  theme(tern.axis.arrow = element_line(size = 4),
        tern.panel.grid.minor.T = element_line(color = "gray80"),
        tern.panel.grid.major.T = element_line(color = "gray80"),
        tern.panel.grid.minor.L = element_line(color = "gray80"),
        tern.panel.grid.major.L = element_line(color = "gray80"),
        tern.panel.grid.minor.R = element_line(color = "gray80"),
        tern.panel.grid.major.R = element_line(color = "gray80"),
        tern.panel.background = element_rect(fill = "gray92"),
        axis.title = element_text(size=25,face="bold"),
        tern.axis.text=element_text(size=13.5),
        tern.axis.text.T=element_text(hjust=1,angle=-30),
        tern.axis.text.R=element_text(hjust=1.5,angle=-90),
        tern.axis.text.L=element_text(hjust=0.15,angle=30),
        tern.axis.arrow.text=element_text(size=25,face="bold"),
        legend.title=element_text(size=25),
        
        legend.text = element_text(size=20)
  )+
  theme_arrowlarge()+
  labs(x = 'm-type', xarrow='Proportion',
       z = 'n-type', zarrow='Proportion',
       y = 'y-type', yarrow='Proportion',
       color='Sampling'
  ) + 
  scale_L_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  scale_R_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  scale_T_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  scale_color_discrete(labels=c('Complete', 'Extant-only'))+
  theme_latex(TRUE)+
  theme_rotate(210)

just_n_save('c_r_props')


###################
### Level Plots ###
###################

ggplot(simp_frame[(!simp_frame$recon) & (simp_frame$rets<=10),],mapping=aes(x=rets,level,group=rets))+
  geom_violin(adjust=2)+scale_y_continuous(breaks=1:10)+
  scale_x_continuous(breaks=1:10)+
  labs(x='Number of Reticulations',y="Level")+
  theme(text =  element_text(size=20))
just_n_save('level_per_ret10')

ave_levl <- simp_frame[(!simp_frame$recon),] %>%
  group_by(rets) %>%
  summarize(lvl = mean(level))

ggplot(ave_levl,mapping=aes(x=rets,lvl))+
  geom_point()+
  labs(x='Number of Reticulations',y=" Average Level")+
  theme(text =  element_text(size=20))
just_n_save('level_per_ret_all')


##############################################################
##############################################################
######                                                  ######
###### Genetic Distance and Incomplete Sampling Section ######
######                                                  ######
##############################################################
##############################################################




################################
### Load incomplete sampling ###
################################


gathered_if<- if_dat %>% 
  select(frac,ave_rets,rets0,ratio,tb,tc,fus,nor) %>%
  gather(key=class,value=prop,-c(frac,ave_rets,ratio,rets0))


incom_by_rets<-incom_frame %>% 
  filter(rets<=10) %>%
  group_by(frac,rets) %>%
  summarize(tb = sum(tree_based)/n(),
            fus= sum(fu_stable)/n(),
            tc = sum(tree_child)/n(),
            nor= sum(normal)/n())


incom_dir<-'incom_plots'
dir.create(incom_dir)



ggplot(data=gathered_if, mapping = aes(x=frac,y=prop,color=factor(class,levels = c('tb','fus','tc','nor'))))+
  geom_point(size=5) +
  scale_color_discrete(name='Class',labels=c('Tree-Based','FU-Stable','Tree-Child','Normal'))+
  xlab(expression('Sampling Fraction '*rho))+
  ylab('Proportion in class')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(incom_dir,'/incon_props.png',sep=''),width = 10,height = 10,dpi=600)



ggplot(data=gathered_if, mapping = aes(x=frac,y=rets0))+
  geom_line(size=4) +
  xlab(expression('Sampling Fraction '*rho))+
  ylab('Proportion without Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(incom_dir,'/incon_ret0.png',sep=''),width = 10,height = 10,dpi=600)

ggplot(data=gathered_if, mapping = aes(x=frac,y=ave_rets))+
  geom_line(size=4) +
  xlab(expression('Sampling Fraction '*rho))+
  ylab('Average Number of Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(incom_dir,'/incon_rets.png',sep=''),width = 10,height = 10,dpi=600)

ggplot(data=gathered_if, mapping = aes(x=frac,y=ratio))+
  geom_line(size=4) +
  xlab(expression('Sampling Fraction '*rho))+
  ylab('Average Reticulation Density')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(incom_dir,'/incon_ratio.png',sep=''),width = 10,height = 10,dpi=600)


ggplot(data=incom_by_rets,mapping=aes(x=rets,y=tb,color=factor(frac,levels = sort(unique(frac)))))+
  geom_line(size=2)+
  scale_color_viridis_d(name='Sampling Fraction')+
  ylab('Proportion in Class')+
  xlab('Number of Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(incom_dir,'/incon_ret_tb.png',sep=''),width = 10,height = 10,dpi=600)

ggplot(data=incom_by_rets,mapping=aes(x=rets,y=fus,color=factor(frac,levels = sort(unique(frac)))))+
  geom_line(size=2)+
  scale_color_viridis_d(name='Sampling Fraction')+
  ylab('Proportion in Class')+
  xlab('Number of Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(incom_dir,'/incon_ret_fus.png',sep=''),width = 10,height = 10,dpi=600)

ggplot(data=incom_by_rets,mapping=aes(x=rets,y=tc,color=factor(frac,levels = sort(unique(frac)))))+
  geom_line(size=2)+
  scale_color_viridis_d(name='Sampling Fraction')+
  ylab('Proportion in Class')+
  xlab('Number of Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(incom_dir,'/incon_ret_tc.png',sep=''),width = 10,height = 10,dpi=600)

ggplot(data=incom_by_rets,mapping=aes(x=rets,y=nor,color=factor(frac,levels = sort(unique(frac)))))+
  geom_line(size=2)+
  scale_color_viridis_d(name='Sampling Fraction')+
  ylab('Proportion in Class')+
  xlab('Number of Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(incom_dir,'/incon_ret_nor.png',sep=''),width = 10,height = 10,dpi=600)


if_prop<- (if_dat[,c('frac','gen_prop','degen_prop','neu_prop')])


temp_dat<-data.frame(xx=1/3,yy=1/3,zz=1/3)
ggtern(if_prop, mapping=aes(x=gen_prop,y=degen_prop,z=neu_prop))+
  geom_point(mapping=ggtern::aes(color=frac),size=6)+
  geom_point(temp_dat,mapping=aes(x=xx,y=yy,z=zz),size=8,show.legend = F,shape='diamond')+
  theme_custom(
    col.T = '#DDCC77',
    col.L = '#CC6677',
    col.R = '#AA4499',
  )+
  theme_nomask() +
  theme(tern.axis.arrow = element_line(size = 4),
        tern.panel.grid.minor.T = element_line(color = "gray80"),
        tern.panel.grid.major.T = element_line(color = "gray80"),
        tern.panel.grid.minor.L = element_line(color = "gray80"),
        tern.panel.grid.major.L = element_line(color = "gray80"),
        tern.panel.grid.minor.R = element_line(color = "gray80"),
        tern.panel.grid.major.R = element_line(color = "gray80"),
        tern.panel.background = element_rect(fill = "gray92"),
        axis.title = element_text(size=25,face="bold"),
        tern.axis.text=element_text(size=13.5),
        tern.axis.text.T=element_text(hjust=1,angle=-30),
        tern.axis.text.R=element_text(hjust=1.5,angle=-90),
        tern.axis.text.L=element_text(hjust=0.15,angle=30),
        tern.axis.arrow.text=element_text(size=25,face="bold"),
        legend.title=element_text(size=25),
        
        legend.text = element_text(size=20)
  )+
  theme_arrowlarge()+
  labs(x = 'm-type', xarrow='Proportion',
       z = 'n-type', zarrow='Proportion',
       y = 'y-type', yarrow='Proportion',
       color='Sampling \n Fraction'
  ) + 
  scale_L_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  scale_R_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  scale_T_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  theme_latex(TRUE)+
  theme_rotate(210)

just_n_save('sampling_frac_props')

#############################
### Load Genetic Distance ###
#############################

gathered_gd<- gd_dat %>% 
  select(stre,ave_rets,rets0,ratio,tb,tc,fus,nor) %>%
  gather(key=class,value=prop,-c(stre,ave_rets,ratio,rets0))



gd_by_rets<-gd_frame %>% 
  filter(rets<=10) %>%
  group_by(rets,stre) %>%
  summarize(tb = sum(tree_based)/n(),
            fus= sum(fu_stable)/n(),
            tc = sum(tree_child)/n(),
            nor= sum(normal)/n())


gd_dir<-'gd_plots'
dir.create(gd_dir)






ggplot(data=gathered_gd, mapping = aes(x=stre,y=prop,color=factor(class,levels = c('tb','fus','tc','nor'))))+
  geom_point(size=5) +
  scale_color_discrete(name='Class',labels=c('Tree-Based','FU-Stable','Tree-Child','Normal'))+
  xlab(expression('Strength of Distance Dependence '*delta))+
  ylab('Proportion in class')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(gd_dir,'/gd_props.png',sep=''),width = 10,height = 10,dpi=600)



ggplot(data=gathered_gd, mapping = aes(x=stre,y=rets0))+
  geom_line(size=4) +
  xlab(expression('Distance Dependence Strength '*delta))+
  ylab('Proportion without Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(gd_dir,'/gd_ret0.png',sep=''),width = 10,height = 10,dpi=600)

ggplot(data=gathered_gd, mapping = aes(x=stre,y=ave_rets))+
  geom_line(size=4) +
  xlab(expression('Distance Dependence Strength '*delta))+
  ylab('Average Number of Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(gd_dir,'/gd_rets.png',sep=''),width = 10,height = 10,dpi=600)

ggplot(data=gathered_gd, mapping = aes(x=stre,y=ratio))+
  geom_line(size=4) +
  xlab(expression('Distance Dependence Strength '*delta))+
  ylab('Average Reticulation Density')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(gd_dir,'/gd_ratio.png',sep=''),width = 10,height = 10,dpi=600)


ggplot(data=gd_by_rets,mapping=aes(x=rets,y=tb,color=factor(stre,levels = sort(unique(stre)))))+
  geom_line(size=2)+
  scale_color_viridis_d(name='Distance Dependence')+
  ylab('Proportion in Class')+
  xlab('Number of Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(gd_dir,'/gd_ret_tb.png',sep=''),width = 10,height = 10,dpi=600)

ggplot(data=gd_by_rets,mapping=aes(x=rets,y=fus,color=factor(stre,levels = sort(unique(stre)))))+
  geom_line(size=2)+
  scale_color_viridis_d(name='Distance Dependence')+
  ylab('Proportion in Class')+
  xlab('Number of Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(gd_dir,'/gd_ret_fus.png',sep=''),width = 10,height = 10,dpi=600)

ggplot(data=gd_by_rets,mapping=aes(x=rets,y=tc,color=factor(stre,levels = sort(unique(stre)))))+
  geom_line(size=2)+
  scale_color_viridis_d(name='Distance Dependence')+
  ylab('Proportion in Class')+
  xlab('Number of Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(gd_dir,'/gd_ret_tc.png',sep=''),width = 10,height = 10,dpi=600)

ggplot(data=gd_by_rets,mapping=aes(x=rets,y=nor,color=factor(stre,levels = sort(unique(stre)))))+
  geom_line(size=2)+
  scale_color_viridis_d(name='Distance Dependence')+
  ylab('Proportion in Class')+
  xlab('Number of Reticulations')+
  theme(
    legend.title = element_text(size=30), #change legend title font size
    legend.text = element_text(size=20),
    axis.title=element_text(size=30,face="bold"))
ggsave(paste(gd_dir,'/gd_ret_nor.png',sep=''),width = 10,height = 10,dpi=600)

