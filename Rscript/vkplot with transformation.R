

som_all_fin=fread("Datafile/SOM_sel.csv")

som_all=fread("Datafile/SOM_1st.csv")
som_all$Sample=paste(som_all$Type,som_all$Temp,som_all$Incubation,sep = "_")
som_all

tt=unique(som_all[,c("Formula","Sample")])
tt

table(tt$Sample)


som_all$Freq=1

som_all_cnt=aggregate(som_all$Freq,by=list(Sample=som_all$Sample,Formula=som_all$Formula),sum)

som_all_cnt_sel=subset(som_all_cnt,som_all_cnt$x>1)
length(unique(som_all_cnt_sel$Formula)) ##0<10505, 1< 7010 (selected), 2< 5906





som_all

fm_info=unique(som_all[,c("Formula","Calc.m/z","O/C","H/C","C#","H#","N#","O#","DBE","AI","Comp","Molecularclass")])


vk_data=som_all_cnt_sel %>% inner_join(fm_info)

dim(som_all_cnt_sel)

vk_data=vk_data %>% separate(Sample, c("Type","Temp","Incubation"),sep = "_")

vk_data$Sample=paste(vk_data$Type,vk_data$Temp,vk_data$Incubation, sep = "_")

ggplot(vk_data, aes(x=`O/C`, y=`H/C`))+
  geom_point(size=1.5, alpha=0.5)+
  stat_density2d(aes(fill=..level..), geom = "polygon",colour="black")+
  scale_fill_distiller(palette = "YlOrBr",direction = 1)+
  facet_rep_grid(Temp~Incubation)



trans_num_all=fread("Datafile/Num.Peak.Trans_Allms.csv")

trans_num_sel=trans_num_all[,c("peak","num.trans.involved.in")]
trans_num_sel=trans_num_sel %>% `colnames<-`(c("Calc.m/z","num.trans"))


vk_data2=vk_data %>% inner_join(trans_num_sel)

vk_data_re=subset(vk_data2,vk_data2$Temp!="Ctrl") %>% droplevels()
vk_data_ctr=subset(vk_data2,vk_data2$Temp=="Ctrl")

vk_data_ctr_5=vk_data_ctr
vk_data_ctr_5$Temp=5
vk_data_ctr_15=vk_data_ctr
vk_data_ctr_15$Temp=15

vk_data_ctr_25=vk_data_ctr
vk_data_ctr_25$Temp=25

vk_data_re2=rbind(vk_data_ctr_5,vk_data_ctr_15,vk_data_ctr_25,vk_data_re)

table(vk_data_re2$Temp)
table(vk_data_re2$Incubation)
vk_data_re2$Temp=factor(vk_data_re2$Temp,levels = c("0","5","15","25"))
vk_data_re2$Incubation=factor(vk_data_re2$Incubation,levels = c("0","70","154","223","336"))


vk_data_re2$activity=ifelse(vk_data_re2$num.trans>25, "Active","Inactive")

vk_data_re2$ord=ifelse(vk_data_re2$Comp=="CHO",1,
                       ifelse(vk_data_re2$Comp=="CHON",2,
                              ifelse(vk_data_re2$Comp=="CHOS",3,
                                     ifelse(vk_data_re2$Comp=="CHONS",4,5))))


vk_data_al=subset(vk_data_re2,vk_data_re2$Type=="AL")
vk_data_pf=subset(vk_data_re2,vk_data_re2$Type=="PF")

vk_data_al=vk_data_al[order(vk_data_al$num.trans),]
vk_data_al=vk_data_al[order(vk_data_al$ord),]

cs=topo.colors(80)[c(5,70)]

vk_data_al

ggplot(vk_data_al, aes(x=`O/C`, y=`H/C`, col=Comp))+
#  geom_hline(yintercept = 1.5, lty=2)+
  geom_point(size=1.5, alpha=0.5)+
  scale_color_manual(values =  c("#BC3C29FF","#EFC000FF","#008B45FF","#5F559BFF","grey50"))+
  facet_rep_grid(Temp~Incubation, repeat.tick.labels = T)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold", colour = "black"),
        axis.title = element_text(size = 15, face = "bold", colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 16, face = "bold", colour = "black"),
        legend.text = element_text(size = 12,colour = "black"))+
  ggsave(filename("vk_comp_al"),height = 30, width = 55, units = "cm", dpi = 700)

vk_data_pf=vk_data_pf[order(vk_data_pf$ord),]

ggplot(vk_data_pf, aes(x=`O/C`, y=`H/C`, col=Comp))+
  #  geom_hline(yintercept = 1.5, lty=2)+
  geom_point(size=1.5, alpha=0.5)+
  scale_color_manual(values =  c("#BC3C29FF","#EFC000FF","#008B45FF","#5F559BFF","grey50"))+
  facet_rep_grid(Temp~Incubation, repeat.tick.labels = T)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold", colour = "black"),
        axis.title = element_text(size = 15, face = "bold", colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 16, face = "bold", colour = "black"),
        legend.text = element_text(size = 12,colour = "black"))+
  ggsave(filename("vk_comp_pf"),height = 30, width = 55, units = "cm", dpi = 700)



vk_data_re2$activity=ifelse(vk_data_re2$num.trans>25, "Active",
                            ifelse(vk_data_re2$num.trans<8,"Inactive", "Intermediate active"))

vk_data_re2$activity=factor(vk_data_re2$activity, levels = c("Active","Intermediate active","Inactive"),
                            labels = c("Active (> 24)",
                                       "Semi active (6 ~ 24)",
                                       "Inactive (< 5)"))

vk_data_re2$ord2=ifelse(vk_data_re2$activity=="Active (> 24)",2,
                        ifelse(vk_data_re2$activity=="Inactive (< 5)",3,1))

vk_data_al=subset(vk_data_re2,vk_data_re2$Type=="AL")
vk_data_pf=subset(vk_data_re2,vk_data_re2$Type=="PF")

cs=topo.colors(80)[c(65,15)]

vk_data_al=vk_data_al[order(vk_data_al$ord2,decreasing = F),]

#ggplot(vk_data_al, aes(x=`O/C`, y=`H/C`, col=num.trans))+
ggplot(vk_data_al, aes(x=`O/C`, y=`H/C`, col=activity))+
  geom_hline(yintercept = 1.5, lty=2)+
  geom_point(size=1.5, alpha=0.7)+
  scale_color_manual(values = c("#FFE247","grey50","#0052FF"))+
#  scale_color_gradientn(colors = topo.colors(80)[c(1:15,60:80)])+
  #stat_density2d(aes(fill=..level..), geom = "polygon",colour="black")+
  #scale_fill_distiller(palette = "YlOrBr",direction = 1)+
  facet_rep_grid(Temp~Incubation, repeat.tick.labels = T)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold", colour = "black"),
        axis.title = element_text(size = 15, face = "bold", colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 16, face = "bold", colour = "black"),
        legend.text = element_text(size = 12,colour = "black"))+
  ggsave(filename("vk_trans_al"),height = 30, width = 55, units = "cm", dpi = 700)


vk_data_pf=vk_data_pf[order(vk_data_pf$ord2,decreasing = F),]

ggplot(vk_data_pf, aes(x=`O/C`, y=`H/C`, col=activity))+
  geom_hline(yintercept = 1.5, lty=2)+
  geom_point(size=1.5, alpha=0.7)+
  scale_color_manual(values = c("#FFE247","grey50","#0052FF"))+
  #  scale_color_gradientn(colors = topo.colors(80)[c(1:15,60:80)])+
  #stat_density2d(aes(fill=..level..), geom = "polygon",colour="black")+
  #scale_fill_distiller(palette = "YlOrBr",direction = 1)+
  facet_rep_grid(Temp~Incubation, repeat.tick.labels = T)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold", colour = "black"),
        axis.title = element_text(size = 15, face = "bold", colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 16, face = "bold", colour = "black"),
        legend.text = element_text(size = 12,colour = "black"))+
  ggsave(filename("vk_trans_pf"),height = 30, width = 55, units = "cm", dpi = 700)



vk_data_re2$activity2=ifelse(vk_data_re2$num.trans>25, "Active",
                            ifelse(vk_data_re2$num.trans<8,"Inactive", "Others"))

vk_data_re2$Reactivity=ifelse(vk_data_re2$`H/C`<1.5,"R","L")


vk_data_re2$part=paste0(vk_data_re2$Reactivity,vk_data_re2$activity2)

som_all$Sample=paste(som_all$Type,som_all$Temp,som_all$Incubation,sep = "_")

is.factor(som_all$Incubation)
is.factor(vk_data_re2$Incubation)
dim(vk_data_re2)

part_info=unique(vk_data_re2[,c("Sample","Formula","activity2")])

som_all=som_all %>% inner_join(part_info[,c("Sample","Formula","activity2")])



som_part=aggregate(som_all$Freq, by=list(Type=som_all$Type,Temp=som_all$Temp,
                                         Incubation=som_all$Incubation,Rep=som_all$Rep,
                                         Part=som_all$activity2),sum)

som_part_tot=aggregate(som_all$Freq, by=list(Type=som_all$Type,Temp=som_all$Temp,
                                         Incubation=som_all$Incubation,Rep=som_all$Rep),sum) %>% 
  `colnames<-`(c("Type","Temp","Incubation","Rep","Tot"))



som_part=som_part %>% inner_join(som_part_tot)

som_part$rel=som_part$x/som_part$Tot*100

som_part_ctr5=subset(som_part,som_part$Temp=="Ctrl")
som_part_ctr5$Temp=5

som_part_ctr15=subset(som_part,som_part$Temp=="Ctrl")
som_part_ctr15$Temp=15

som_part_ctr25=subset(som_part,som_part$Temp=="Ctrl")
som_part_ctr25$Temp=25

som_part_sel=subset(som_part,som_part$Temp!="Ctrl")

som_part_sel_all=rbind(som_part_ctr5,som_part_ctr15,som_part_ctr25,som_part_sel)

som_part_sel_all2=som_part_sel_all %>% filter(!Part%in%c("LO","RO"))

som_part_sel_all2$Incubation=factor(som_part_sel_all2$Incubation, levels=c("0","70","154","223","336"))


som_part_sel_all2$Templab=factor(som_part_sel_all2$Temp, levels = c(5,15,25),
                    labels = c("5°C","15°C","25°C"))

som_part_sel_m=dcast(som_part_sel_all2,Type+Temp+Incubation~Part, mean, value.var = "rel") %>% 
  melt(id.vars=c("Type","Temp","Incubation"))

som_part_sel_m$Templab=factor(som_part_sel_m$Temp, levels = c(5,15,25),
                                 labels = c("5°C","15°C","25°C"))


som_part_sel_m$variable=factor(som_part_sel_m$variable,levels = c("Active","Inactive","Others"))

som_part_sel_m_sel=subset(som_part_sel_m,som_part_sel_m$variable!="Others")


ggplot(som_part_sel_m_sel, aes(x=Incubation,y=value,fill=variable))+
  geom_bar(stat = "identity",position = position_stack(reverse = T))+
  scale_fill_manual(values = c("#FFCB42","#0052FF","grey50"))+
  #scale_fill_manual(values = c("#374E55FF","#DF8F44FF","#00A1D5FF","#79AF97FF","#5F559BFF","#CD534CFF","#4A6990FF","grey70"))+
  facet_wrap(Type~Templab, scales = "free", ncol=3)+
#  scale_y_continuous("Proportions (%)",labels = scales::percent_format(accuracy = 1,
#                                                                       decimal.mark = '.',scale = 1),expand = c(0.01,0.01))+
  scale_y_continuous("Proportions (%)",labels = scales::percent_format(accuracy = 1,decimal.mark = '.',scale = 1),
                       expand = c(0.01,0.01), limits = c(0,67))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 16,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 1.0,
                                   margin = unit(c(0.02,0.2,0.6,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.text.y = element_text(size = 16, colour = "black",face=2 ),
        axis.title.x = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm")),
        axis.title.y = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 16, colour = "black",face = 2,margin = unit(c(0.02,0.2,0.1,0.2),"cm")),
        strip.text.y = element_blank(),
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 20, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm"), face = 2),
        legend.text = element_text(size = 18, colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm")),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.4, 0.5),
        legend.position = "NULL")+
  guides(fill=guide_legend(title = "",nrow = 1))+
  ggsave(filename("Active_bar"),height = 25, width = 30, units = "cm", dpi = 300)


som_part_sel_all2

som_part_m=dcast(som_part_sel_all2,Type+Temp+Incubation~Part, value.var = "rel",mean) %>% 
  melt(id.vars=c("Type","Temp","Incubation"), value.name = "Mean")
som_part_sd=dcast(som_part_sel_all2,Type+Temp+Incubation~Part, value.var = "rel",sd) %>% 
  melt(id.vars=c("Type","Temp","Incubation"), value.name = "SD")

som_part_stat=som_part_m %>% inner_join(som_part_sd)


scaleFUN <- function(x) sprintf("%.1f", x)


som_part_stat$Incubationlab=as.numeric(som_part_stat$Incubation)

som_part_stat$Temp=factor(som_part_stat$Temp, levels = c(5,15,25),
                         labels = c("5°C","15°C","25°C"))

som_part_stat$variable=factor(som_part_stat$variable, levels = c("RA","RI","LA","LI"),
                              labels = c("Recalcitrant-Active (RA)",
                                         "Recalcitrant-Inactive (RI)",
                                         "Labile-Active (LA)",
                                         "Labile-Inactive (LI)"))

som_part_stat_al=subset(som_part_stat,som_part_stat$Type=="AL")
som_part_stat_pf=subset(som_part_stat,som_part_stat$Type=="PF")


gg <- ggplot()+
  geom_errorbar(data = som_part_stat_al, aes(x=Incubation,ymin=Mean-SD,ymax=Mean+SD,fill=Temp), width=0.2)+
  geom_point(data = som_part_stat_al, aes(x=Incubation,y=Mean,col=Temp,fill=Temp), size=3)+
  geom_smooth(data = som_part_stat_al, aes(x=Incubationlab,y=Mean,col=Temp), method = "loess", size=1.5,se=FALSE)+
  geom_vline(xintercept = 1.5, lty=2)+
  geom_vline(xintercept = 2.5, lty=2)+
  geom_vline(xintercept = 3.5, lty=2)+
  geom_vline(xintercept = 4.5, lty=2)+
  scale_x_discrete(name="")+
  scale_y_continuous(labels=scaleFUN)+
  #scale_fill_manual(values = c("#374E55FF","#DF8F44FF","#00A1D5FF","#CD534CFF","#5F559BFF","#79AF97FF","#4A6990FF","grey70"))+
  scale_color_manual(values = c("#145DA0","#478C5C","#DF362D"))+
  scale_fill_manual(values = c("#145DA0","#478C5C","#DF362D"))+
  facet_rep_wrap(variable~., scales = "free", ncol=4,repeat.tick.labels = T,strip.position = "left" )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 1.0,
                                   margin = unit(c(0.0,0.2,0.2,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.ticks = element_line(size = 1),
        axis.text.y = element_text(size = 18, colour = "black",face=2 ),
        axis.title.x = element_text(size = 22,face=2, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm")),
        axis.title.y = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.placement = "outside",
        strip.text = element_text(size = 22, colour = "black",face = 2,margin = unit(c(0.2,0.4,0.2,0.1),"cm")),
        #strip.text.y = element_blank(),
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 20, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 18, hjust = 0,colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm")),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.direction = "vertical",
        legend.justification=c(0.5, 0.5),
        legend.position = "right")+
  guides(colour=guide_legend(title = "Temperature", override.aes = list(size=1.5)),
         fill="none")+
  ggsave(filename("part_al"),height = 12, width = 60, units = "cm", dpi = 300)

gg <- ggplot()+
  geom_errorbar(data = som_part_stat_pf, aes(x=Incubation,ymin=Mean-SD,ymax=Mean+SD,fill=Temp), width=0.2)+
  geom_point(data = som_part_stat_pf, aes(x=Incubation,y=Mean,col=Temp,fill=Temp), size=3)+
  geom_smooth(data = som_part_stat_pf, aes(x=Incubationlab,y=Mean,col=Temp), method = "loess", size=1.5,se=FALSE)+
  geom_vline(xintercept = 1.5, lty=2)+
  geom_vline(xintercept = 2.5, lty=2)+
  geom_vline(xintercept = 3.5, lty=2)+
  geom_vline(xintercept = 4.5, lty=2)+
  scale_x_discrete(name="")+
  scale_y_continuous(labels=scaleFUN)+
  #scale_fill_manual(values = c("#374E55FF","#DF8F44FF","#00A1D5FF","#CD534CFF","#5F559BFF","#79AF97FF","#4A6990FF","grey70"))+
  scale_color_manual(values = c("#145DA0","#478C5C","#DF362D"))+
  scale_fill_manual(values = c("#145DA0","#478C5C","#DF362D"))+
  facet_rep_wrap(variable~., scales = "free", ncol=4,repeat.tick.labels = T,strip.position = "left" )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 1.0,
                                   margin = unit(c(0.0,0.2,0.2,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.ticks = element_line(size = 1),
        axis.text.y = element_text(size = 18, colour = "black",face=2 ),
        axis.title.x = element_text(size = 22,face=2, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm")),
        axis.title.y = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.placement = "outside",
        strip.text = element_text(size = 22, colour = "black",face = 2,margin = unit(c(0.2,0.4,0.2,0.1),"cm")),
        #strip.text.y = element_blank(),
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 20, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 18, hjust = 0,colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm")),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.direction = "vertical",
        legend.justification=c(0.5, 0.5),
        legend.position = "right")+
  guides(colour=guide_legend(title = "Temperature", override.aes = list(size=1.5)),
         fill="none")+
  ggsave(filename("part_pf"),height = 12, width = 60, units = "cm", dpi = 300)

