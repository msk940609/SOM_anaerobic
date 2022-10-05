

#som_all_fin=fread("Datafile/SOM_sel.csv")

som_all=fread("Datafile/SOM_1st.csv")


som_all$Sample=paste(som_all$Type,som_all$Temp,som_all$Incubation,sep = "_")
som_all


som_all$Freq=1

som_all_cnt=aggregate(som_all$Freq,by=list(Sample=som_all$Sample,Formula=som_all$Formula),sum)

som_all_cnt_sel=subset(som_all_cnt,som_all_cnt$x>1)
length(unique(som_all_cnt_sel$Formula)) ##0<10505, 1< 7010 (selected), 2< 5906


som_all_fin

fm_info=unique(som_all_fin[,c("Formula","Calc.m/z","O/C","H/C","C#","H#","N#","O#","DBE","AI")])


vk_data=som_all_cnt_sel %>% inner_join(fm_info)

dim(som_all_cnt_sel)

vk_data=vk_data %>% separate(Sample, c("Type","Temp","Incubation"),sep = "_")

vk_data$Sample=paste(vk_data$Type,vk_data$Temp,vk_data$Incubation, sep = "_")

ggplot(vk_data, aes(x=`O/C`, y=`H/C`))+
  geom_point(size=1.5, alpha=0.5)+
  stat_density2d(aes(fill=..level..), geom = "polygon",colour="black")+
  scale_fill_distiller(palette = "YlOrBr",direction = 1)+
  facet_rep_grid(Temp~Incubation)

trans_num_sel=trans_num[,c("Sample","peak","num.trans.involved.in")]
trans_num_sel=trans_num_sel %>% `colnames<-`(c("Sample","Calc.m/z","num.trans"))


vk_data=vk_data %>% inner_join(trans_num_sel)



vk_data_re=subset(vk_data,vk_data$Temp!="Ctrl") %>% droplevels()
vk_data_ctr=subset(vk_data,vk_data$Temp=="Ctrl")

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


trans_num

vk_data_al=subset(vk_data_re2,vk_data_re2$Type=="AL")
vk_data_pf=subset(vk_data_re2,vk_data_re2$Type=="PF")

vk_data_al=vk_data_al[order(vk_data_al$num.trans),]

ggplot(vk_data_al, aes(x=`O/C`, y=`H/C`, col=num.trans))+
  geom_hline(yintercept = 1.5, lty=2)+
  geom_point(size=1.5, alpha=0.5)+
  scale_color_gradientn(colors = topo.colors(80)[c(1:20,60:80)])+
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


vk_data_pf=vk_data_pf[order(vk_data_pf$num.trans),]

ggplot(vk_data_pf, aes(x=`O/C`, y=`H/C`,col=num.trans))+
  geom_hline(yintercept = 1.5)+
  geom_point(size=1.5, alpha=0.5)+
  scale_color_gradientn(colors = topo.colors(80)[c(1:20,60:80)])+
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


