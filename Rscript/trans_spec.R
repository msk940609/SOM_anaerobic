library(vegan)
library(ggplot2)
library("openxlsx")
library(shiny)
library(plotly)
library(dplyr)
library(data.table)
options(java.parameters = "- Xmx2048m")
options(shiny.maxRequestSize=1000*1024^2)
getOption("digits")
options("digits" = 15)
library(data.table)
library(RColorBrewer)
options(scipen=1000)
library(rsconnect)
library(tidyverse)
library(bit64)
library(shinyFiles)
library(xlsx)
library(writexl)
library(openxlsx)
library(tools)
library(extrafont)
loadfonts(device="win")



lf=list.files(path = "Transformation Peak Comparisons/Dataset_Name/", pattern = ".csv")
lf

dt=data.table()
for (i in 1:length(lf)) {
  #  i=1
  
  temp=fread(paste0("Transformation Peak Comparisons/Dataset_Name/",lf[i]))
  
  dt=rbind(dt,temp)
  
}

dt$Freq=1
dt_p2p=dt


dt_p2p_sel=dt_p2p %>% filter(Trans.name%in%c("glyoxylate_(_H2O)_C2O2","secondary amine (NH)","Carboxylation_CO2","SO42-"))
dt_p2p_sel



p2p=dcast(dt_p2p, sample~Trans.name, sum)
p2p

dim(p2p)


p2p=p2p %>% separate(sample, c("non","Type","Temp","Incubation"))
p2p

p2p

table(p2p$Temp)
table(p2p$Incubation)

p2p_ctr5=subset(p2p,p2p$Temp=="Ctrl")
p2p_ctr5$Temp=5
p2p_ctr15=subset(p2p,p2p$Temp=="Ctrl")
p2p_ctr15$Temp=15
p2p_ctr25=subset(p2p,p2p$Temp=="Ctrl")
p2p_ctr25$Temp=25
p2p_ctr25

p2p_incu=subset(p2p,p2p$Temp!="Ctrl")


p2p_fin=rbind(p2p_ctr5,p2p_ctr15,p2p_ctr25,p2p_incu)

p2p_fin$Incubation=as.numeric(p2p_fin$Incubation)

#fwrite(p2p_fin,file = "Datafile/p2p_merge.csv")

p2p_fin=fread("Datafile/p2p_merge.csv")


p2p_fin=as.data.frame(p2p_fin)
typ=unique(p2p_fin$Type)
temp=unique(p2p_fin$Temp)


dt=data.table()
for (i in 1:length(typ)) {
  #i=1
  tempo=subset(p2p_fin,p2p_fin$Type==typ[i])
  
  for (j in 1:length(temp)) {
   # j=1
    tempo2=subset(tempo,tempo$Temp==temp[j])
    
       
    for (k in 5:dim(tempo2)[2]) {
      #k=5
      
      cor=cor.test(tempo2[,4],tempo2[,k],method = "spearman")

      colnames(tempo2)[k]
      new=data.table(Type=typ[i],Temp=temp[j],Var1="Incubation",Var2=colnames(tempo2)[k],rho=round(cor$estimate,2),
                     p=round(cor$p.value,3))
            
      dt=rbind(dt,new)
    }
    
     
  }
  
}

dt

table(dt$Trans.name)

p2p_m=melt(p2p[,-1],id.vars = c("Type","Temp","Incubation")) 

tran_type=dcast(p2p_m,variable~Type,mean)
tran_type$delt=tran_type$AL-tran_type$PF

tran_type=tran_type[order(tran_type$delt),]
tran_type

dt_sel=subset(dt,dt$rho>0.5)

dt_sel$Group=paste0(dt_sel$Type,"_",dt_sel$Temp)



data_new2 <- dt_sel %>%                                      # Top N highest values by group
  arrange(desc(rho)) %>% 
  group_by(Group)

trans_sel

fwrite(data_new2, "trans_list.csv")




data_new2
library(reshape2)
as.data.frame(data_new2)
data_new2$Freq=1
data_d=reshape2::dcast(as.data.frame(data_new2),Group~Var2, sum,value.var = "Freq")


da=aggregate(data_new2$Freq,by=list(Type=data_new2$Type,Trans=data_new2$Var2),sum)




dt_p2p_sel=dt_p2p %>% filter(Trans.name%in%c("glyoxylate_(_H2O)_C2O2","acetylation_(_H2O)_C2H2O","Carboxylation_CO2","SO42-"))
dt_p2p_sel$Freq=1

table(dt_p2p_sel$Trans.name)

dim(p2p)

p2p_sel=dcast(dt_p2p_sel, sample~Trans.name, sum)
p2p_sel

p2p_sel=p2p_sel %>% separate(sample, c("non","Type","Temp","Incubation"))


table(p2p_sel$Incubation)

p2p_sel_ctr5=subset(p2p_sel,p2p_sel$Temp=="Ctrl")
p2p_sel_ctr5$Temp=5
p2p_sel_ctr15=subset(p2p_sel,p2p_sel$Temp=="Ctrl")
p2p_sel_ctr15$Temp=15
p2p_sel_ctr25=subset(p2p_sel,p2p_sel$Temp=="Ctrl")
p2p_sel_ctr25$Temp=25
p2p_sel_ctr25

p2p_incu_sel=subset(p2p_sel,p2p_sel$Temp!="Ctrl")

p2p_fin_sel=rbind(p2p_sel_ctr5,p2p_sel_ctr15,p2p_sel_ctr25,p2p_incu_sel)

p2p_fin_sel_m=melt(p2p_fin_sel[,-1],id.vars = c("Type","Temp","Incubation"))

p2p_fin_sel_m$val=ifelse(p2p_fin_sel_m$variable!="SO42-",round(p2p_fin_sel_m$value/100,0),round(p2p_fin_sel_m$value/3,0))


p2p_fin_sel_m$Templab=factor(p2p_fin_sel_m$Temp, levels =c(5,15,25),
                          labels = c("5°C","15°C","25°C"))

p2p_fin_sel_m$varlab=factor(p2p_fin_sel_m$variable, levels =c("acetylation_(_H2O)_C2H2O",
                                                              "Carboxylation_CO2","glyoxylate_(_H2O)_C2O2",
                                                              "SO42-"),
                             labels = c(expression(bold("Acetylation")),
                                        expression(bold("Carboxylation")),
                                        expression(bold("Glyoxylate")),
                                        expression(bold("SO"["4"]^"2-"))))


p2p_fin_sel_m$Incubationlab=ifelse(p2p_fin_sel_m$Incubation==0,1,
                                   ifelse(p2p_fin_sel_m$Incubation==70,2,
                                          ifelse(p2p_fin_sel_m$Incubation==154,3,
                                                 ifelse(p2p_fin_sel_m$Incubation==223,4,5))))
p2p_fin_sel_m$Incubation=factor(p2p_fin_sel_m$Incubation, levels=c("0","70","154","223","336"))

p2p_fin_sel_m_al=subset(p2p_fin_sel_m,p2p_fin_sel_m$Type=="AL")
p2p_fin_sel_m_pf=subset(p2p_fin_sel_m,p2p_fin_sel_m$Type=="PF")




scaleFUN_int <- function(x) sprintf("%.0f", x)

ggplot()+
  geom_point(data = p2p_fin_sel_m_al, aes(x=Incubation,y=val,col=Templab,fill=Templab), size=3)+
  geom_smooth(data = p2p_fin_sel_m_al, aes(x=Incubationlab,y=val,col=Templab), method = "loess", size=1.5,se=FALSE)+
  geom_vline(xintercept = 1.5, lty=2)+
  geom_vline(xintercept = 2.5, lty=2)+
  geom_vline(xintercept = 3.5, lty=2)+
  geom_vline(xintercept = 4.5, lty=2)+
  scale_x_discrete(name="Incubation days")+
  scale_color_manual(values = c("#145DA0","#478C5C","#DF362D"))+
  scale_fill_manual(values = c("#145DA0","#478C5C","#DF362D"))+
  facet_rep_wrap(.~varlab, scales = "free",ncol = 4, labeller = label_parsed,repeat.tick.labels = T,strip.position = "left")+
  #facet_rep_wrap(variable~., scales = "free", ncol=4,repeat.tick.labels = T,strip.position = "left" )+
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
  ggsave(filename("trans_al"),height = 12, width = 60, units = "cm", dpi = 300)
  


ggplot()+
  geom_point(data = p2p_fin_sel_m_pf, aes(x=Incubation,y=val,col=Templab,fill=Templab), size=3)+
  geom_smooth(data = p2p_fin_sel_m_pf, aes(x=Incubationlab,y=val,col=Templab), method = "loess", size=1.5,se=FALSE)+
  geom_vline(xintercept = 1.5, lty=2)+
  geom_vline(xintercept = 2.5, lty=2)+
  geom_vline(xintercept = 3.5, lty=2)+
  geom_vline(xintercept = 4.5, lty=2)+
  scale_x_discrete(name="Incubation days")+
  scale_color_manual(values = c("#145DA0","#478C5C","#DF362D"))+
  scale_fill_manual(values = c("#145DA0","#478C5C","#DF362D"))+
  facet_rep_wrap(.~varlab, scales = "free",ncol = 4, labeller = label_parsed,repeat.tick.labels = T,strip.position = "left")+
  #facet_rep_wrap(variable~., scales = "free", ncol=4,repeat.tick.labels = T,strip.position = "left" )+
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
  ggsave(filename("trans_pf"),height = 12, width = 60, units = "cm", dpi = 300)
