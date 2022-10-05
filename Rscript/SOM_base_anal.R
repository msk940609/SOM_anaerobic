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
library(lemon)


som=fread("Datafile/SOM_1st.csv")
som=som %>% mutate(Comp = gsub('[[:digit:]]', '', Formula))

som$Comp=ifelse(som$`O#`==0,"Remainders",som$Comp)
table(som$Comp)

#som$Comp=factor(som$Comp, levels = c("CHO","CHNO","CHOS","CHNOS","CH","CHN","CHS","CHNS"),
#                labels = c("CHO","CHON","CHOS","CHONS","CH","CHN","CHS","CHNS"))

som$Comp=factor(som$Comp, levels = c("CHO","CHNO","CHOS","CHNOS","Remainders"),
                labels = c("CHO","CHON","CHOS","CHONS","Remainders"))


som_comp=aggregate(som$Bromo.Inty, by=list(Sample=som$Sample, Comp=som$Comp), sum)
som_comp_tot=aggregate(som$Bromo.Inty, by=list(Sample=som$Sample), sum) %>% `colnames<-`(c("Sample","Tot"))

som_comp=som_comp %>% inner_join(som_comp_tot)

som_comp

som_comp$rel=som_comp$x/som_comp$Tot*100


som_comp=som_comp %>% separate(Sample, c("Type","Date","Temp","Rep"))

som_comp$Date=as.numeric(gsub("D", "", som_comp$Date))

som_comp=som_comp %>% add_column("Sample"=paste(som_comp$Type,som_comp$Date,som_comp$Temp,som_comp$Rep, sep = "_"),.before = 1)
som_comp

som_comp$Complab=paste0(som_comp$Comp, " (%)")

som_comp$Complab=factor(som_comp$Complab, levels=c("CHO (%)","CHON (%)","CHOS (%)","CHONS (%)","Remainders (%)"))
table(som_comp$Comp)
table(som_comp$Complab)


som_comp_temp15=som_comp %>% filter(Temp%in%c("Ctrl","15"))
som_comp_temp15_sel=subset(som_comp_temp15,som_comp_temp15$Comp!="Remainders")
table(som_comp_temp5_sel$Complab)

ggplot(som_comp_temp15_sel,aes(x=as.factor(Date), y=rel, fill=Type))+
  stat_boxplot(geom = "errorbar", position = position_dodge(width=0.75), width=0.5)+
  geom_boxplot()+
  scale_x_discrete(name="Date")+
  scale_fill_manual(values = c("#A96036","#5EB7CA"))+
  facet_rep_wrap(.~Complab,repeat.tick.labels = T, scales = "free", ncol=4,strip.position="left")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 0.5,
                                   margin = unit(c(0.02,0.2,0.6,0.2),"cm")),
        axis.text.y = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 0.5,
                                   margin = unit(c(0.02,0.2,0.6,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.title.x = element_text(size = 22, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm"), face=2),
        axis.title.y = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 18, colour = "black",face = 2,margin = unit(c(0.02,0.2,0.1,0.2),"cm")),
        strip.text.y = element_text(size = 22, colour = "black",face = 2,margin = unit(c(0.02,0.3,0.1,0.1),"cm")),
        strip.placement = "outside",
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 20, colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm"), face=2),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.4, 0.5),
        legend.position = c(0.16,0.95))+
  ggsave(filename("Comp_bar_15"),height = 15, width = 60, units = "cm", dpi = 300)

som_comp_temp25=som_comp %>% filter(Temp%in%c("Ctrl","25"))
som_comp_temp25_sel=subset(som_comp_temp25,som_comp_temp25$Comp!="Remainders")
table(som_comp_temp5_sel$Complab)

ggplot(som_comp_temp25_sel,aes(x=as.factor(Date), y=rel, fill=Type))+
  stat_boxplot(geom = "errorbar", position = position_dodge(width=0.75), width=0.5)+
  geom_boxplot()+
  scale_x_discrete(name="Date")+
  scale_fill_manual(values = c("#A96036","#5EB7CA"))+
  facet_rep_wrap(.~Complab,repeat.tick.labels = T, scales = "free", ncol=4,strip.position="left")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 0.5,
                                   margin = unit(c(0.02,0.2,0.6,0.2),"cm")),
        axis.text.y = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 0.5,
                                   margin = unit(c(0.02,0.2,0.6,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.title.x = element_text(size = 22, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm"), face=2),
        axis.title.y = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 18, colour = "black",face = 2,margin = unit(c(0.02,0.2,0.1,0.2),"cm")),
        strip.text.y = element_text(size = 22, colour = "black",face = 2,margin = unit(c(0.02,0.3,0.1,0.1),"cm")),
        strip.placement = "outside",
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 20, colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm"), face=2),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.4, 0.5),
        legend.position = c(0.16,0.95))+
  ggsave(filename("Comp_bar_25"),height = 15, width = 60, units = "cm", dpi = 300)



som
som$lipid=ifelse(som$`O/C`>=0&som$`O/C`<0.3,
                     ifelse(som$`H/C`>1.5&som$`H/C`<2.0, "Lipids",""),"")
som$protein=ifelse(som$`O/C`>=0.3&som$`O/C`<=0.67,
                       ifelse(som$`H/C`>1.5&som$`H/C`<2.2,
                              ifelse(som$`N/C`>=0.05,"Proteins","") ,""),"")
som$carbo=ifelse(som$`O/C`>0.67&som$`O/C`<2.0,
                     ifelse(som$`H/C`>1.5&som$`H/C`<2.3, "Carbohydrates",""),"")
som$Unhydro=ifelse(som$`O/C`>0&som$`O/C`<0.1,
                       ifelse(som$`H/C`>=0.7&som$`H/C`<=1.5, "Unsaturated hydrocarbons",""),"")
som$lignin=ifelse(som$`O/C`>=0.1&som$`O/C`<=0.67,
                      ifelse(som$`H/C`>=0.7&som$`H/C`<=1.5, 
                             ifelse(som$AI<0.67, "Lignins",""),""),"")
som$tannin=ifelse(som$`O/C`>0.67&som$`O/C`<2.0,
                      ifelse(som$`H/C`>=0.5&som$`H/C`<=1.5, 
                             ifelse(som$AI<0.67, "Tannins",""),""),"")
som$conaro=ifelse(som$`O/C`>0&som$`O/C`<=0.67,
                      ifelse(som$`H/C`>=0.2&som$`H/C`<0.67, 
                             ifelse(som$AI>=0.67, "Condensed aromatics",""),""),"")

som=som %>%  unite("Molecularclass",c("lipid","protein","carbo","Unhydro","lignin","tannin","conaro"), sep = "")

som$Molecularclass=ifelse(som$Molecularclass=="","Unassigned", som$Molecularclass) ###colum of molecular class
table(som$Molecularclass)




som_mole=aggregate(som$Bromo.Inty, by=list(Sample=som$Sample, mole=som$Molecularclass), sum)
som_mole_tot=aggregate(som$Bromo.Inty, by=list(Sample=som$Sample), sum) %>% `colnames<-`(c("Sample","Tot"))

som_mole=som_mole %>% inner_join(som_mole_tot)

som_mole

som_mole$rel=som_mole$x/som_mole$Tot*100
som_mole=som_mole %>% separate(Sample, c("Type","Date","Temp","Rep"))

som_mole$Date=as.numeric(gsub("D", "", som_mole$Date))

som_mole=som_mole %>% add_column("Sample"=paste(som_mole$Type,som_mole$Date,som_mole$Temp,som_mole$Rep, sep = "_"),.before = 1)
som_mole

som_mole$molelab=paste0(som_mole$mole, " (%)")

som_mole$molelab=factor(som_mole$molelab, levels=c("CHO (%)","CHON (%)","CHOS (%)","CHONS (%)","Remainders (%)"))
table(som_mole$molelab)


som_mole_temp5=som_mole %>% filter(Temp%in%c("Ctrl","5"))
som_mole_temp5_sel=subset(som_mole_temp15,som_mole_temp15$mole!="Remainders")
table(som_mole_temp5_sel$molelab)

ggplot(som_mole_temp5_sel,aes(x=as.factor(Date), y=rel, fill=Type))+
  stat_boxplot(geom = "errorbar", position = position_dodge(width=0.75), width=0.5)+
  geom_boxplot()+
  scale_x_discrete(name="Date")+
  scale_fill_manual(values = c("#A96036","#5EB7CA"))+
  facet_rep_wrap(.~molelab,repeat.tick.labels = T, scales = "free", ncol=4,strip.position="left")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 0.5,
                                   margin = unit(c(0.02,0.2,0.6,0.2),"cm")),
        axis.text.y = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 0.5,
                                   margin = unit(c(0.02,0.2,0.6,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.title.x = element_text(size = 22, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm"), face=2),
        axis.title.y = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 18, colour = "black",face = 2,margin = unit(c(0.02,0.2,0.1,0.2),"cm")),
        strip.text.y = element_text(size = 22, colour = "black",face = 2,margin = unit(c(0.02,0.3,0.1,0.1),"cm")),
        strip.placement = "outside",
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 20, colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm"), face=2),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.4, 0.5),
        legend.position = c(0.16,0.95))+
  ggsave(filename("mole_bar_5"),height = 25, width = 60, units = "cm", dpi = 300)



som_mole_temp15=som_mole %>% filter(Temp%in%c("Ctrl","15"))
som_mole_temp15_sel=subset(som_mole_temp15,som_mole_temp15$mole!="Remainders")
table(som_mole_temp5_sel$molelab)

ggplot(som_mole_temp15_sel,aes(x=as.factor(Date), y=rel, fill=Type))+
  stat_boxplot(geom = "errorbar", position = position_dodge(width=0.75), width=0.5)+
  geom_boxplot()+
  scale_x_discrete(name="Date")+
  scale_fill_manual(values = c("#A96036","#5EB7CA"))+
  facet_rep_wrap(.~molelab,repeat.tick.labels = T, scales = "free", ncol=4,strip.position="left")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 0.5,
                                   margin = unit(c(0.02,0.2,0.6,0.2),"cm")),
        axis.text.y = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 0.5,
                                   margin = unit(c(0.02,0.2,0.6,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.title.x = element_text(size = 22, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm"), face=2),
        axis.title.y = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 18, colour = "black",face = 2,margin = unit(c(0.02,0.2,0.1,0.2),"cm")),
        strip.text.y = element_text(size = 22, colour = "black",face = 2,margin = unit(c(0.02,0.3,0.1,0.1),"cm")),
        strip.placement = "outside",
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 20, colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm"), face=2),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.4, 0.5),
        legend.position = c(0.16,0.95))+
  ggsave(filename("mole_bar_15"),height = 15, width = 60, units = "cm", dpi = 300)

som_mole_temp25=som_mole %>% filter(Temp%in%c("Ctrl","25"))
som_mole_temp25_sel=subset(som_mole_temp25,som_mole_temp25$mole!="Remainders")
table(som_mole_temp5_sel$molelab)


ggplot(som_mole_temp25_sel,aes(x=as.factor(Date), y=rel, fill=Type))+
  stat_boxplot(geom = "errorbar", position = position_dodge(width=0.75), width=0.5)+
  geom_boxplot()+
  scale_x_discrete(name="Date")+
  scale_fill_manual(values = c("#A96036","#5EB7CA"))+
  facet_rep_wrap(.~molelab,repeat.tick.labels = T, scales = "free", ncol=4,strip.position="left")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 0.5,
                                   margin = unit(c(0.02,0.2,0.6,0.2),"cm")),
        axis.text.y = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 0.5,
                                   margin = unit(c(0.02,0.2,0.6,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.title.x = element_text(size = 22, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm"), face=2),
        axis.title.y = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 18, colour = "black",face = 2,margin = unit(c(0.02,0.2,0.1,0.2),"cm")),
        strip.text.y = element_text(size = 22, colour = "black",face = 2,margin = unit(c(0.02,0.3,0.1,0.1),"cm")),
        strip.placement = "outside",
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 20, colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm"), face=2),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.4, 0.5),
        legend.position = c(0.16,0.95))+
  ggsave(filename("mole_bar_25"),height = 15, width = 60, units = "cm", dpi = 300)




