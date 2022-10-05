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

##Data analysis=====
som_1st=fread("Datafile/SOM_1st.csv")

#Molecular class====
ft_data=som_1st
ft_data$lipid=ifelse(ft_data$`O/C`>=0&ft_data$`O/C`<0.3,
                     ifelse(ft_data$`H/C`>1.5&ft_data$`H/C`<2.0, "Lipids",""),"")
ft_data$protein=ifelse(ft_data$`O/C`>=0.3&ft_data$`O/C`<=0.67,
                       ifelse(ft_data$`H/C`>1.5&ft_data$`H/C`<2.2,
                              ifelse(ft_data$`N/C`>=0.05,"Proteins","") ,""),"")
ft_data$carbo=ifelse(ft_data$`O/C`>0.67&ft_data$`O/C`<2.0,
                     ifelse(ft_data$`H/C`>1.5&ft_data$`H/C`<2.3, "Carbohydrates",""),"")
ft_data$Unhydro=ifelse(ft_data$`O/C`>0&ft_data$`O/C`<0.1,
                       ifelse(ft_data$`H/C`>=0.7&ft_data$`H/C`<=1.5, "Unsaturated hydrocarbons",""),"")
ft_data$lignin=ifelse(ft_data$`O/C`>=0.1&ft_data$`O/C`<=0.67,
                      ifelse(ft_data$`H/C`>=0.7&ft_data$`H/C`<=1.5, 
                             ifelse(ft_data$AI<0.67, "Lignins",""),""),"")
ft_data$tannin=ifelse(ft_data$`O/C`>0.67&ft_data$`O/C`<2.0,
                      ifelse(ft_data$`H/C`>=0.5&ft_data$`H/C`<=1.5, 
                             ifelse(ft_data$AI<0.67, "Tannins",""),""),"")
ft_data$conaro=ifelse(ft_data$`O/C`>0&ft_data$`O/C`<=0.67,
                      ifelse(ft_data$`H/C`>=0.2&ft_data$`H/C`<0.67, 
                             ifelse(ft_data$AI>=0.67, "Condensed aromatics",""),""),"")

ft_data=ft_data %>%  unite("Molecularclass",c("lipid","protein","carbo","Unhydro","lignin","tannin","conaro"), sep = "")

ft_data$Molecularclass=ifelse(ft_data$Molecularclass=="","Unassigned", ft_data$Molecularclass) ###colum of molecular class
table(ft_data$Molecularclass)

ft_data

#Molecular category====
ft_data$PCA=ifelse(ft_data$AI>0.66, "Combustion-drived PCA","")

ft_data$aroma=ifelse(ft_data$AI>0.5,ifelse(ft_data$AI<=0.66,"Soil-derived polyphenols and PCAs with aliphatic chains",""),"")

ft_data$humic=ifelse(ft_data$AI<=0.5,
                     ifelse(ft_data$`H/C`<1.5,"Soil derived humics and high unsaturated compounds",""),"")

ft_data$aliphatic=ifelse(ft_data$`H/C`<2.0&ft_data$`H/C`>=1.5,
                         ifelse(ft_data$AI<=0.5,"Unsaturated aliphatic compounds",""),"")

ft_data$fatty=ifelse(ft_data$`H/C`>=2.0,
                     ifelse(ft_data$AI<=0.5,"Saturated fatty and sulfonic acid,carbohyrates",""),"")

ft_data=ft_data %>%  unite("Molecularcategory",c("PCA","aroma","humic","aliphatic","fatty"), sep = "")
ft_data$Molecularcategory=ifelse(ft_data$Molecularcategory=="","Unassigned", ft_data$Molecularcategory) ###colum of molecular class
table(ft_data$Molecularcategory)

ft_data
#Chemical composition====
ft_data
ft_data$Cn=ifelse(ft_data$`C#`>0, "C","") #C >0 이상이면 C출력 (H, O, N, S 반복)
ft_data$Hn=ifelse(ft_data$`H#`>0, "H","")
ft_data$On=ifelse(ft_data$`O#`>0, "O","")
ft_data$Nn=ifelse(ft_data$`N#`>0, "N","")
ft_data$Sn=ifelse(ft_data$`S#`>0, "S","")
ft_data=ft_data %>% unite("Comp",c("Cn","Hn","On","Nn","Sn"),sep = "") 
ft_data$Comp=ifelse(ft_data$`O#`==0, "Remainders",ft_data$Comp)

ft_data

table(ft_data$Comp)
table(ft_data$Molecularcategory)
table(ft_data$Molecularclass)

som_all=ft_data

##molecular class distribution=====
som_all

mole_som=as.data.table(aggregate(som_all$Bromo.Inty, 
                                 by=list(Sample=som_all$Sample,Molecularclass=som_all$Molecularclass),sum))

mole_som_tot=as.data.table(aggregate(som_all$Bromo.Inty, 
                                     by=list(Sample=som_all$Sample),sum)) %>% `colnames<-`(c("Sample","Tot"))

mole_som=mole_som %>% inner_join(mole_som_tot)
mole_som
mole_som$rel=mole_som$x/mole_som$Tot*100
mole_som

aggregate(mole_som$rel, by=list(Sample=mole_som$Sample),sum)
som_all
som_id=unique(som_all[,c("Sample","Type","Incubation","Temp","Rep")])

mole_som=mole_som %>% inner_join(som_id)
mole_som

mole_som_m=melt(mole_som[,c("Molecularclass","Type","Incubation","Temp","Rep","rel")],id.vars = c("Molecularclass","Type","Incubation","Temp","Rep")) %>% 
  dcast(Type+Incubation+Temp~Molecularclass,mean) %>% 
  melt(id.vars=c("Type","Incubation","Temp"),variable.name = "Molecularclass")
  
mole_som_m
unique(mole_som_m$Molecularclass)
mole_som_m$Molecularclass=factor(mole_som_m$Molecularclass, 
                                 levels = c("Lignins","Tannins","Condensed aromatics","Unsaturated hydrocarbons",
                                            "Lipids","Proteins","Carbohydrates","Unassigned"))
mole_som_m_sel=subset(mole_som_m,mole_som_m$Temp!="Ctrl") %>% droplevels()
mole_som_m_sel

mole_som_m_ctr=subset(mole_som_m,mole_som_m$Temp=="Ctrl")

mole_som_m_ctr5=mole_som_m_ctr
mole_som_m_ctr5$Temp="5"

mole_som_m_ctr15=mole_som_m_ctr
mole_som_m_ctr15$Temp="15"

mole_som_m_ctr25=mole_som_m_ctr
mole_som_m_ctr25$Temp="25"

mole_som_m_ctr_all=rbind(mole_som_m_ctr5,mole_som_m_ctr15,mole_som_m_ctr25)
mole_som_m_ctr_all

mole_som_m_sel2=rbind(mole_som_m_sel,mole_som_m_ctr_all)
mole_som_m_sel2

mole_som_m_sel2$Incubation=factor(mole_som_m_sel2$Incubation, levels = c(0,70,154,223,336))
mole_som_m_sel2


mole_som_m_sel2$Temp=factor(mole_som_m_sel2$Temp, levels = c(5,15,25))

ggplot(mole_som_m_sel2, aes(x=Incubation,y=value,fill=Molecularclass))+
  geom_bar(stat = "identity",position = position_stack(reverse = T))+
  #scale_fill_manual(values = c("#374E55FF","#DF8F44FF","#00A1D5FF","#CD534CFF","#5F559BFF","#79AF97FF","#4A6990FF","grey70"))+
  scale_fill_manual(values = c("#374E55FF","#DF8F44FF","#00A1D5FF","#79AF97FF","#5F559BFF","#CD534CFF","#4A6990FF","grey70"))+
  facet_wrap(Type~Temp, scales = "free", ncol=3)+
  scale_y_continuous("Proportions (%)",labels = scales::percent_format(accuracy = 1,
                                                                      decimal.mark = '.',scale = 1),expand = c(0.01,0.01))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 14,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 1.0,
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
        legend.title = element_text(size = 20, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 18, colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm")),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.4, 0.5),
        legend.position = "bottom")+
  guides(fill=guide_legend(title = "Molecular classes",nrow = 2))+
  ggsave(filename("Mole_bar"),height = 32, width = 40, units = "cm", dpi = 300)

mole_som$Molecularclass=factor(mole_som$Molecularclass, 
                                 levels = c("Lignins","Tannins","Unsaturated hydrocarbons","Condensed aromatics",
                                            "Lipids","Proteins","Carbohydrates","Unassigned"))

mole_som$Molecularclass2=factor(mole_som$Molecularclass, 
                               levels = c("Lignins","Tannins","Condensed aromatics","Unsaturated hydrocarbons",
                                          "Lipids","Proteins","Carbohydrates","Unassigned"),
                               labels = c("Lignins","Tannins","Condensed\naromatics","Unsaturated\nhydrocarbons",
                                          "Lipids","Proteins","Carbohydrates","Unassigned"))

mole_som$Temp=factor(mole_som$Temp, levels=c("Ctrl","5","15","25"))
mole_som$Incubation=factor(mole_som$Incubation,levels = c("0","70","154","223","336"))

mole_som_sel=subset(mole_som,mole_som$Molecularclass!="Unassigned") %>% droplevels()

ggplot(mole_som_sel, aes(x=Type,y=rel,fill=Incubation))+
  geom_boxplot()+
  #scale_fill_manual(values = c("#374E55FF","#DF8F44FF","#00A1D5FF","#CD534CFF","#5F559BFF","#79AF97FF","#4A6990FF","grey70"))+
  scale_fill_manual(values = c("grey50","#FD812D","#8BCD50","#6BCAE2","#8A6FDF"))+
  facet_wrap(Temp+Molecularclass2~., scales = "free", ncol=7)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 16,colour = "black", angle = 0, hjust = 0.5,vjust = 1.0),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.text.y = element_text(size = 12, colour = "black" ),
        axis.title.x = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm")),
        axis.title.y = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 16, colour = "black",face = 2,margin = unit(c(0.02,0.2,0.1,0.2),"cm")),
        strip.text.y = element_blank(),
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 22, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 18, colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm")),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.5, 0.5),
        legend.position = "bottom")+
  guides(fill=guide_legend(title = "Incubation (Days)"))+
  ggsave(filename("Mole_dis"),height = 35, width = 60, units = "cm", dpi = 300)

mole_som_ctr=subset(mole_som_sel,mole_som_sel$Temp=="Ctrl")
mole_som_ctr

mole_som_ctr_5=mole_som_ctr
mole_som_ctr_5$Temp="5"

mole_som_ctr_15=mole_som_ctr
mole_som_ctr_15$Temp="15"

mole_som_ctr_25=mole_som_ctr
mole_som_ctr_25$Temp="25"

mole_ctr=rbind(mole_som_ctr_5,mole_som_ctr_15,mole_som_ctr_25)

mole_som_sel2=subset(mole_som_sel,mole_som_sel$Temp!="Ctrl") %>% droplevel()
mole_som_sel2=rbind(mole_som_sel2,mole_ctr)
mole_som_sel2

ggplot(mole_som_sel2, aes(x=Type,y=rel,fill=Incubation))+
  geom_boxplot()+
  #scale_fill_manual(values = c("#374E55FF","#DF8F44FF","#00A1D5FF","#CD534CFF","#5F559BFF","#79AF97FF","#4A6990FF","grey70"))+
  scale_fill_manual(values = c("grey50","#FD812D","#8BCD50","#6BCAE2","#8A6FDF"))+
  facet_wrap(Temp+Molecularclass2~., scales = "free", ncol=7)+
  scale_y_continuous("Proportions (%)",labels = scales::number_format(accuracy = 0.1,
                                                                       decimal.mark = '.'))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 1.0),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.text.y = element_text(size = 16, colour = "black",face=2 ),
        axis.title.x = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm")),
        axis.title.y = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 16, colour = "black",face = 2,margin = unit(c(0.02,0.2,0.1,0.2),"cm")),
        strip.text.y = element_blank(),
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 22, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 18, colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm")),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.5, 0.5),
        legend.position = "bottom")+
  guides(fill=guide_legend(title = "Incubation (Days)"))+
  ggsave(filename("Mole_dis_2"),height = 34, width = 60, units = "cm", dpi = 300)

##chemical composition distribution=====
som_all

comp_som=as.data.table(aggregate(som_all$Bromo.Inty, 
                                 by=list(Sample=som_all$Sample,Comp=som_all$Comp),sum))

comp_som_tot=as.data.table(aggregate(som_all$Bromo.Inty, 
                                     by=list(Sample=som_all$Sample),sum)) %>% `colnames<-`(c("Sample","Tot"))

comp_som=comp_som %>% inner_join(comp_som_tot)
comp_som
comp_som$rel=comp_som$x/comp_som$Tot*100
comp_som

aggregate(comp_som$rel, by=list(Sample=comp_som$Sample),sum)

som_all
som_id=unique(som_all[,c("Sample","Type","Incubation","Temp","Rep")])

comp_som=comp_som %>% inner_join(som_id)
comp_som

comp_som_m=melt(comp_som[,c("Comp","Type","Incubation","Temp","Rep","rel")],id.vars = c("Comp","Type","Incubation","Temp","Rep")) %>% 
  dcast(Type+Incubation+Temp~Comp,mean) %>% 
  melt(id.vars=c("Type","Incubation","Temp"),variable.name = "Comp")

comp_som_m

unique(comp_som_m$Comp)
comp_som_m$Comp=factor(comp_som_m$Comp,levels = c("CHO","CHON","CHOS","CHONS","Remainders"))

comp_som_m_sel=subset(comp_som_m,comp_som_m$Temp!="Ctrl") %>% droplevels()
comp_som_m_sel

comp_som_m_ctr=subset(comp_som_m,comp_som_m$Temp=="Ctrl")

comp_som_m_ctr5=comp_som_m_ctr
comp_som_m_ctr5$Temp="5"

comp_som_m_ctr15=comp_som_m_ctr
comp_som_m_ctr15$Temp="15"

comp_som_m_ctr25=comp_som_m_ctr
comp_som_m_ctr25$Temp="25"

comp_som_m_ctr_all=rbind(comp_som_m_ctr5,comp_som_m_ctr15,comp_som_m_ctr25)
comp_som_m_ctr_all

comp_som_m_sel2=rbind(comp_som_m_sel,comp_som_m_ctr_all)
comp_som_m_sel2

comp_som_m_sel2$Incubation=factor(comp_som_m_sel2$Incubation, levels = c(0,70,154,223,336))
comp_som_m_sel2

comp_som_m_sel2$Temp=factor(comp_som_m_sel2$Temp, levels = c(5,15,25))

ggplot(comp_som_m_sel2, aes(x=Incubation,y=value,fill=Comp))+
  geom_bar(stat = "identity",position = position_stack(reverse = T))+
  scale_fill_manual(values =  c("#BC3C29FF","#EFC000FF","#008B45FF","#5F559BFF","grey50"))+
  facet_wrap(Type~Temp, scales = "free", ncol=3)+
  scale_y_continuous("Proportions (%)",labels = scales::percent_format(accuracy = 1,
                                                                       decimal.mark = '.',scale = 1),expand = c(0.01,0.01))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 14,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 1.0,
                                   margin = unit(c(0.02,0.2,0.2,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.text.y = element_text(size = 16, colour = "black",face=2 ),
        axis.title.x = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm")),
        axis.title.y = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 16, colour = "black",face = 2,margin = unit(c(0.2,0.2,0.2,0.2),"cm")),
        strip.text.y = element_blank(),
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 16,face=2, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 14,face=2, colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm")),
        legend.key.width = unit(0.8,"cm"),
        legend.key.height = unit(0.8,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.5, 0.5),
        legend.position = "bottom")+
  guides(fill=guide_legend(title = "Chemical composition"))+
ggsave(filename("Chemicalcomposition_bar"),height = 34, width = 40, units = "cm", dpi = 300)

comp_som$Comp=factor(comp_som$Comp,levels = c("CHO","CHON","CHOS","CHONS","Remainders"))

comp_som$Temp=factor(comp_som$Temp, levels=c("Ctrl","5","15","25"))
comp_som$Incubation=factor(comp_som$Incubation,levels = c("0","70","154","223","336"))

##molecular category=====
som_all

cat_som=as.data.table(aggregate(som_all$Bromo.Inty, 
                                 by=list(Sample=som_all$Sample,Category=som_all$Molecularcategory),sum))

cat_som_tot=as.data.table(aggregate(som_all$Bromo.Inty, 
                                     by=list(Sample=som_all$Sample),sum)) %>% `colnames<-`(c("Sample","Tot"))

cat_som=cat_som %>% inner_join(cat_som_tot)
cat_som
cat_som$rel=cat_som$x/cat_som$Tot*100
cat_som

aggregate(cat_som$rel, by=list(Sample=cat_som$Sample),sum)

som_all
som_id=unique(som_all[,c("Sample","Type","Incubation","Temp","Rep")])

cat_som=cat_som %>% inner_join(som_id)
cat_som

cat_som_save_inty=melt(cat_som[,c("Sample","Category","Type","Incubation","Temp","Rep","rel")],id.vars = c("Sample","Category","Type","Incubation","Temp","Rep")) %>% 
  dcast(Sample+Type+Incubation+Temp+Rep~Category,mean)
cat_som_save_inty

fwrite(cat_som_save_inty,file = "Datafile/mole_group_inty.csv")
cat_som_freq=as.data.table(aggregate(som_all$Freq, 
                                by=list(Sample=som_all$Sample,Category=som_all$Molecularcategory),sum))

cat_som_freq_tot=as.data.table(aggregate(som_all$Freq, 
                                    by=list(Sample=som_all$Sample),sum)) %>% `colnames<-`(c("Sample","Tot"))

cat_som_freq=cat_som_freq %>% inner_join(cat_som_freq_tot)
cat_som_freq
cat_som_freq$rel=cat_som_freq$x/cat_som_freq$Tot*100
cat_som_freq

cat_som_freq=cat_som_freq %>% inner_join(som_id)
cat_som_save_freq=melt(cat_som_freq[,c("Sample","Category","Type","Incubation","Temp","Rep","rel")],id.vars = c("Sample","Category","Type","Incubation","Temp","Rep")) %>% 
  dcast(Sample+Type+Incubation+Temp+Rep~Category,mean)
cat_som_save_freq
fwrite(cat_som_save_freq,file = "Datafile/mole_group_freq.csv")

cat_som_m=melt(cat_som[,c("Category","Type","Incubation","Temp","Rep","rel")],id.vars = c("Category","Type","Incubation","Temp","Rep")) %>% 
  dcast(Type+Incubation+Temp~Category,mean) %>% 
  melt(id.vars=c("Type","Incubation","Temp"),variable.name = "Category")

cat_som_m

unique(cat_som_m$Category)
cat_som_m$Category=factor(cat_som_m$Category,
                          levels = c("Combustion-drived PCA",
                                     "Soil-derived polyphenols and PCAs with aliphatic chains",
                                     "Soil derived humics and high unsaturated compounds",
                                     "Unsaturated aliphatic compounds",
                                     "Saturated fatty and sulfonic acid,carbohyrates"),
                          labels  = c("Combustion-drived PCA",
                                              "Soil-derived polyphenols and PCA with aliphatic chains",
                                              "Soil-derived humics and highly unsaturated compounds",
                                              "Unsaturated aliphatic compounds",
                                              "Saturated fatty and sulfonic acids, carbohydrates"))
cat_som_m
cat_som_m_sel=subset(cat_som_m,cat_som_m$Temp!="Ctrl") %>% droplevels()
cat_som_m_sel

cat_som_m_ctr=subset(cat_som_m,cat_som_m$Temp=="Ctrl")

cat_som_m_ctr5=cat_som_m_ctr
cat_som_m_ctr5$Temp="5"

cat_som_m_ctr15=cat_som_m_ctr
cat_som_m_ctr15$Temp="15"

cat_som_m_ctr25=cat_som_m_ctr
cat_som_m_ctr25$Temp="25"

cat_som_m_ctr_all=rbind(cat_som_m_ctr5,cat_som_m_ctr15,cat_som_m_ctr25)
cat_som_m_ctr_all

cat_som_m_sel2=rbind(cat_som_m_sel,cat_som_m_ctr_all)
cat_som_m_sel2

cat_som_m_sel2$Incubation=factor(cat_som_m_sel2$Incubation, levels = c(0,70,154,223,336))
cat_som_m_sel2

cat_som_m_sel2$Temp=factor(cat_som_m_sel2$Temp,levels = c(5,15,25))

ggplot(cat_som_m_sel2, aes(x=Incubation,y=value,fill=Category))+
  geom_bar(stat = "identity",position = position_stack(reverse = T))+
  scale_fill_manual(values =  c("#BCCEB8","#9D95CA","#A6BFD5","#FAD692","#FDB2AD"))+
  facet_wrap(Type~Temp, scales = "free", ncol=3)+
  scale_y_continuous("Proportions (%)",labels = scales::percent_format(accuracy = 1,
                                                                       decimal.mark = '.',scale = 1),expand = c(0.01,0.01))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 14,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 1.0,
                                   margin = unit(c(0.02,0.2,0.2,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.text.y = element_text(size = 16, colour = "black",face=2 ),
        axis.title.x = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm")),
        axis.title.y = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 16, colour = "black",face = 2,margin = unit(c(0.2,0.2,0.2,0.2),"cm")),
        strip.text.y = element_blank(),
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 18, face=2,colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 14, face=2,colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm")),
        legend.key.width = unit(0.8,"cm"),
        legend.key.height = unit(0.8,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.85, 0.5),
        legend.position = "bottom")+
  guides(fill=guide_legend(title = "Molecular groups",title.hjust = 0.5,title.position = "top", nrow = 2, byrow = T))+
  ggsave(filename("Category_bar"),height = 34, width = 40, units = "cm", dpi = 300)

##chemodiversity=====
som_all$Freq=1
som_all

div=as.data.table(aggregate(som_all$Freq, by=list(Sample=som_all$Sample),sum)) %>% `colnames<-`(c("Sample","Diversity"))
div

##Chemical properties=====
som_all

som_chp=melt(som_all[,c("Sample","AI","DBE","O/C","H/C")], id.vars = c("Sample")) %>% 
  dcast(Sample~variable, mean) %>% 
  melt(id.vars=c("Sample"))

som_chp

som_chp=som_chp %>% inner_join(som_id)
som_chp

som_chp$Incubation=factor(som_chp$Incubation,levels = c(0,70,154,223,336))
som_chp$Temp=factor(som_chp$Temp, levels = c("Ctrl",5,15,25),
                    labels = c("Before incubation","5°C","15°C","25°C"))
som_chp

som_chp$variable=factor(som_chp$variable,levels = c("AI","DBE","O/C","H/C"),
                        labels = c("Mean AI", "Mean DBE","Mean O/C", "Mean H/C"))


ggplot(som_chp, aes(x=Incubation,y=value,fill=Temp))+
  stat_boxplot(geom = "errorbar",color="black", position = position_dodge(width = 0.75), width=0.5)+
  geom_boxplot()+
  geom_vline(xintercept = 1.5, lty=2)+
  geom_vline(xintercept = 2.5, lty=2)+
  geom_vline(xintercept = 3.5, lty=2)+
  geom_vline(xintercept = 4.5, lty=2)+
  scale_x_discrete(name="Incubation Days")+
  #scale_fill_manual(values = c("#374E55FF","#DF8F44FF","#00A1D5FF","#CD534CFF","#5F559BFF","#79AF97FF","#4A6990FF","grey70"))+
  scale_fill_manual(values = c("grey50","#145DA0","#478C5C","#DF362D"))+
  facet_wrap(Type+variable~., scales = "free", ncol=4)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 1.0,
                                   margin = unit(c(0.2,0.2,0.4,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.text.y = element_text(size = 16, colour = "black",face=2 ),
        axis.title.x = element_text(size = 18,face=2, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm")),
        axis.title.y = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 16, colour = "black",face = 2,margin = unit(c(0.2,0.2,0.4,0.2),"cm")),
        strip.text.y = element_blank(),
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 20, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 18, hjust = 0,colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm")),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "vertical",
        legend.justification=c(0.5, 0.5),
        legend.position = "right")+
  guides(fill=guide_legend(title = "Temperature"))+
  ggsave(filename("chp_dis2"),height = 27, width = 60, units = "cm", dpi = 300)


som_chp_sel=subset(som_chp,som_chp$Temp!="Ctrl") %>% droplevels()
som_chp_sel

som_chp_ctr=subset(som_chp,som_chp$Temp=="Ctrl")

som_chp_ctr5=som_chp_ctr
som_chp_ctr5$Temp="5"

som_chp_ctr15=som_chp_ctr
som_chp_ctr15$Temp="15"

som_chp_ctr25=som_chp_ctr
som_chp_ctr25$Temp="25"

som_chp_ctr_all=rbind(som_chp_ctr5,som_chp_ctr15,som_chp_ctr25)
som_chp_ctr_all

som_chp_sel2=rbind(som_chp_sel,som_chp_ctr_all)
som_chp_sel2

som_chp_sel2$Incubation=factor(som_chp_sel2$Incubation, levels = c(0,70,154,223,336))
som_chp_sel2

som_chp_sel2$Temp=factor(som_chp_sel2$Temp,levels = c(5,15,25))


som_chp_sel2_d=dcast(som_chp_sel2,Sample+Type+Incubation+Temp+Rep~variable, mean)
som_chp_sel2_d

fwrite(som_chp_sel2_d, file = "Datafile/som_chp.csv")

som_chp_sel3=subset(som_chp_sel2,som_chp_sel2$variable=="Mean AI"|som_chp_sel2$variable=="Mean DBE"|
                      som_chp_sel2$variable=="Mean O/C"|
                    som_chp_sel2$variable=="Mean H/C")
som_chp_sel3


ggplot(som_chp_sel3, aes(x=Type,y=value,fill=Incubation))+
  geom_boxplot()+
  #scale_fill_manual(values = c("#374E55FF","#DF8F44FF","#00A1D5FF","#CD534CFF","#5F559BFF","#79AF97FF","#4A6990FF","grey70"))+
  scale_fill_manual(values = c("grey50","#FD812D","#8BCD50","#6BCAE2","#8A6FDF"))+
  facet_wrap(Temp+variable~., scales = "free", ncol=4)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 18,colour = "black",face = 2, angle = 0, hjust = 0.5,vjust = 1.0),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.text.y = element_text(size = 16, colour = "black",face=2 ),
        axis.title.x = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.0,0.0,0.0),"cm")),
        axis.title.y = element_text(size = 0, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 16, colour = "black",face = 2,margin = unit(c(0.2,0.2,0.1,0.2),"cm")),
        strip.text.y = element_blank(),
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 22, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 18, colour = "black",margin = unit(c(0.2,0.2,0.2,0.0),"cm")),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(0.5, 0.5),
        legend.position = "bottom")+
  guides(fill=guide_legend(title = "Incubation (Days)"))+
  ggsave(filename("chp_dis_all"),height = 34, width = 60, units = "cm", dpi = 300)


som_chp_sel2_d


