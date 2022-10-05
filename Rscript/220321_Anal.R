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


source("Rscript/func_filename.R")

allist=dir(pattern = ".xlsx",path = "Datafile/220314_KOPRI_AL/")

dt=data.table()
for (i in 1:length(allist)) {
  print(i)
  tempal=openxlsx::read.xlsx(xlsxFile = paste0("Datafile/220314_KOPRI_AL/",allist[i]))
  tempal$id=tools::file_path_sans_ext(allist[i])
  
  dt=rbind(dt,tempal)
}

dt_al=dt
unique(dt$id)
table(dt$id)

pflist=dir(pattern = ".xlsx",path = "Datafile/220314_KOPRI_PF/")

dt=data.table()
for (i in 1:length(pflist)) {
  print(i)
  temppf=openxlsx::read.xlsx(xlsxFile = paste0("Datafile/220314_KOPRI_PF/",pflist[i]))
  temppf$id=tools::file_path_sans_ext(pflist[i])
  
  dt=rbind(dt,temppf)
}
unique(dt$id)
table(dt$id)

dt_pf=dt

som_all=rbind(dt_al, dt_pf)



unique(som_all$id)
som_all=som_all %>% separate(id, c("Incu","Temp","Rep"))
som_all$Sample=paste(som_all$Incu,som_all$Temp,som_all$Rep,sep = "_")

fwrite(som_all,file = "Datafile/SOM_1st.csv")

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

som_all

#NMDS=====
ddm=melt(som_all[,c("Sample","Formula","Bromo.Inty")],id.vars = c("Sample","Formula")) %>% 
  dcast(Sample~Formula, value.var = "value",sum)

ddm[,1:23]

nmds=metaMDS(ddm[,-1],k=2)
nmds

ggnmds=as.data.frame(nmds$points)
ggnmds$Sample=ddm$Sample
ggnmds

som_label=unique(som_all[,c("Sample","Type","Incubation","Temp","Rep")])
som_label

ggnmds=ggnmds %>% left_join(som_label, by = "Sample")
ggnmds

ggnmds$Incubation=factor(ggnmds$Incubation,levels = c("0","70","154","230","336"))
ggnmds$Temp=factor(ggnmds$Temp, levels = c("Ctrl","05","15","25"),
                   labels = c("Ctrl","5 (°C)","15 (°C)","25 (°C)"))
ggnmds$id=paste0("D",ggnmds$Incubation,"_",ggnmds$Temp)

ggnmds$id=factor(ggnmds$id, levels = c("D70_5 (°C)","D70_15 (°C)","D70_25 (°C)",
                                       "D154_5 (°C)","D154_15 (°C)","D154_25 (°C)",
                                       "D230_5 (°C)","D230_15 (°C)","D230_25 (°C)",
                                       "D336_5 (°C)","D336_15 (°C)","D336_25 (°C)","D0_Ctrl"),
                 labels = c("D70_5 (°C)","D70_15 (°C)","D70_25 (°C)",
                            "D154_5 (°C)","D154_15 (°C)","D154_25 (°C)",
                            "D230_5 (°C)","D230_15 (°C)","D230_25 (°C)",
                            "D336_5 (°C)","D336_15 (°C)","D336_25 (°C)","Ctrl"))

ggnmds
?order()

#ggnmds=ggnmds[order(ggnmds$id,decreasing=T),]

ggplot(ggnmds, aes(x=MDS1,y=MDS2,shape=Type, fill=id))+
  geom_point(size=7)+
  scale_fill_manual(values = c("#E4AEC5","#F14668","#E04DB0",
                               "#FFBC80","#F76E11","#C36839",
                                "#B1E693","#4E9F3D","#085E7D",
                                "#90E0EF","#00B4D8","#113CFC",
                                "black"))+
  scale_shape_manual(values=c(23,21))+
  scale_x_continuous(name = "NMDS1",limits = c(-1.0,2))+
  scale_y_continuous(name = "NMDS2",limits = c(-1,1))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 2),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.text.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.title.x = element_text(size = 25, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.5,0,0.0,0),"cm")),
        axis.title.y = element_text(size = 25, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.0,0.2,0.0,0.0),"cm")),
        legend.text = element_text(size = 12, colour = "black", family = "Arial",vjust = 0.5, margin = unit(c(-0.2,0,-0.2,0),"cm")),
        legend.title = element_text(size = 16, colour = "black", family = "Arial", face = "bold"),
        legend.spacing = unit(1.4,"cm"),
        legend.position = c(0.50, 0.07),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.background = element_blank()
  )+
  guides(shape=guide_legend(title = "Type",title.position = "top",order = 1, override.aes = list(shape=c(23,21),fill=c("grey50","grey50") )),
         fill=guide_legend(title = "Day&temperature (°C)",title.position = "top",order = 2,
                           override.aes = list(shape=21,
                                               fill=c("#E4AEC5","#F14668","#E04DB0",
                                                      "#FFBC80","#F76E11","#C36839",
                                                      "#B1E693","#4E9F3D","#085E7D",
                                                      "#90E0EF","#00B4D8","#113CFC",
                                                      "black")),nrow = 3))+
  ggsave(filename("NMDS_all"),height = 25, width = 25, units = "cm", dpi = 300)



