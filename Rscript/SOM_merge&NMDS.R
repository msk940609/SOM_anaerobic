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

source("Rscript/func_filename.R")

allist=dir(pattern = ".xlsx",path = "Datafile/220314_KOPRI_AL/")

dt=data.table()
for (i in 1:length(allist)) {
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


ddm=melt(som_all[,c("Sample","Formula","Bromo.Inty")],id.vars = c("Sample","Formula")) %>% 
  dcast(Sample~Formula, value.var = "value",sum)


ddm[,1:23]


nmds=metaMDS(ddm[,-1],k=2)
nmds

ggnmds=as.data.frame(nmds$points)

ggnmds$id=ddm$Sample

ggnmds

ggnmds=ggnmds %>% separate(id, c("Incu","Temp","Rep"))
ggnmds=ggnmds %>% separate(Incu,c("Type","Day"),sep = "D")
ggnmds$Temp=ifelse(ggnmds$Temp=="ctr","ctrl",ggnmds$Temp)
#ggnmds$Day=as.numeric(ggnmds$Day)

ggnmds$Day=factor(ggnmds$Day,levels = c("0","70","154","230","336"))
ggnmds$Sample=paste(ggnmds$Day,ggnmds$Temp,sep = "_")


ggplot(ggnmds, aes(x=MDS1,y=MDS2, col=Sample, shape=Type))+
  geom_point(size=4)


