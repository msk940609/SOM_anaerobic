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

allist=dir(pattern = ".xlsx",path = "Datafile/CTL/")

dt=data.table()
for (i in 1:length(allist)) {
  tempal=openxlsx::read.xlsx(xlsxFile = paste0("Datafile/CTL/",allist[i]))
  tempal$id=tools::file_path_sans_ext(allist[i])
  
  dt=rbind(dt,tempal)
}

dt_al=dt
unique(dt$id)
table(dt$id)

pflist=dir(pattern = ".xlsx",path = "Datafile/DEEP/")

dt=data.table()
for (i in 1:length(pflist)) {
  temppf=openxlsx::read.xlsx(xlsxFile = paste0("Datafile/DEEP/",pflist[i]))
  temppf$id=tools::file_path_sans_ext(pflist[i])
  
  dt=rbind(dt,temppf)
}
unique(dt$id)
table(dt$id)

dt_pf=dt

som_all=rbind(dt_al, dt_pf)
table(som_all$id)



som_all=fread("Datafile/SOM_1st.csv")


som_all$Sample=paste(som_all$Type,som_all$Temp,som_all$Incubation,sep = "_")
som_all


som_all$Freq=1

som_all_cnt=aggregate(som_all$Freq,by=list(Sample=som_all$Sample,Formula=som_all$Formula),sum)

som_all_cnt_sel=subset(som_all_cnt,som_all_cnt$x>1)
length(unique(som_all_cnt_sel$Formula)) ##0<10505, 1< 7010 (selected), 2< 5906

fm_list=unique(som_all_cnt_sel$Formula)

som_all_sel=som_all %>% left_join(som_all_cnt_sel)

som_all_fin=subset(som_all_sel,som_all_sel$x>1)

fwrite(som_all_fin,file = "Datafile/SOM_sel.csv")


som_all_m=melt(som_all_fin[,c("Calc.m/z","Bromo.Inty","Sample")], id.vars = c("Calc.m/z","Sample") )
som_all_m

som_all_m_d=dcast(som_all_m,`Calc.m/z`~Sample,value.var = "value",mean)
som_all_m_d[is.na(som_all_m_d)] <- 0

dim(som_all_m_d)
fwrite(som_all_m_d,file = "Datafile/SOM_FTICR_sel.csv")


som_all

som_chp=unique(som_all_fin[,c("Calc.m/z","C#","H#","N#","O#","S#","DBE","AI")])

som_chp=som_chp[order(som_chp$`Calc.m/z`),]

dim(som_chp)

fwrite(som_chp,file = "Datafile/SOM_chp_sel.csv")


