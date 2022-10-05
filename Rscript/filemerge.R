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

