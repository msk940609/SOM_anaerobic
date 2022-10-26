library(lemon)
library(dplyr)
library(tidyr)
library(data.table)
source("Rscript/func_filename.R")


lf=list.files(path = "Transformations per Peak/Dataset_Name/", pattern = ".csv")
lf

dt=data.table()
for (i in 1:length(lf)) {
#  i=1
  
  temp=fread(paste0("Transformations per Peak/Dataset_Name/",lf[i]))
  
  dt=rbind(dt,temp)
  
}

dt

table(dt$sample)

dt



trans_num_all=fread("Datafile/Num.Peak.Trans_Allms.csv")
trans_num_all$Type="All"

trans_num_al=fread("Datafile/Num.Peak.Trans_ALms.csv")
trans_num_al$Type="Active layer (AL)"

trans_num_pf=fread("Datafile/Num.Peak.Trans_Sample_PFms.csv")
trans_num_pf$Type="Permafrost (PF)"


trans_num_all_merge=rbind(trans_num_all,trans_num_al,trans_num_pf)
trans_num_all_merge$Type=factor(trans_num_all_merge$Type, levels = c("All","Active layer (AL)", 
                                                                     "Permafrost (PF)"))

trans_num_type=subset(trans_num_all_merge,trans_num_all_merge$Type!="All")

ggplot(trans_num_all_merge, aes(x=`num.trans.involved.in`))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 6, lty=2, col="red")+
  geom_vline(xintercept = 26, lty=2, col="red")+
  #geom_vline(xintercept = 38, lty=2, col="red")+
  facet_rep_grid(.~Type, scales = "free",repeat.tick.labels = T)+
  scale_x_continuous(breaks = seq(0,110,20), limits = c(0,105))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 18,face = "bold", colour = "black"),
        axis.title = element_text(size = 16, face = "bold", colour = "black"),
        axis.text = element_text(size = 15, colour = "black"))+
  xlab("Number of transformation")+
  ylab("Counts")+
  ggsave(filename("Histo_num_trans"),height = 10, width = 30, units = "cm", dpi = 300)




