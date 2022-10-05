
library(dplyr)
library(tidyr)
library(data.table)


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


trans_num=dt


trans_num=trans_num %>% separate(sample, c("Sam","Type","Temp","Incubation"))
trans_num$Sample=paste(trans_num$Type,trans_num$Temp,trans_num$Incubation, sep = "_")


trans_num2=trans_num
trans_num2$Type="All"

tt=subset(trans_num2,trans_num2$num.trans.involved.in<10)
length(unique(tt$peak)) ##252

length(unique(trans_num2$peak)) ##6947
252/6947*100
 
trans_num3=rbind(trans_num,trans_num2)
trans_num3$Type=factor(trans_num3$Type,levels = c("All","AL","PF"),
                       labels = c("All","Active layer (AL)","Permafrost (PF)"))



ggplot(trans_num3, aes(x=`num.trans.involved.in`))+
  geom_histogram(binwidth = 3)+
  facet_wrap(.~Type)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank())+
  xlab("Number of transformation")+
  ylab("Counts")+
  ggsave(filename("Histo_num_trans"),height = 10, width = 30, units = "cm", dpi = 300)


ggplot(trans_num3, aes(x=`num.trans.involved.in`))+
  geom_histogram(binwidth = 3)+
  facet_grid(.~Type, scales = "free")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 18,face = "bold", colour = "black"),
        axis.title = element_text(size = 16, face = "bold", colour = "black"),
        axis.text = element_text(size = 15, colour = "black"))+
  xlab("Number of transformation")+
  ylab("Counts")+
  ggsave(filename("Histo_num_trans"),height = 10, width = 30, units = "cm", dpi = 300)




