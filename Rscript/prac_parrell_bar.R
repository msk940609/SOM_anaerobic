data <- as.data.frame(Titanic)

data=data[,c(1,4,3,2,5)]

data2 <- gather_set_data(data, 1:4)

data2

ggplot(data2, aes(x, id = id, split = y, value = Freq)) +
  geom_parallel_sets(aes(fill = Class), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1, colour = "lightgrey", fill = "steelblue") +
  geom_parallel_sets_labels() + 
  theme_no_axes()


data2_1st=subset(data2,data2$Class=="1st")
data2_c=subset(data2,data2$Class=="Crew")


aggregate(data2_1st$Freq, by=list(YN=data2_1st$Survived),sum)
aggregate(data2_c$Freq, by=list(YN=data2_c$Survived),sum)
