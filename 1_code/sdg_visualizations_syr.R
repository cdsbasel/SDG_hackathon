# SDG hackathon
# Group: syr

library(tidyverse)
library(ggplot2)

# read data
publications <- read_csv('0_data/publications_sdg.csv')
projects <- read_csv('0_data/projects_sdg.csv')

SDG <- publications %>%
  select(`SDG-01`:`SDG-17`) %>%
  summarise_all(sum)

SDG_long <- SDG %>%
  pivot_longer(starts_with("SDG"),names_to="SDG",values_to="count")
  

###The frequency distribution of SDG
pdf('2_figures/sdg_frequencies_GROUP_syr.pdf')
ggplot(SDG_long, aes(x=SDG, y=count))+
  geom_bar(stat = "identity")+
  labs(y="Number of publications")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45)) #+
  #scale_colour_viridis_d()
dev.off()




  
  





# non-quality visualization
pdf('2_figures/sdg_frequencies_GROUP_syr.pdf')
barplot(colMeans(publications[,paste0('SDG-',ifelse(1:17<10,paste0('0',1:17),1:17))]))
dev.off()