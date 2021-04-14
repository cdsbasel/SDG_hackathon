# SDG hackathon
# Group: GROUPPSEUDONYM

library(tidyverse)

# read data
publications <- read_csv('0_data/publications_sdg.csv')
projects <- read_csv('0_data/projects_sdg.csv')

pub = publications %>% select(starts_with("SDG")) %>% colSums() %>% t() %>% as_tibble() %>% pivot_longer(everything())
pro = projects %>% select(starts_with("SDG")) %>% colSums() %>% t() %>% as_tibble() %>% pivot_longer(everything())

ggplot(data = pub) +
  geom_col(mapping = aes(x = name, y = value, fill = name)) +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "SDG Frequencies"
  )
  





# non-quality visualization
pdf('2_figures/sdg_frequencies_GROUP_GROUPPSEUDONYM.pdf')
barplot(colMeans(publications[,paste0('SDG-',ifelse(1:17<10,paste0('0',1:17),1:17))]))
dev.off()