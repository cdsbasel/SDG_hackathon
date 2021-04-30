library(tidyverse)

unibas_cols = c(mint = "#A5D7D2",
         mint_hell = "#D2EBD9",
         red = "#D20537",
         anthra = "#2D373C",
         anthra_hell = "#46505A")
names(unibas_cols) = NULL


# read data
publications <- read_csv('0_data/publications_sdg.csv')
projects <- read_csv('0_data/projects_sdg.csv')

# count occurences
pub = publications %>% 
  select(starts_with("SDG") & ends_with("Elsevier")) %>% 
  summarize_all(sum) %>% 
  pivot_longer(everything()) %>% 
  mutate(item_type = "publication",
         name = substring(name,5,6),
         value = value/sum(value))

pro = projects %>% 
  select(type, starts_with("SDG") & ends_with("Elsevier")) %>% 
  group_by(type) %>% 
  summarize_all(sum) %>% 
  pivot_longer(-1) %>% 
  rename(item_type = type) %>% 
  group_by(item_type) %>% 
  mutate(name = substring(name,5,6),
         value = value/sum(value)) %>% 
  ungroup()


elsevier_counts = c(11323, 93390,3349291,25550,35625, 46074,383354,89498,
                    39114, 47227, 141331, 84127, 180102, 104532, 111202, 169330)

elsevier = tibble(item_type = "reference",name = pub$name, value = elsevier_counts / sum(elsevier_counts))

sdg_props = pub %>% bind_rows(pro) %>% 
  mutate(item_type = as_factor(item_type)) %>% 
  rename(`Research item` = item_type)
levels(sdg_props$`Research item`) = c("Publication", "Projects (external)", "Projects (internal)")

ggplot(sdg_props, aes(x = name, y = value)) + 
  geom_bar(stat = 'identity', position = "dodge", mapping = aes(fill = `Research item`)) + 
  geom_segment(data = elsevier %>% mutate(x = (1:16)-.45, xend = (1:16)+.45), 
               mapping = aes(x = x, xend=xend, y = value, yend=value), col = unibas_cols[3], size = .3) +
  theme_minimal() + scale_fill_manual(values = unibas_cols[c(5,1,2)]) +
  labs(x = "Sustainable development goal", y = "Relative frequency")




cnts = publications %>% 
  select(starts_with("SDG") & ends_with("Elsevier")) %>% 
  rowSums()









pro = projects %>% pivot_longer(45:59 )
pro2 <- pro %>% group_by(type,name, value) %>% tally() %>% filter(value == TRUE)
pro2$name <-substring(pro2$name,1,6)

# for plot org-unit/sdg
publications_all <- publications %>%
  pivot_longer(starts_with("Org-"), names_to = "Organisations") %>%
  filter(value == TRUE) %>%
  group_by(Organisations) %>%
  select(`SDG-01-Elsevier`:`SDG-16-Elsevier`) %>%
  summarise_all(sum) 

publications_total <- publications_all %>%
  pivot_longer(`SDG-01-Elsevier`:`SDG-16-Elsevier`, names_to = "SDG") %>% 
  group_by(Organisations) %>%
  mutate(value = value/sum(value))
publications_total$SDG <-substring(publications_total$SDG,1,6)
publications_total$Organisations2 <-substring(publications_total$Organisations,5,)
