# Mapping-States-for-business
#learning from this site https://www.r-bloggers.com/mapping-the-best-states-for-business/
library(tidyverse)
library(rvest)
library(stringr)
library(fiftystater)


html.business <- read_html("https://chiefexecutive.net/2017-best-worst-states-business/")
html.business %>% 
  html_nodes('table') %>%
  .[[1]] %>%
  html_table() ->
  df.business

df.business %>% colnames()

colnames(df.business) <- c('rank_2017'
                           ,'state'
                           ,'text'
                           ,'rank_2016'
                           ,'change_in_rank'
)

df.business %>%
  as_tibble() ->
  df.business

df.business %>% glimpse()

df.business %>% 
  select(state
         ,rank_2016
         ,rank_2017
         ,change_in_rank
         ,text
  ) ->
  df.business

map.usa <- map_data('state')

map.usa %>% glimpse()

df.business %>%
  mutate(state = str_to_lower(state)) ->
  df.business

theme.map <- theme(
  text = element_text(family = 'Helvetica Neue', color = '#444444')
  ,panel.background = element_rect(fill = '#DDDDDD')
  ,plot.background = element_rect(fill = '#DDDDDD')
  ,legend.background = element_blank()
  ,legend.position = c(.9, .4)
  ,legend.key = element_blank()
  ,panel.grid = element_blank()
  ,plot.title = element_text(size = 18, face = 'bold')
  ,plot.subtitle = element_text(size = 12)
  ,axis.text = element_blank()
  ,axis.ticks = element_blank()
  ,axis.title = element_blank()
)

ggplot(df.business, aes(map_id = state)) +
  geom_map(aes(fill = rank_2017), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  theme.map +
  scale_fill_gradientn(colours = c('#009900','yellow', 'red')
                       ,breaks = c(1,10,20,30,40,50)) +
  labs(fill = str_c('Rank of best states\n'
                    ,'for business, 2017\n'
  )
  ,title = str_c('New York, California, and Illinois '
                 ,'rank very low among CEOs'
  )
  ,subtitle = str_c('chiefexecutive.net surveyed hundreds of CEOs and asked which states they prefered.\n'
                    ,'Large "blue state" states like California, Illinois, and New York ranked at the bottom\n'
                    ,'while Texas and Florida ranked at the top.  2017 is the 13th straight year with Texas\n'
                    ,'in the #1 position.\n'
  )
  ) +
  guides(fill = guide_colourbar(reverse = T)) +
  coord_map("albers", lat0 = 30, lat1 = 40) +
  annotate(geom = 'text'
           ,label = 'data source: chiefexecutive.net/2017-best-worst-states/'
           ,x = -85, y = 22
           ,size = 3
           ,color = '#666666'
  )

