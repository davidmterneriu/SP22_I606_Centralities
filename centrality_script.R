#Centralities Assignment Feb 19th 2022
# SP2022 I606
#David Terner 
################################################################################

#Clear working env.
rm(list=ls())

#Load libs. 
library(readxl)
library(tidyverse)
library(ggplot2)
library(readr)
library(igraph)
library(tidygraph)
library(ggraph)


dolphin=read_graph("dolphins.gml",format = "gml")

node_count=length(V(dolphin))

top_n=3


dolphin=dolphin%>%
  set_vertex_attr("degree",value=degree(dolphin))%>%
  set_vertex_attr("betweenness",value=betweenness(dolphin))%>%
  set_vertex_attr("closeness",value=closeness(dolphin))%>%
  set_vertex_attr("eigen",value=eigen_centrality(dolphin)$vector%>%unlist()%>%
                    as.numeric())

dolphin_nodes=data.frame(id=V(dolphin)$id,
                         label=V(dolphin)$label,
                         degree=V(dolphin)$degree,
                         betweenness=V(dolphin)$betweenness,
                         closeness=V(dolphin)$closeness,
                         eigen=V(dolphin)$eigen)

dolphin_eign=dolphin_nodes%>%dplyr::arrange(-eigen)%>%
  dplyr::select(label)%>%
  head(top_n)%>%
  unlist()%>%
  as.character()

dolphin_close=dolphin_nodes%>%dplyr::arrange(-closeness)%>%
  dplyr::select(label)%>%
  head(top_n)%>%
  unlist()%>%
  as.character()

dolphin_between=dolphin_nodes%>%dplyr::arrange(-betweenness)%>%
  dplyr::select(label)%>%
  head(top_n)%>%
  unlist()%>%
  as.character()

# (1) Popularity contest 


dolphin%>%ggraph(layout = 'kk') + 
  geom_edge_link(alpha=0.2) + 
  geom_node_point(aes(size = eigen, colour = eigen)) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()+
  geom_node_label(aes(label = ifelse(label %in%dolphin_eign , as.character(label),
                                    NA_character_)),size=2)+
  labs(color="Eigenvalue Score",title ="Popularity contest",
       subtitle = "Dolphin Network")+
  scale_color_viridis_c()+
  scale_size(guide = 'none')


# (2) Relay 
#standardized score

dolphin%>%ggraph(layout = 'kk') + 
  geom_edge_link(alpha=0.1) + 
  geom_node_point(aes(size = closeness, 
                      colour = closeness*(node_count-1))) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()+ 
  geom_node_label(aes(label = ifelse(label %in%dolphin_close , as.character(label),
                                     NA_character_)),size=2)+
  labs(color="Closeness Score",title ="Relay",
       subtitle = "Dolphin Network")+
  scale_color_viridis_c()+
  scale_size(guide = 'none')

# (3) Gossip

dolphin%>%ggraph(layout = 'kk') + 
  geom_edge_link(alpha=0.1) + 
  geom_node_point(aes(size = betweenness, colour = betweenness)) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()+ 
  geom_node_label(aes(label = ifelse(label %in%dolphin_between , as.character(label),
                                     NA_character_)),size=2)+
  labs(color="Betweenness Score",title ="Gossip",
       subtitle = "Dolphin Network")+
  scale_color_viridis_c()+
  scale_size(guide = 'none')

dolphin_nodes=dolphin_nodes%>%
  mutate(id2=id+1)

dolphin_edgelist=as_data_frame(dolphin)

dist_matrix=distances(dolphin)

dist_matrix=dist_matrix%>%as.data.frame()%>%
  mutate(row_n=seq_along(V1))%>%
  gather(key=from1,value=dist,-row_n)%>%
  mutate(from1=gsub("V","",from1))%>%
  rename(from_d=row_n,to_d=from1)%>%
  filter(dist>0)


measure_compare=dist_matrix%>%inner_join(dolphin_nodes,by=c("from_d"="id2"))%>%
  group_by(dolphin=label,dist)%>%
  tally()%>%
  ungroup()%>%
  dplyr::group_by(dolphin)%>%
  summarise(rumor_time=max(dist))%>%
  inner_join(dolphin_nodes,by=c("dolphin"="label"))


measure_compare%>%
  gather(key="central_measures",value="v",-c(dolphin,id,id2,rumor_time))%>%
  dplyr::group_by(central_measures)%>%
  summarise(cor=cor(v,rumor_time,method = "kendall"))%>%
  arrange(cor)%>%
  kableExtra::kable(format = "latex",digits = 3,booktabs=T)


cbind(dolphin_nodes%>%arrange(-eigen)%>%
        mutate(rank=seq_along(eigen))%>%
        select(rank,label,eigen),
  dolphin_nodes%>%arrange(-closeness)%>%
    select(label,closeness),
  dolphin_nodes%>%arrange(-betweenness)%>%
    select(label,betweenness))%>%
  kableExtra::kable(format = "latex",digits = 3,booktabs=T,longtable = T)
  

