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


dolphin=dolphin%>%
  set_vertex_attr("degree",value=degree(dolphin))%>%
  set_vertex_attr("betweenness",value=betweenness(dolphin))%>%
  set_vertex_attr("closeness",value=closeness(dolphin))%>%
  set_vertex_attr("eigen",value=eigen_centrality(dolphin)$vector%>%unlist()%>%
                    as.numeric())

dolphin%>%ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree, colour = degree)) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()

dolphin%>%ggraph(layout = 'kk') + 
  geom_edge_link(alpha=0.1) + 
  geom_node_point(aes(size = betweenness, colour = betweenness)) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()

dolphin%>%ggraph(layout = 'kk') + 
  geom_edge_link(alpha=0.1) + 
  geom_node_point(aes(size = closeness, colour = closeness)) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()


dolphin_nodes=data.frame(id=V(dolphin)$id,
           label=V(dolphin)$label,
           degree=V(dolphin)$degree,
           betweenness=V(dolphin)$betweenness,
           closeness=V(dolphin)$closeness,
           eigen=V(dolphin)$eigen)

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
  group_by(dolphin)%>%
  summarise(rumor_time=max(dist))%>%
  inner_join(dolphin_nodes,by=c("dolphin"="label"))



measure_compare%>%
  gather(key="central_measures",value="v",-c(dolphin,id,id2,rumor_time))%>%
  ggplot(aes(x=rumor_time,y=v))+
  geom_point()+
  facet_wrap(~central_measures,scales = "free_y")

library(GGally)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    #geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue",alpha=0.2, ...)
  p
}

measure_compare[,c("rumor_time","degree","betweenness","closeness",  "eigen")]%>%
  ggpairs(lower = list(continuous = my_fn))

  