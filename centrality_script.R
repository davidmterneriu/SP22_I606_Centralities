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

dolphin=read_graph("dolphins.gml",format = "gml")

dolphin=dolphin%>%
  set_vertex_attr("degree",value=degree(dolphin))%>%
  set_vertex_attr("betweenness",value=betweenness(dolphin))%>%
  set_vertex_attr("closeness",value=closeness(dolphin))
 


  
  