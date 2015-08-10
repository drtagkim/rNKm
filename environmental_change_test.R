#environmental change
library(igraph)
library(intergraph)
inf_graph <- graph.empty() + vertices(c(paste("a",1:4,sep="_"),paste("b",1:4,sep="_"),paste("c",1:4,sep="_")))
inf_graph <- inf_graph + edges(c('a_1','a_2', 'a_1','a_3', 'a_1','a_4'))
inf_graph <- inf_graph + edges(c('a_2','a_3', 'a_2','a_4'))
inf_graph <- inf_graph + edges(c('a_3','a_1', 'a_3','a_2', 'a_3','a_4'))
inf_graph <- inf_graph + edges(c('b_1','b_2', 'b_1','b_3', 'b_1','b_4'))
inf_graph <- inf_graph + edges(c('b_2','a_1', 'b_2','b_1', 'b_2','b_3', 'b_2','b_4'))
inf_graph <- inf_graph + edges(c('b_3','a_2', 'b_3','b_1', 'b_3','b_2', 'b_3','b_4'))
inf_graph <- inf_graph + edges(c('b_4','b_1', 'b_4','b_2', 'b_4','b_3'))
inf_graph <- inf_graph + edges(c('c_1','b_4', 'c_1','c_2', 'c_1','c_3', 'c_1','c_4'))
inf_graph <- inf_graph + edges(c('c_2','c_1', 'c_2','c_3', 'c_2','c_4'))
inf_graph <- inf_graph + edges(c('c_3','c_1', 'c_3','c_2', 'c_3','c_4'))
inf_graph <- inf_graph + edges(c('c_4','b_3', 'c_4','c_1', 'c_4','c_2', 'c_4','c_3'))

inf_mat <- intergraph:::as.matrix.igraph(inf_graph)

new_conf <- c('a_1','a_2','b_4','c_2',
              'a_3','b_1','b_3','c_1',
              'a_4','b_2','c_3','c_4')
new_inf_mat <- redesign_influence_matrix(inf_mat,new_conf)
new_inf_mat$a


a <- matrix(c(1,1,1,0,0,1,1,1,1,0,1,1,1,1,0,1),4)
rownames(a) <- colnames(a) <- c("a","b","c","d")
b <- c("b","a","d","c")
e <- redesign_influence_matrix(a,b)
e
a
