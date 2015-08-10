#Modularity Test
library(igraph)
library(intergraph)
g <- graph(c(1,1, 2,1, 1,2, 2,2, 3,3, 4,3, 3,4, 4,4))
plot(g)
inf_mat <- intergraph:::as.matrix.igraph(g,"adjacency")
cont_mat <- rNKm:::convert_influence_contribution(inf_mat)
land_fun <- rNKm:::landscape_gen(N=dim(inf_mat)[1],K=2)
land_fun(c(1,1,1,1))
land_fun(c(1,0,0,1))

my.small_world <- sample_smallworld(1,16,4,0.01)
V(my.small_world)$size <- scales:::rescale(igraph:::degree(my.small_world),c(5,20))
plot(my.small_world,layout=layout.circle)

inf_mat <- intergraph:::as.matrix.igraph(my.small_world,"adjacency")
diag(inf_mat) <- 1
inf_mat

N=16
K=max(rowSums(inf_mat))
cont_mat <- convert_influence_contribution(inf_mat)
cont_mat
fun <- landscape_gen(N,K,PI=cont_mat)
fun(c(1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0))

a = matrix(c(1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1),nrow=4,byrow=T)
N=4;K=3
b = convert_influence_contribution(a)
b
fun <- landscape_gen(N,K,PI=b)
fun(c(1,1,1,1))
