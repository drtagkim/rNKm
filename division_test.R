#division
N=6
K=2
landscape_sp <- landscape_structure_uniform(N=N,K=K)
fun1 <- landscape_gen(N=N,K=K,sub_idx=c(1,2,3),g = landscape_sp)
fun2 <- landscape_gen(N=N,K=K,sub_idx=c(4,5,6),g = landscape_sp)
test_policy = c(1,1,1,0,1,0)
fitness1 = fun1(test_policy)
fitness2 = fun2(test_policy)
fitness = mean(c(fitness1,fitness2))
fitness
