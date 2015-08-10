if(!require(foreach)) {
  install.packages("foreach")
  library(foreach)
}
# creating experiment environment
experiment_gavettiLevinthal <- function(K) {
  nk.data = create_nkm_simple(6,K)
  agent_size=2^6
  result = search_hill_climbing_full(agent_size,nk.data$loc_ids,nk.data$nk_landscape,random_pos=F)
  stabilized_set = do.call(rbind,lapply(result,function(x){x %>% filter(from>0 & to<0)}))
  length(unique(stabilized_set$fitness_value))
}
# experiment executor
do_exp <- function(K,trial=4) {
  results <- foreach(i=1:trial,.combine=c) %do% {
    experiment_gavettiLevinthal(K)
  }
  sum(results)/trial
}
experiment_gavettiLevinthal2 <- function(K) {
  nk.data = create_nkm_simple_lowdim(6,K,1:3)
  agent_size=2^6
  result = search_hill_climbing_full(agent_size,nk.data$loc_ids,nk.data$nk_landscape,random_pos=F)
  stabilized_set = do.call(rbind,lapply(result,function(x){x %>% filter(from>0 & to<0)}))
  length(unique(stabilized_set$fitness_value))
}
# experiment executor
do_exp2 <- function(K,trial=4) {
  results <- foreach(i=1:trial,.combine=c) %do% {
    experiment_gavettiLevinthal2(K)
  }
  sum(results)/trial
}

experiment_gavettiLevinthal(3)
experiment_gavettiLevinthal2(3)


gen_lowdim_fraction(c(1,0,0,0),c(1,2))
