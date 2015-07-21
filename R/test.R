# Test: NK Landscape ------------------------------------------------------
#' NK Lanscape simulation Test
#'
#' A simple test unit for NKM
#'
#' @param N N
#' @param K K
#'
#'
test_nkm <- function(N,K,loudly=TRUE) {
  loc_ids = seq(0,2^N-1)
  loc_bit_ids = foreach(id=loc_ids,.combine=rbind) %do% int2bit(id,N)
  rownames(loc_bit_ids) = loc_ids
  #loc_bit_ids
  contributions = t(as.matrix(loc_bit_ids))
  fun <- landscape_gen(N,K)
  fitness_values = foreach(id=contributions,.combine=c) %do% fun(id)
  nk_landscape = cbind(loc_bit_ids,fitness_values)
  result = foreach(current_loc_id=loc_ids,.combine=c) %do% {
    path=current_loc_id
    while(TRUE) {
      fnow = nk_landscape[current_loc_id+1,N+1]
      if(loudly) cat("Fitness now:",fnow,"\n")
      nk.result = nk_landscape[get_all_neighbors(current_loc_id,N)+1,] #In R, ID starts with 1
      if(loudly) print(nk.result)
      if(fnow < max(nk.result[,N+1])) {
        new_pos=as.integer(rownames(nk.result)[which.max(nk.result[,N+1])])
        if(loudly) cat("From",current_loc_id,"to",new_pos,"\n")
        path=c(path,new_pos)
        current_loc_id = new_pos
      } else {
        if(loudly) cat("Optimal\n")
        break
      }
    }
    current_loc_id
  }
  results = list()
  results$result = result
  results$fun = fun
  results$N=N
  results$K=K
  return(results)
}
#' Plot of Landscape Test
#'
#' (under development)
#'
#' @param peaks test_nkm object
test_plot <- function(peaks) {
  N=peaks$N
  K=peaks$K
  fun=peaks$fun
  peak_ids=unique(peaks$result)
  loc_ids = seq(0,2^N-1)
  loc_bit_ids = foreach(id=loc_ids,.combine=rbind) %do% int2bit(id,N)
  rownames(loc_bit_ids) = loc_ids
  #loc_bit_ids
  contributions = t(as.matrix(loc_bit_ids))
  fun <- landscape_gen(N,K)
  fitness_values = foreach(id=contributions,.combine=c) %do% fun(id)
  nk_landscape = cbind(loc_bit_ids,fitness_values)
  a <- create_plot_landscape(N=N)
  b <- b2(a)
  idx = foreach(pid = peak_ids,.combine=c) %do% which(b==pid)
  cc <- matrix(nk_landscape[b+1,ncol(nk_landscape)],nrow=dim(b)[1],ncol=dim(b)[2])
  cc0 <- matrix(0,nrow=dim(b)[1],ncol=dim(b)[2])
  cc0[idx] = cc[idx]
  M <- mesh(1:dim(cc0)[1],1:dim(cc0)[2])
  par(mar=c(0,0,0,0))
  windows()
  surf3D(M$x,M$y,cc0,colkey=NULL,bty='b2', inttype=2, border='black',col=alpha.col(col="grey",0.6))
}
create_nkm_simple <- function(N,K) {
  loc_ids = seq(0,2^N-1)
  loc_bit_ids = foreach(id=loc_ids,.combine=rbind) %do% int2bit(id,N)
  rownames(loc_bit_ids) = loc_ids
  #loc_bit_ids
  contributions = t(as.matrix(loc_bit_ids))
  fun <- landscape_gen(N,K)
  fitness_values = foreach(id=contributions,.combine=c) %do% fun(id)
  nk_landscape = cbind(loc_bit_ids,fitness_values)
  rv = list()
  rv$N = N
  rv$K = K
  rv$loc_ids = loc_ids
  rv$loc_bit_ids = loc_bit_ids
  rv$fun <- fun
  rv$fitness_values = fitness_values
  rv$nk_landscape = nk_landscape
  rv
}
