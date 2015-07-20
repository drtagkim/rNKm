# Test: NK Landscape ------------------------------------------------------
#' NK Lanscape simulation Test
#'
#' A simple test unit for NKM
#'
#' @param N N
#' @param K K
#'
#' @return optimal locations
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
  return(result)
}
#' Plot of Landscape Test
#'
#' (under development)
#'
#' @param N N
#' @param K K
test_plot <- function(N,K) {
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
  cc <- matrix(nk_landscape[b+1,ncol(nk_landscape)],nrow=dim(b)[1],ncol=dim(b)[2])
  # Shifting to Center ------------------------------------------------------
  m.idx <- which.max(cc)
  m.idx.row <- m.idx %% nrow(cc)
  m.idx.col <- ifelse(m.idx.row==0,m.idx%/%nrow(cc),m.idx%/%nrow(cc)+1)
  m.idx.middle <- trunc(median(1:nrow(cc)))
  cc.new <- cc
  if(m.idx.row < m.idx.middle) {
    diff.idx <- m.idx.middle - m.idx.row
    cc.new <- rbind(cc.new[nrow(cc.new):(nrow(cc.new)-(diff.idx+1)),],cc[1:(nrow(cc.new)-diff.idx),])
  } else {
    diff.idx <- m.idx.row - m.idx.middle
    cc.new <- rbind(cc.new[(diff.idx+1):nrow(cc.new),],cc.new[diff.idx:1,])
  }
  if(m.idx.col < m.idx.middle) {
    diff.idx <- m.idx.middle - m.idx.col
    cc.new <- cbind(cc.new[,ncol(cc.new):(ncol(cc.new)-diff.idx+1)],cc.new[,1:(ncol(cc.new)-diff.idx)])
  } else {
    diff.idx <- m.idx.col - m.idx.middle
    cc.new <- cbind(cc.new[,(diff.idx+1):ncol(cc.new)],cc.new[,diff.idx:1])
  }
  M <- mesh(1:dim(cc.new)[1],1:dim(cc.new)[2])
  windows()
  surf3D(M$x,M$y,cc.new,colkey=NULL,bty='b2', inttype=1, border='black',col=alpha.col(col="grey",alpha=0.5))
}
