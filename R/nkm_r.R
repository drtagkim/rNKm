# INTRODUCTION ------------------------------------------------------------
#   Taekyung Kim
#   kimtk@suwon.ac.kr
#   NK Landscape Practice
#   2015.07.
# Dependencies ------------------------------------------------------------
# if(!require(CEGO)) {
#   install.packages("CEGO")
#   library(CEGO)
# }
# if(!require(foreach)) {
#   install.packages("foreach")
#   library(foreach)
# }
# if(!require(stringr)) {
#   install.packages("stringr")
#   library(stringr)
# }
# if(!require(plot3D)) {
#   install.packages("plot3D")
#   library(plot3D)
# }
# Int to Bit Representation -----------------------------------------------
landscape_gen <- function (N = 10, K = 1, PI = 1:K, g = NULL)
{
  if (is.null(g)) {
    g <- matrix(runif(N * 2^(K + 1)), N)
  }
  bits = 2^(0:K)
  landscape <- function(x) {
    usum = 0
    for (i in 1:N) {
      xx <- x[c(i, ((i + PI - 1)%%N) + 1)]
      usum = usum + g[i, sum(bits * xx) + 1]
    }
    -usum/N
  }
}

int2bit <- function(x,precision=0) {
  if(x > 0 ) {
    precision0 = ifelse(x%%2==0,log2(x),log2(x-1)) + 1
  } else {
    precision0 = 1
  }
  bit_numbers = as.integer(intToBits(x))
  if(precision > 0) precision0=precision
  bit_numbers[1:precision0]
}
# Get Neighbors -----------------------------------------------------------
get_neighbors <- function(location_id,N,element_id) {
  bit_id = as.logical(int2bit(location_id,N))
  bit_id[element_id] = !bit_id[element_id]
  bit_id_out = as.integer(bit_id)
  sum(foreach(idx=0:(N-1),i=bit_id_out,.combine=c) %do% {i*2^idx})
}
# Get All Neighbors -------------------------------------------------------
get_all_neighbors <- function(location_id,N) {
  an = foreach(element_id=1:N,.combine=c) %do% {
    get_neighbors(location_id,N,element_id)
  }
  return(an)
}
# Plot Space Functions ----------------------------------------------------
bit2int <- function(bit_data) {
  a = as.integer(bit_data)
  b = 0:(length(a)-1)
  c = sum(a*2^(b))
  return(c)
}
b2 <- function(m) matrix(unlist(lapply(str_split(m,""),bit2int)),nrow=dim(m)[1],ncol=dim(m)[2])
expand.m <- function(m) {
  N = nchar(m[1])
  m.dim = dim(m)
  m0 = matrix(str_c("0",as.character(m)),nrow=m.dim[1],ncol=m.dim[2])
  m1 = matrix(str_c("1",as.character(m)),nrow=m.dim[1],ncol=m.dim[2])
  if(N %% 2==0) {
    div.middle = m.dim[2] %/% 2
    m2 = m1[,div.middle:1]
    m3 = m1[,m.dim[2]:(div.middle+1)]
    m.out = cbind(m2,m0,m3)
  } else {
    div.middle = m.dim[1] %/% 2
    m2 = m1[div.middle:1,]
    m3 = m1[m.dim[1]:(div.middle+1),]
    m.out=rbind(m2,m0,m3)
  }
  rownames(m.out) <- colnames(m.out) <- NULL
  return(m.out)
}
create_plot_landscape <- function(N=2) {
  a0 <- matrix(c("00","01","10","11"),2)
  if(N==2) return(a0)
  for(i in 3:N) {
    a0 <- expand.m(a0)
  }
  a0
}
