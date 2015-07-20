# INTRODUCTION ------------------------------------------------------------
#   Taekyung Kim
#   kimtk@suwon.ac.kr
#   NK Landscape Practice
#   2015.07.
#' Fitness Contribution Function Generator
#'
#' This function generates a fitness contribution function
#'
#' @param N Node number (bit code count)
#' @param K dependencies
#' @param PI determining neighbors, default is one-bit off...
#' @param g contribution matrix structure (N by 2^(K+1)), default follows a uniform distribution
#'
#' @return a function(x), x is a numeric vector with 0 or 1, and the length equals N
#'
#' @seealso CEGO::benchmarkGeneratorNKL
#'
#' @examples
#' uniform_landscape_N4_K0 <- landscape_gen(N=4,K=0)
#' uniform_landscape_N4_K1 <- landscape_gen(4,1)
#' uniform_landscape_N4_K0(c(0,0,0,0))
#' uniform_landscape_N4_K0(c(1,0,0,0))
#' uniform_landscape_N4_K1(c(0,1,1,0))
landscape_gen <- function (N = 4, K = 0, PI = 1:K, g = NULL)
{
  if(N < K) return(NA)
  if (is.null(g)) {
    g <- matrix(runif(N * 2^(K + 1)), N)
  }
  bits = 2^(0:K)
  landscape <- function(x) {
    usum = 0
    usum0 = foreach(i = 1:N,.combine=c) %do% {
      if(K != 0) {
        xx <- x[c(i, ((i + PI - 1)%%N) + 1)]
      } else {
        xx <- x[i]
      }
      g[i, sum(bits * xx)+1]
    }
    usum = sum(usum0)
    return(usum/N)
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
