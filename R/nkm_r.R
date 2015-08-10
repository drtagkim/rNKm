# INTRODUCTION ------------------------------------------------------------
#   Taekyung Kim
#   kimtk@suwon.ac.kr
#   NK Landscape Practice
#   2015.07.

#' Generating Random Numbers by Uniform Distribution
#'
#' Generating Random Numbers by Uniform Distribution
#'
#' @param N
#' @param K
#' @param seed_no Random seed if any
#'
#' @return Uniform distribution
#'
landscape_structure_uniform <-function(N=4,K=0,seed_no=NULL) {
  if(!is.null(seed_no)) {
    set.seed(seed_no)
  }
  g <- matrix(runif(N * 2 ^(K+1)), N)
  g
}

#' Fitness Contribution Function Generator
#'
#' This function generates a fitness contribution function
#'
#' @param N Node number (bit code count)
#' @param K dependencies
#' @param PI determining neighbors, default is one-bit off...
#' PI is a contribution matrix of N by K containing location ids
#' @param sub_idx index vector for sub-landscape design
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
#' contr_mat <- matrix(c(3,1,1,2,4,4,2,3),4)
#' landscape_fun <- landscape_gen(N=4,K=2,PI=contr_mat)
landscape_gen <- function (N = 4, K = 0, PI = NULL, sub_idx = NULL, g = NULL)
{
  PI_linear <- TRUE
  if(N < K) return(NA) #assertion
  if (is.null(g)) {
    g <- matrix(runif(N * 2^(K + 1)), N)
  }
  if (is.null(PI)) {
    PI <- 1:K
  } else {
    if(!is.matrix(PI)) return(NA) #assertion
    PI_linear <- FALSE
  }
  if(is.null(sub_idx)) {
    sub_idx = 1:N
  }
  bits = 2^(0:K)
  landscape <- function(x) {
    usum = 0
    usum0 = foreach(i = sub_idx,.combine=c) %do% {
      if(K != 0) {
        if(PI_linear) {
          xx <- x[c(i, ((i + PI - 1)%%N) + 1)]
        } else {
          #contribution matrix
          xx <- x[c(i, PI[i,])]
        }
      } else {
        xx <- x[i]
      }
      g[i, sum(bits * xx)+1]
    }
    usum = sum(usum0)
    return(usum/N)
  }
}

#' Fitness Contribution Function Generator
#'
#' This function generates a fitness contribution function
#'
#' @param N Node number (bit code count)
#' @param K dependencies
#' @param N1 Low dimension mask
#' @param PI determining neighbors, default is one-bit off...
#' PI is a contribution matrix of N by K containing location ids
#' @param sub_idx index vector for sub-landscape design
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
#' contr_mat <- matrix(c(3,1,1,2,4,4,2,3),4)
#' landscape_fun <- landscape_gen(N=4,K=2,PI=contr_mat)
landscape_gen_lowdim <- function (N = 4, K = 0, sub_idx=NULL, N1=1:4, PI = NULL, g = NULL)
{
  PI_linear <- TRUE
  if(N < K) return(NA) #assertion
  if (is.null(g)) {
    g <- matrix(runif(N * 2^(K + 1)), N)
  }
  if (is.null(PI)) {
    PI <- 1:K
  } else {
    if(!is.matrix(PI)) return(NA) #assertion
    PI_linear <- FALSE
  }
  if(is.null(sub_idx)) {
    sub_idx = 1:N
  }
  bits = 2^(0:K)
  landscape <- function(x) {
    usum = 0
    lowdim_fraction = t(gen_lowdim_fraction(x,N1))
    usum1 = foreach(lf=lowdim_fraction,.combine=c) %do% {
        lf = as.numeric(lf)
        usum0 = foreach(i = sub_idx,.combine=c) %do% {
          if(K != 0) {
            if(PI_linear) {
              xx <- lf[c(i, ((i + PI - 1)%%N) + 1)]
            } else {
              #contribution matrix
              xx <- lf[c(i, PI[i,])]
            }
          } else {
            xx <- lf[i]
          }
          g[i, sum(bits * xx)+1]
        }
        usum0
    }
    usum = sum(usum1)
    return(usum/(length(N1)*2^(N-length(N1))))
  }
}


#' Redesign Influence Matrix with Keeping a Structure
#'
#' Given a newly specified component structure, the influence matrix is redesigned.
#'
#' @seealso Ethiraj, S. K. and Levinthal, D. (2004). "Bounded Rationality and the Search for Organizational Architecture: An Evolutionary Perspective on the Design of Organizations and Their Evolvability," \emph{Administrative Science Quarterly}, \strong{49}, 404-437.
#' @param inf_mat Influence matrix
#' @param new_configuration New sequence of configuration
#' @examples
#' inf_mat <- matrix(c(1,1,1,0,0,1,1,1,1,0,1,1,1,1,0,1),4)
#' rownames(inf_mat) <- colnames(inf_mat) <- c("a","b","c","d")
#' new_configuration <- c("b","a","d","c")
#' new_inf_mat <- redesign_influence_matrix(inf_mat,new_configuration)
redesign_influence_matrix <- function(inf_mat,new_configuration) {
    inf_mat_out <- inf_mat[new_configuration,new_configuration]
    rownames(inf_mat_out) <- colnames(inf_mat_out) <- new_configuration
    inf_mat_out
}
# Influence Matrix to Contribution Matrix ---------------------------------
#' Creating a Contribution Matrix from an Influence Matrix
#'
#' Getting PI for generate an NK Landscape
#'
#' @param inf_mat influence matrix (N by N)
#'
#' @return contribution matrix PI
#'
#' @examples
#' inf_mat <- matrix(c(1,1,1,0,0,1,1,1,1,0,1,1,1,1,0,1),4)
#' cont_mat <- convert_influence_contribution(inf_mat)
#' landscape_fun_N4_K2 = landscape_gen(N=4,K=2,PI=cont_mat)
#' landscape_fun_N4_K2(c(0,1,1,0))
convert_influence_contribution <- function(inf_mat) {
  if(!is.matrix(inf_mat)) return(NA)
  if(dim(inf_mat)[1] != dim(inf_mat)[2]) return(NA)
  inf_mat0 <- inf_mat
  diag(inf_mat0) <- 0
  inf_mat0 <- t(inf_mat0)
  cont_mat = foreach(i = inf_mat0,.combine=rbind) %do% {
    which(i==1)
  }
  rownames(cont_mat) <- colnames(cont_mat) <- NULL
  return(cont_mat)
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
#' Creating an NK Landscape
#'
#' Creating all fitness values based on a given generation function
#'
#' @param N N
#' @param K K
#' @param fuc an object of landscape_gen or landscape_gen_lowdim
#'
#' @return
#' a list of fitness values and others:
#' \enumerate{
#' \item $N
#' \item $K
#' \item $loc_ids
#' \item $loc_bit_ids
#' \item $landscape
#' }
create_landscape <- function(N,K,fuc) {
  loc_ids = seq(0,2^N-1)
  loc_bit_ids = foreach(id=loc_ids,.combine=rbind) %do% int2bit(id,N)
  rownames(loc_bit_ids) = loc_ids
  contributions.t = t(as.matrix(loc_bit_ids))
  fitness_values = foreach(id=contributions.t,.combine=c) %do% fun(id)
  nk_landscape = data.frame(id_bit=loc_bit_ids,fitness=fitness_values)
  rv = list()
  rv$N = N
  rv$K = K
  rv$loc_ids = loc_ids
  rv$loc_bit_ids = loc_bit_ids
  rv$landscape = nk_landscape
  rv
}
#' @title
#' Creating Location IDs and Bitwise Representation
#'
#' @description
#' Creating location IDs and bitwise representation
#'
#' @param N
#' @return data.frame object of location ids and bitwise representation
#'
create_locations <- function(N) {
  loc_ids = seq(0,2^N-1)
  loc_bit_ids = foreach(id=loc_ids,.combine=rbind) %do% int2bit(id,N)
  rownames(loc_bit_ids) = loc_ids
  locations = data.frame(id_bit=loc_bit_ids)
  locations
}
