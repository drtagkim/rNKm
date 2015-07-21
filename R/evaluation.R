#' Evaluating Possible Alternatives to Select a New Location ID
#'
#' Evaluating Possible Alternatives to Select a New Location ID
#'
#' @param current_fit Fitness value of current location
#' @param nk.result Alternative information
#' @param nk_landscape
#' @param noise Noise coefficient (0=none to 1=full)
#' @param noise_vector Noise structure (default, following a uniform distribution)
#'
#' @return result list
#' \itemize {
#'  \item new_pos
#'  \item new_fit
#'  \item stabilized
#' }
#'
#' @seealso \code{\link{search_hill_climbing}}
calculate_value_on_location <- function(current_fit,nk.result,nk_landscape,noise=0,noise_vector=NULL) {
  rv = list()
  fitness_col_id = ncol(nk_landscape)
  N = fitness_col_id - 1
  if(is.null(noise_vector)) {
    temp.result.fit = as.numeric(unclass(nk.result[,fitness_col_id]))+(noise*runif(length(nk.result[,1]),min=-1,max=1))
  } else {
    temp.result.fit = as.numeric(unclass(nk.result[,fitness_col_id]))+(noise*noise_vector)
  }
  nk.result[,fitness_col_id] = temp.result.fit
  alternative_fit = max(nk.result[,fitness_col_id])
  if(current_fit < alternative_fit) {
    new_pos=as.integer(rownames(nk.result)[which.max(nk.result[,fitness_col_id])])
  } else {
    new_pos = -1
    alternative_fit=current_fit
  }#if-else
  rv$new_pos=new_pos
  rv$new_fit=alternative_fit
  rv$stabilized=ifelse(new_pos>=0,FALSE,TRUE)
  rv
}
