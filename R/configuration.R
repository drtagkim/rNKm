#' Getting a New Location ID by Configuring a given Component
#'
#' Getting a New Location ID by Configuring a given Component
#'
#' @param location_id Agent's location
#' @param N Number of components
#' @param element_id Component ID (from 1 to N)
#'
#' @return Location ID
#' @seealso \code{\link{get_all_configuration}}
#'
get_configuration <- function(location_id,N,element_id) {
  bit_id = as.logical(int2bit(location_id,N))
  bit_id[element_id] = !bit_id[element_id]
  bit_id_out = as.integer(bit_id)
  sum(foreach(idx=0:(N-1),i=bit_id_out,.combine=c) %do% {i*2^idx})
}
#' Looking around All Possibile Configuration Changes
#'
#' Looking around all the possible configuration changes
#'
#' @param location_id Agent's location
#' @param N Number of components
#'
#' @return Location IDs for all the possible configurations
#'
get_all_configuration <- function(location_id,N,coverage=1:N) {
  an = foreach(element_id=coverage,.combine=c) %do% {
    get_configuration(location_id,N,element_id)
  }
  return(an)
}
