get_configuration <- function(location_id,N,element_id) {
  bit_id = as.logical(int2bit(location_id,N))
  bit_id[element_id] = !bit_id[element_id]
  bit_id_out = as.integer(bit_id)
  sum(foreach(idx=0:(N-1),i=bit_id_out,.combine=c) %do% {i*2^idx})
}
# Get All Neighbors -------------------------------------------------------
get_all_configuration <- function(location_id,N) {
  an = foreach(element_id=1:N,.combine=c) %do% {
    get_configuration(location_id,N,element_id)
  }
  return(an)
}
