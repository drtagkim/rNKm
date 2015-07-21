#' Hill Climbing Search (Full Coverage)
#'
#' Executing hill climbing search
#'
#' @param agent_size Number of agents
#' @param loc_ids Location ids (numeric vector)
#' @param nk_landscape fitness (matrix)
#' @param noise Noise coefficient (0=none to 1=full)
#' @param noise_vector Noise structure (default, following a uniform distribution)
#'
#' @examples
#' nk.data = create_nkm_simple(6,3)
#' agent_size = 10
#' result = search_hill_climbing_full(agent_size,nk.data$loc_ids,nk.data$nk_landscape)
#'
#' @seealso
#' \code{\link{calculate_value_on_location}}
#'
search_hill_climbing_full <- function(agent_size,loc_ids,nk_landscape,noise=0,noise_vector=NULL) {
  cat("========================","\n",sep="")
  cat("    Hill Climbing","\n",sep="")
  cat("========================","\n\nProgress...\n\n",sep="")
  fitness_col_id = ncol(nk_landscape)
  N = fitness_col_id - 1
  search_record = foreach(pos=1:agent_size) %do% {
    agent_loc = sample(loc_ids,1)
    fnow = nk_landscape[agent_loc+1,fitness_col_id]
    data.frame(agent_id=pos,from=-1,to=agent_loc,fitness_value=fnow,stabilized=FALSE)
  }#foreach
  pb <- txtProgressBar(style=3)
  considered_agents = 1:agent_size
  while(TRUE) {
    progress_i = 1 - (length(considered_agents) / agent_size)
    setTxtProgressBar(pb,progress_i)
    if(length(considered_agents)==0) break
    considered_agents = sample(considered_agents)
    considered_agents0 = foreach(agent_id = considered_agents,.combine=c) %do% {
      agent_data = search_record[[agent_id]]
      current_fit = as.numeric(unclass(agent_data[nrow(agent_data),"fitness_value"]))
      current_loc_id = as.numeric(unclass(agent_data[nrow(agent_data),"to"]))
      nk.result = nk_landscape[get_all_configuration(current_loc_id,N)+1,] #In R, ID starts with 1
      new_turn = calculate_value_on_location(current_fit,nk.result,nk_landscape,noise=noise,noise_vector=noise_vector)
      new_pos = new_turn$new_pos
      stabilized = new_turn$stabilized
      alternative_fit = new_turn$new_fit
      search_record[[agent_id]] = bind_rows(search_record[[agent_id]],
                                  data.frame(agent_id=agent_id,
                                             from=current_loc_id,
                                             to=new_pos,
                                             fitness_value=alternative_fit,
                                             stabilized=stabilized))
      rv = ifelse(!stabilized,agent_id,-1)
      rv
    }#foreach
    considered_agents = considered_agents0[considered_agents0 != -1]
  }#while
  Sys.sleep(0.5)
  close(pb)
  cat("\n         COMPLETE","\n",sep="")
  search_record
}#function
