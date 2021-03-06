#' Define MDP model

#' @importFrom truncnorm dtruncnorm

continuous_model <- function(states, actions, params, transition_fn, utility_fn){
  #' @param transition_fn a function(state, action, params) giving the transition probability to the next state
  #' @param utility_fn a function(state, action, params) giving the value of the next state
  #' @param states a vector of the possible states
  #' @param actions a vector of the possible actions
  #' @param params must be a list with an element named sigma

  transition_matrix <- function(states, actions, f, params){
    n_s <- length(states)
    n_a <- length(actions)
    transition <- array(0, dim = c(n_s, n_s, n_a))
    for(i in 1:n_a){
      for (k in 1:n_s) {
        nextpop <- transition_fn(states[k], actions[i], params)
        if (nextpop <= 0) {
          x  <- c(1, rep(0, n_s - 1))
        } else {
          x <- truncnorm::dtruncnorm(states, 0, max(states), nextpop, params$sigma) # assumes truncated normal error
          if(sum(x) <= 0){
            x  <- c(1, rep(0, n_s - 1))
          } else {
            x <- x / sum(x)
          }
        }
        transition[k, , i] <- x
      }
    }
    if(any(is.na(transition))) stop("error creating transition matrix")
    transition
  }

  utility_matrix <- function(states, actions, utility_fn, params){
    utility <- array(dim=c(length(states), length(actions)))
    for(i in 1:length(states)){
      for(j in 1:length(actions)){
        utility[i,j] <- utility_fn(states[i], actions[j], params)
      }
    }
    utility
  }

  list(P = transition_matrix(states, actions, f, params),
       U = utility_matrix(states, actions, utility_fn, params))
}



#' Simulate MDP under a given policy

sim_mdp <- function(P, U, policy, discount, x0, Tmax){

  n_states <- dim(P)[1]
  state <- action <- value <- numeric(Tmax+1)
  state[1] <- x0
  tsteps <- 1:(Tmax+1)

  for(t in tsteps){
    ## Select action, determine value, transition to next state
    action[t] <- policy[state[t]]
    value[t] <- U[state[t], action[t]] * discount^(t-1)
    state[t+1] <- sample(1:n_states, 1, prob = P[state[t], , action[t]])
  }
  data.frame(time = 0:Tmax, state = state[tsteps], action = action[tsteps], value = value[tsteps])
}