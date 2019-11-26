#' @importFrom truncnorm dtruncnorm

continuous_model <- function(states, actions, params,
                             f = function(s, a, params) s + params$r * (a - s),
                             utility_fn = function(s,a, params) params$benefit * s - params$cost * a
                             ){

  #' @param f a function(state,action, params) giving the expected value of the next state
  #' @param states a vector of the possible states
  #' @param actions a vector of the possible actions
  #' @param params must be a list with an element named sigma
  # assumes truncated normal error
  transition_matrix <- function(states, actions, f, params){
    n_s <- length(states)
    n_a <- length(actions)
    transition <- array(0, dim = c(n_s, n_s, n_a))
    for(i in 1:n_a){
      for (k in 1:n_s) {
        nextpop <- f(states[k], actions[i], params)
        if (nextpop <= 0) {
          x  <- c(1, rep(0, n_s - 1))
        } else {
          x <- truncnorm::dtruncnorm(states, 0, max(states), nextpop, params$sigma)
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
       U = utility_matrix( states, actions, utility_fn, params))
}


