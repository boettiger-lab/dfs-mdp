
single_step_model <- function(states, actions, params){
  #' Transisition function
  #'
  #' Return the probability of being in each possible future state,
  #' given a you are in the current state `state` and take action `action`.
  #' Loop over this function with all possible states and actions to create
  #' the transistion matrix.
  #'
  #' @param state an index (`1:N_s` where `N_s` is length of `states`) to the
  #'  current state, states[state]
  #' @param action an index to the action (`1:N_a`, where `N_a` is the length of `actions`)
  #' @param states a vector of the possible states
  #' @param actions a vector of the possible actions
  #' @param params a list of parameters
  transition_fn <- function(state, action,
                            states = seq(0,1, length.out = 11),
                            actions = seq(0,1,length.out = 11),
                            params = list(stability = .5)){

    ## Initialize the output vector -- will give probability associated with each state
    prob_next_state <- numeric(length(states))

    ## contrast between state and action: \in [-1, +1]
    contrast <- (actions[action] / max(actions) - states[state] / max(states))

    ## assumes ES & DP on same scale
    prob_improve <- 1/2 + contrast / 2
    sum <- prob_improve + (1 - prob_improve) + params$stability

    prob_decline <- (1 - prob_improve)/sum
    prob_remain <- params$stability/sum
    prob_improve <- (prob_improve)/sum

    prob_next_state[state] <- prob_remain
    if(state == length(states)) # Upper boundary
      prob_next_state[state] <- prob_next_state[state] + prob_improve
    else
      prob_next_state[state + 1] <- prob_improve

    if(state == 1) # Lower boundary
      prob_next_state[state] <- prob_decline + prob_next_state[state]
    else
      prob_next_state[state - 1] <- prob_decline

    ## Normalize
    prob_next_state <- prob_next_state/ sum(prob_next_state)
    prob_next_state
  }

  utility <- function(state, action,
                      params){
    params$benefit * state -  params$cost * action
  }


  ## Matrix-ify these functions. (yeah this is just outer(states, actions, utility, params))
  utility_matrix <- function(states,
                             actions,
                             params){
    n_s <- length(states)
    n_a <- length(actions)
    U <- array(dim=c(n_s,n_a))
    for(i in 1:n_s){
      for(k in 1:n_a){
        U[i,k] <- utility(states[i], actions[k], params)
      }
    }
    U
  }

  transition_matrix <- function(states, actions, params){
    P <- array(dim=c(length(states),length(states),length(actions)))
    for(i in 1:length(states)){
      for(k in 1:length(actions)){
        P[i,,k] <- transition_fn(i, k, states, actions, params)
      }
    }
    P
  }

  ## Now we can use these functions to generate the matrices we need for an MDP model:
  list(P = transition_matrix(states,actions, params),
       U = utility_matrix(states, actions, params))
}


