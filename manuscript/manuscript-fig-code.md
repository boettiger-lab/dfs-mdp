DFS-MDP experiment and figure generation
================
2020-01-15

``` r
actions <- seq(0, 1, length = 100)
# Allow max(state) > max(actions) to avoid creating a hard boundary on ecosystem state
states <- seq(0, 1.5, length = 100)
params <- list(benefit = 1.57, cost = 1, sigma = 0.1, r = .1)
discount <- 0.97
transition_fn <- function(s, a, params) s + params$r * (a - s)
utility_fn <- function(s,a, params) params$benefit * s - params$cost * a
model <- continuous_model(states, actions, params, transition_fn, utility_fn)
```

``` r
soln <- MDPtoolbox::mdp_value_iteration(model$P, model$U, discount)
soln_df <- tibble(state = states,
                  action = actions[soln$policy])
#write_csv(soln_df, "../data/soln_df.csv.gz")
```

``` r
sim_plot <- function(sims, panelA_opts = ggtitle(""), panelB_opts = ggtitle("")){
  
   df <- sims %>% 
    select(-obs,-value) %>% # tidy
    mutate(state = states[state], action = actions[action]) # rescale
  
   panelA <- df %>% 
     ggplot(aes(time, state, group = reps, col = time)) + 
     geom_path(alpha = 0.1, show.legend = FALSE) + 
      panelA_opts +
      labs(x="Time", y="ES State") +
      theme(axis.text.x=element_text(size=12),
            axis.text.y=element_text(size=12),
            axis.title.x=element_text(size=13),
            axis.title.y=element_text(size=13))
   
   Tmax <- max(sims$time)
   panelB <- df %>% filter(time %in% c(1, Tmax))  %>%
      ggplot() + geom_density(aes(state, group = time, fill = time), alpha=0.8) +
      scale_fill_continuous(breaks=c(1,10)) +
      coord_flip() +
      panelB_opts +
      labs(x="", y="Density", fill="Time") +
      theme(axis.text.x=element_text(size=12),
            axis.text.y=element_text(size=12),
            axis.title.x=element_text(size=13),
            axis.title.y=element_text(size=13),
            legend.title = element_text(size=13))
   
   
   panelA + panelB + plot_layout(widths = c(3, 1))
   
   # ggarrange(
   #   panelA, panelB, widths = c(3, 2), align = "v", nrow = 1, ncol = 2, labels = c("A", "B")
   # )
   
}
```

## Base case simulation

``` r
reps <- 500
init <- truncnorm::rtruncnorm(reps, 0, 1, 0.5, 0.2) %>% 
                  map_int(function(x) which.min(abs(x - states)))

sim <- function(soln, 
                Tmax = 10,
                x0 =  init){
  map_dfr(1:length(x0), 
          function(i)
                   mdp_planning(model$P, model$U, discount, 
                   policy = soln$policy,
                   x0 = x0[i], Tmax = Tmax), .id = "reps")
}

sims <- sim(soln)
#write_csv(sims, "../data/sims.csv.gz")
```

## Basic bifurcation result

``` r
sim_plot(sims)
```

![](manuscript-fig-code_files/figure-gfm/fig_bifurcation-1.pdf)<!-- -->

``` r
ggsave("test.pdf", device = cairo_pdf)
```

    ## Saving 7 x 5 in image

``` r
# , fig.cap="Simulations of the ecosystem state of 500 farming plots over time. Plots are initialized with a truncated normal distributed ecosystem state (mean state 0.5, standard deviation 0.2, truncated at 0,1).  Plots are then managed according to the decision rule as discussed in the main text."
```

``` r
soln_df %>%
  ggplot(aes(state,action)) + geom_point() +
  coord_cartesian(xlim = c(0,1))
```

![The emergent decision
rule](manuscript-fig-code_files/figure-gfm/fig_policyfn-1.pdf)

``` r
tenure_2 <- MDPtoolbox::mdp_finite_horizon(model$P, model$U, discount, N = 2)
sims_short_tenure <- sim(tenure_2)
#write_csv(sims_short_tenure, "../data/sims_short_tenure.csv.gz")
```

``` r
sim_plot(sims_short_tenure) 
```

![Under short land tenure (shown at 2 years), farmers on most farm plots
opt to discontinue or not to adopt diversified practices. This results
in a degredation of ecosystem state even among those plots with an
initially high value. All parameters are as in previous figure, but
decision problem is solved under the finite, 2-year horizon during which
farmer will have access to the current plot and thus be able to reap the
benefits of the ecosystem
state.](manuscript-fig-code_files/figure-gfm/fig_tenure-1.pdf)

## Subsidies

``` r
params_subsidy_burst <- list(benefit = 1.57, cost = 0, sigma = 0.1, r = .1)
params_subsidy_sustained <- list(benefit = 1.57, cost = .8, sigma = 0.1, r = .1)

## Note that farmers do not solve the finite-time horizon problem for the short period of the subsidy,
## because they are not "cashing out" (moving off the land) after the subsidy expires.
## This is still not the optimal solution, but is a reasonable approximation.
modelA <- continuous_model(states, actions, params_subsidy_burst, transition_fn, utility_fn)
solnA <- MDPtoolbox::mdp_value_iteration(modelA$P, modelA$U, discount)
  
## Do 2 years with large subsidy (no cost to adoption)
start <- sim(solnA, Tmax = 3, x0 = init)
x1 <- start %>% filter(time == 3) %>% pull(state)
## Go another 18 years with decision rule under no subsidy
rest <- sim(soln, Tmax = 18, x0 = x1) %>% filter(time!=1) %>% mutate(time = time+2)
simA <- bind_rows(start, rest)

## Do 10 years with minor subsidy (subsidize 10% of unit cost over first 10 years)
modelB <- continuous_model(states, actions, params_subsidy_sustained, transition_fn, utility_fn)
solnB <- MDPtoolbox::mdp_value_iteration(modelB$P, modelB$U, discount)
start <- sim(solnB, Tmax=10, x0 = init)  
x10 <- start %>% filter(time == 10) %>% pull(state)
## Go another 10 years with decision rule under no subsidy
rest <- sim(soln, Tmax = 10, x0 = x10) %>% filter(time!=1) %>% mutate(time = time+10)
simB <- bind_rows(start, rest)


#write_csv(simA, "../data/simA.csv.gz")
#write_csv(simB, "../data/simB.csv.gz")
```

``` r
sim_plot(simA, ggtitle("A. Large, short subsidy")) / 
   sim_plot(simB, ggtitle("B. Small, sustained subsidy"))
```

![Panel A: replicate simulations from 500 normally distributed starting
states under a large subsidy (no direct cost to adoption) over two
years. Panel B: the same starting conditions for 500 simulations under a
smaller subsidy (cost = 0.9) for 10 years. Ingoring discounting,
subsidies have the same total cost After subsidy is removed, farmer
adjusts their decision rule to that of no
subsidy.](manuscript-fig-code_files/figure-gfm/fig_subsidy-1.pdf)

## Model mathematical description

Mathematically, the farmer’s decision model can be expressed
as

\[\max_{\lbrace a_t \rbrace} \mathbb{E} \left[ \sum_t^T U(x_t, a_t) \delta^t \right] \]

where \(\lbrace a_t \rbrace\) is the set of available actions to be
taken at each point in time \(t\), \(\delta\) is the discount rate,
\(\mathbb{E}\) the expectation operator, and \(U(x_t, a_t)\) the utility
which the farmer associates with being in state \(x_t\) and taking
action \(a_t\) at time \(t\). \(T\) is the land tenure of the farm
(\(T = \infty\) if the farmer owns the land or otherwise expects to be
able to farm the same land and thus benefit from the ecosystem services
established there indefinitely)

We assume a simple model for the farmer’s utility \(U(x_t, a_t)\) as
combination of the costs associated with adopting the diversified
practice and the benefits derived from the ecosystem state (which is in
turn influenced by the diversified practices or non-diversified
practices adopted),

\[U(x_t, a_t) = b x_t  - c a_t\] Where \(x_t\) is the ecosystem state,
\(b\) the benefit associated, and \(c a_t\) the cost of taking action
\(a_t\). In general, more complicated nonlinear functions of both the
ecosystem state and action are possible in this framework.

The ecosystem state is also dynamic, evolving according to the
transition function \(f(x_t, a_t)\)

\[x_{t+1} = f(x_t, a_t) := x_t + r \left(a_t - x_t \right)\]

This provides a minimal, one-parameter model in which the parameter
\(r\) sets the natural timescale at which the ecosystem can respond to a
change in mangement practice.
