# create landscape
library(tidyverse)
library(patchwork)

create_landscape <- function(x = 5, y = 5, p_occupied = 0.5) {
  return(
    tibble(crossing(x = seq(1 : x), y = seq(1 : y)),
                      t0 = ifelse(runif(x * y, 0, 1) < p_occupied, 1, 0))
    )
}

draw_landscape <- function(landscape, col_name) {
  landscape |>
    ggplot(aes(x=x, y=y, fill=as.factor(!!sym(col_name)))) +
    geom_tile(alpha = 0.7) +
    scale_fill_manual(values = c("tan","darkgreen"),
                      labels = c("absent", "present")) +
    theme_classic() +
    labs(fill="") +
    theme(aspect.ratio = 1,
          legend.position = "none",
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          )
}

simulate <- function(landscape, 
                     iterations = 50, 
                     colonization_rate = 0.5, 
                     extirpation_rate = 0.5) {
  sim <- landscape
  for (i in 1:iterations) {
    next_t <- sim |>
      select(last_col()) |>
      mutate(!!paste0("t",i) := ifelse(sim[[ncol(sim)]] == 0,
                                 ifelse(runif(length(sim[[ncol(sim)]]), 0, 1) < colonization_rate, 1, 0),
                                 ifelse(runif(length(sim[[ncol(sim)]]), 0, 1) < extirpation_rate, 0, 1))) |>
      select(last_col())
    sim <- add_column(sim, next_t)
  }
  return(sim)
}

create_proportions <- function(simulation) {
  patches <- simulation |>
    select(contains("t"))
  return(tibble(t = 0:(ncol(simulation)-3),
                p = colMeans(patches)))
}

# Take input parameters and produce a matrix-cell plot and histogram:

metapop <- function(x=5, 
                    y=5, 
                    starting_proportion=0.5,
                    colonization_rate=0.5, 
                    extirpation_rate=0.5,
                    iterations = 50) {
  land0 <- create_landscape(x=x,y=y,p_occupied = starting_proportion)
  sim <- simulate(landscape = land0, 
                  iterations = iterations, 
                  colonization_rate = colonization_rate,
                  extirpation_rate = extirpation_rate) 
  sim_p <- create_proportions(sim)
  p_vec <- sim_p$p
  sd_sim <- sd(p_vec)
  n_sim <- length(p_vec)
  scotts_bin <- 3.5*sd_sim/(n_sim)^(1/3)
  last_sim <- names(sim |> select(last_col()))
  p1 <- draw_landscape(sim,"t0") + labs(x="Initial Landscape")
  p2 <- ggplot(sim_p, aes(x=p)) + 
    geom_histogram(binwidth = scotts_bin, fill="cyan3", color="black", alpha=0.7) +
    theme_classic() +
    labs(x="Proportion Occupied")
  p3 <- ggplot(sim_p,aes(x=t, y=p)) +
    geom_line(color="blue3", linewidth=0.5) +
    geom_smooth(color="red3", alpha=0.6, se=FALSE, linewidth = 0.5, method = "loess", formula = y~x) +
    theme_classic() +
    labs(x="Time Periods", y="Proportion Occupied")
  p4 <- draw_landscape(sim, last_sim) + labs(x="Final Landscape")
    
  metamodel <- list(simulation = sim,
                  proportions = sim_p,
                  plot = (p1+p2+p4)/p3 + plot_layout(guides = "collect")
                  )
  return(metamodel)
}

metapop(x=20,
        y=20,
        iterations = 200,
        starting_proportion = 0.5,
        colonization_rate = 0.7,
        extirpation_rate = 0.3)

sim <- simulate(create_landscape())

