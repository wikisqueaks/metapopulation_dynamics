# create landscape
library(tidyverse)
library(patchwork)

create_landscape <- function(x = 5, y = 5, p_occupied = 0.5) {
  return(
    tibble(crossing(x = seq(1 : x), y = seq(1 : y)),
                      t0 = ifelse(runif(x * y, 0, 1) < p_occupied, 1, 0))
    )
}

draw_landscape <- function(landscape) {
  landscape |>
    ggplot(aes(x=x, y=y, fill=as.factor(t0))) +
    geom_tile(alpha = 0.7) +
    scale_fill_manual(values = c("tan","darkgreen"),
                      labels = c("absent", "present")) +
    theme_classic() +
    labs(y="Y", x="X", fill="") +
    theme(aspect.ratio = 1,
          legend.box = "horizontal",
          legend.position = "bottom",
          legend.margin = margin(t=-70),
          axis.title = element_blank(),
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
  p1 <- draw_landscape(sim)
  p2 <- ggplot(sim_p, aes(x=p)) + 
    geom_histogram(binwidth = scotts_bin, fill="cyan3", color="black", alpha=0.7) +
    theme_classic()
  p3 <- ggplot(sim_p,aes(x=t, y=p)) +
    geom_line(color="blue3", linewidth=0.5) +
    geom_smooth(color="red3", alpha=0.6, se=FALSE, linewidth = 0.5) +
    theme_classic()
  p4 <- ggplot(sim_p, aes(y=p, x="")) +
    geom_boxplot() +
    theme_classic() + 
    labs(x="")
  metamodel <- list(simulation = sim,
                  proportions = sim_p,
                  plot = (p1+p2+p4)/p3
                  )
  return(metamodel)
}

metapop(x=20,
        y=20,
        iterations = 200,
        starting_proportion = 0.5,
        colonization_rate = 0.7,
        extirpation_rate = 0.3)
