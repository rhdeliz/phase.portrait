#' @name lotka_volterra
#' @title Lotka-Volterra Model Simulation
#' This function generates an example dataset for this package, simulating several trajectories using predator-prey equations.
#' @description This function simulates the Lotka-Volterra predator-prey model over a specified time range
#' and initial conditions for the populations of prey and predator species. It generates a table of
#' population dynamics for various starting conditions and parameter values.
#' @param alpha Numeric. The growth rate of the prey population. Default is 1.1.
#' @param beta Numeric. The rate of predation, describing how frequently predators consume prey. Default is 0.4.
#' @param gamma Numeric. The mortality rate of the predator population. Default is 0.1.
#' @param delta Numeric. The reproduction rate of predators per prey consumed. Default is 0.01.
#' @param min_x Numeric. Minimum initial population of prey. Default is 10.
#' @param max_x Numeric. Maximum initial population of prey. Default is 50.
#' @param bin_width_x Numeric. Interval width for the prey population bins. Default is 10.
#' @param min_y Numeric. Minimum initial population of predators. Default is 5.
#' @param max_y Numeric. Maximum initial population of predators. Default is 25.
#' @param bin_width_y Numeric. Interval width for the predator population bins. Default is 5.
#' @param min_time Numeric. Starting time for the simulation. Default is 0.
#' @param max_time Numeric. Ending time for the simulation. Default is 50.
#' @param time_interval Numeric. Time increment for each simulation step. Default is 0.1.
#'
#' @return A data frame containing the time series of prey and predator populations for each initial condition
#' and parameter set. The output includes columns for time, prey and predator populations, and initial conditions.
#'
#' @details The Lotka-Volterra model describes the dynamics of biological systems in which two species interact,
#' one as a predator and the other as prey. This function allows for exploring various initial conditions
#' and parameter values to study the system's behavior under different scenarios.
#'
#' @examples
#' # Run the Lotka-Volterra simulation with default parameters
#' results <- lotka_volterra()
#'
#' # Run with custom parameters
#' results <- lotka_volterra(alpha = 1.2, beta = 0.5, gamma = 0.15, delta = 0.02)
#'
#' @import deSolve
#' @import dplyr
#' @importFrom data.table setDT
#' @importFrom tidyr pivot_longer
#' @export
lotka_volterra <- function(alpha = 0.5, beta = 0.8, gamma = 1, delta = 0.8,
                           min_x = 1, max_x = 10, bin_width_x = 1,
                           min_y = 1, max_y = 5, bin_width_y = 1,
                           min_time = 0, max_time = 25, time_interval = 0.1) {

  # Define the Lotka-Volterra model as a function
  equation <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      # Predator-prey model equations
      dPrey <- alpha * Prey - beta * Prey * Predator
      dPredator <- delta * Prey * Predator - gamma * Predator
      list(c(dPrey, dPredator))
    })
  }

  # Parameters for the model
  parameters <- c(alpha = alpha,  # Prey growth rate
                  beta = beta,    # Predation rate
                  gamma = gamma,  # Predator death rate
                  delta = delta)  # Predator reproduction rate

  # Initial conditions for the state variables (Prey and Predator populations)
  initial_states <- expand.grid(Prey = seq(min_x, max_x, by = bin_width_x),
                                Predator = seq(min_y, max_y, by = bin_width_y))

  # Time vector
  times <- seq(min_time, max_time, by = time_interval)

  # Function to run simulation for each initial condition
  simulate_trajectories <- function(init_state) {
    # Convert init_state to a named numeric vector
    init_state <- as.numeric(init_state)
    names(init_state) <- c("Prey", "Predator")

    result <- deSolve::ode(y = init_state, times = times, func = equation, parms = parameters)
    result <- as.data.frame(result)
    result$initial_x <- init_state["Prey"]
    result$initial_y <- init_state["Predator"]
    result
  }

  # Run the simulation for each initial state
  trajectory_list <- lapply(1:nrow(initial_states), function(i) {
    simulate_trajectories(initial_states[i, ])
  })

  # Combine the results into a single data frame
  trajectory_table <- do.call(rbind, trajectory_list)

  trajectory_table$condition <- paste0(
    "a=", round(alpha, 3),
    " b=", round(beta, 3),
    " c=", round(gamma, 3),
    " d=", round(delta, 3)
  )

  trajectory_table <-
    trajectory_table %>%
    mutate(
      sample = paste(initial_x, initial_y, sep = "_")
    ) %>%
    rename(
      pseudotime = time
    ) %>%
    tidyr::pivot_longer(
      cols = c(Predator, Prey),
      values_to = "value",
      names_to = "variable"
    ) %>%
    select(-c(initial_x, initial_y))

  trajectory_table <- setDT(trajectory_table)

  return(trajectory_table)
}
