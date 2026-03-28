# =============================================================================
# simulation.R — Moteur de simulation M/M/1
# Auteur  : Mohamed El Otmany
# Projet  : Modélisation de file d'attente M/M/1 — Processus de Poisson
# =============================================================================

library(R6)

# ─── Classe Statistics ────────────────────────────────────────────────────────
#' Collecte les statistiques au fil de la simulation par event-driven approach.
Statistics <- R6::R6Class(
  "Statistics",
  public = list(
    area_number  = 0.0,   # Intégrale de N(t) dt  → donne L
    area_queue   = 0.0,   # Intégrale de Nq(t) dt → donne Lq
    last_time    = 0.0,
    wait_times   = numeric(),
    system_times = numeric(),
    events_history = list(),

    initialize = function() {
      self$area_number   <- 0.0
      self$area_queue    <- 0.0
      self$last_time     <- 0.0
      self$wait_times    <- numeric()
      self$system_times  <- numeric()
      self$events_history <- list()
    },

    #' Met à jour les aires sous les courbes N(t) et Nq(t)
    update_areas = function(current_time, queue_length, server_busy) {
      dt <- current_time - self$last_time
      self$area_number <- self$area_number + dt * (queue_length + as.numeric(server_busy))
      self$area_queue  <- self$area_queue  + dt * queue_length
      self$last_time   <- current_time
    },

    #' Enregistre un événement dans l'historique
    add_event = function(time, event_type, queue_length, server_busy) {
      self$events_history <- c(
        self$events_history,
        list(data.frame(
          time         = time,
          event        = event_type,
          queue_length = queue_length,
          server_busy  = as.integer(server_busy)
        ))
      )
    }
  )
)


# ─── Générateur exponentiel ───────────────────────────────────────────────────
#' Génère une v.a. Exp(rate) par inversion de la CDF.
#' X = -ln(U)/rate, U ~ Unif(0,1)
exponential_rv <- function(rate) {
  stopifnot(rate > 0)
  -log(runif(1)) / rate
}


# ─── Simulation principale M/M/1 ─────────────────────────────────────────────
#'
#' @param lambda  Taux d'arrivée (clients/unité de temps)
#' @param mu      Taux de service (clients/unité de temps)
#' @param T_max   Horizon de simulation
#' @param warm_up Période de chauffe (exclue des statistiques)
#' @param seed    Graine pour la reproductibilité
#' @param record_history Booléen — enregistrer chaque événement
#'
#' @return Liste : simulated, theoretical, events, stats, meta
simulate_mm1 <- function(lambda, mu, T_max,
                          warm_up       = 0.0,
                          seed          = NULL,
                          record_history = TRUE) {

  # Validation
  stopifnot(lambda > 0, mu > 0, T_max > 0, warm_up >= 0, warm_up < T_max)
  if (!is.null(seed)) set.seed(seed)

  # Initialisation
  stats        <- Statistics$new()
  queue        <- numeric()   # File FIFO: stocke les temps d'arrivée
  server_busy  <- FALSE
  current_time <- 0.0

  next_arrival   <- exponential_rv(lambda)
  next_departure <- Inf

  event_count            <- 0L
  arrivals_after_warmup  <- 0L

  # ── Boucle événementielle ──────────────────────────────────────────────────
  while (current_time < T_max) {

    if (next_arrival <= next_departure) {
      # ── ARRIVÉE ──
      event_time   <- next_arrival
      stats$update_areas(event_time, length(queue), server_busy)
      if (record_history) stats$add_event(event_time, "Arrivée", length(queue), server_busy)
      current_time <- event_time
      next_arrival <- current_time + exponential_rv(lambda)

      if (!server_busy) {
        server_busy   <- TRUE
        service_time  <- exponential_rv(mu)
        next_departure <- current_time + service_time
        if (current_time >= warm_up) {
          stats$wait_times   <- c(stats$wait_times, 0.0)
          stats$system_times <- c(stats$system_times, service_time)
          arrivals_after_warmup <- arrivals_after_warmup + 1L
        }
      } else {
        queue <- c(queue, current_time)   # Mise en file
      }

    } else {
      # ── DÉPART ──
      event_time   <- next_departure
      stats$update_areas(event_time, length(queue), server_busy)
      if (record_history) stats$add_event(event_time, "Départ", length(queue), server_busy)
      current_time <- event_time

      if (length(queue) > 0) {
        arrival_time   <- queue[1]
        queue          <- queue[-1]
        wait_time      <- current_time - arrival_time
        service_time   <- exponential_rv(mu)
        next_departure <- current_time + service_time

        if (arrival_time >= warm_up) {
          stats$wait_times   <- c(stats$wait_times, wait_time)
          stats$system_times <- c(stats$system_times, wait_time + service_time)
          arrivals_after_warmup <- arrivals_after_warmup + 1L
        }
      } else {
        server_busy    <- FALSE
        next_departure <- Inf
      }
    }

    event_count <- event_count + 1L
  }

  # ── Indicateurs de performance simulés ────────────────────────────────────
  L_hat  <- stats$area_number / T_max
  Lq_hat <- stats$area_queue  / T_max
  Wq_hat <- if (length(stats$wait_times)   > 0) mean(stats$wait_times)   else 0
  W_hat  <- if (length(stats$system_times) > 0) mean(stats$system_times) else 0

  # ── Formules analytiques M/M/1 ────────────────────────────────────────────
  rho <- lambda / mu
  if (rho >= 1) {
    L_theo <- Lq_theo <- W_theo <- Wq_theo <- Inf
  } else {
    L_theo  <- rho / (1 - rho)
    Lq_theo <- rho^2 / (1 - rho)
    W_theo  <- 1 / (mu - lambda)
    Wq_theo <- lambda / (mu * (mu - lambda))
  }

  # ── Historique événements ──────────────────────────────────────────────────
  events_df <- if (record_history && length(stats$events_history) > 0)
    do.call(rbind, stats$events_history)
  else NULL

  list(
    simulated = c(L = L_hat, Lq = Lq_hat, Wq = Wq_hat, W = W_hat),
    theoretical = c(L = L_theo, Lq = Lq_theo, Wq = Wq_theo, W = W_theo),
    events  = events_df,
    stats   = stats,
    meta    = list(
      rho                   = rho,
      lambda                = lambda,
      mu                    = mu,
      T_max                 = T_max,
      warm_up               = warm_up,
      seed                  = seed,
      event_count           = event_count,
      arrivals_after_warmup = arrivals_after_warmup,
      stable                = rho < 1
    )
  )
}


# ─── Analyse de sensibilité ───────────────────────────────────────────────────
#' Calcule les KPIs théoriques pour une grille de rho = lambda/mu
#'
#' @param rho_grid Vecteur de valeurs rho dans ]0,1[
sensitivity_analysis <- function(rho_grid = seq(0.05, 0.95, by = 0.05), mu = 1.0) {
  rho_grid <- rho_grid[rho_grid < 1 & rho_grid > 0]
  data.frame(
    rho    = rho_grid,
    lambda = rho_grid * mu,
    L      = rho_grid / (1 - rho_grid),
    Lq     = rho_grid^2 / (1 - rho_grid),
    W      = 1 / (mu * (1 - rho_grid)),
    Wq     = rho_grid / (mu * (1 - rho_grid))
  )
}
