# =============================================================================
# plots.R — Fonctions de visualisation ggplot2
# =============================================================================

library(ggplot2)

# Thème personnalisé QuantStyle
theme_mm1 <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 1, color = "#1a1a2e"),
      plot.subtitle    = element_text(color = "#5a6a82", size = base_size - 1),
      axis.title       = element_text(color = "#2d3561", size = base_size - 1),
      axis.text        = element_text(color = "#4a4a6a"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e8edf5", linewidth = 0.5),
      legend.position  = "top",
      legend.text      = element_text(size = base_size - 1),
      plot.background  = element_rect(fill = "white", color = NA),
      strip.text       = element_text(face = "bold", color = "#2d3561")
    )
}

PAL <- c(Simulé = "#2563eb", Théorique = "#f97316")


# ─── 1. Barplot comparatif Simulé vs Théorique ───────────────────────────────
plot_performance_comparison <- function(results) {
  df <- data.frame(
    Indicateur = factor(rep(c("L", "Lq", "Wq", "W"), 2), levels = c("L", "Lq", "Wq", "W")),
    Valeur = c(results$simulated, results$theoretical),
    Type   = rep(c("Simulé", "Théorique"), each = 4)
  )

  labels_map <- c(
    L  = "L\n(nb moyen\nsystème)",
    Lq = "Lq\n(nb moyen\nfile)",
    Wq = "Wq\n(attente\nmoyenne)",
    W  = "W\n(séjour\nmoyen)"
  )

  ggplot(df, aes(x = Indicateur, y = Valeur, fill = Type)) +
    geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.9) +
    geom_text(aes(label = round(Valeur, 3)),
              position = position_dodge(0.75),
              vjust = -0.4, size = 3.2, fontface = "bold") +
    scale_fill_manual(values = PAL) +
    scale_x_discrete(labels = labels_map) +
    labs(
      title    = "Comparaison des indicateurs de performance",
      subtitle = paste0("ρ = ", round(results$meta$rho, 3),
                        " | λ = ", results$meta$lambda,
                        " | μ = ", results$meta$mu),
      x = NULL, y = "Valeur", fill = NULL
    ) +
    theme_mm1()
}


# ─── 2. Histogramme des temps d'attente + courbe théorique ───────────────────
plot_wait_histogram <- function(results) {
  wt <- results$stats$wait_times
  if (length(wt) == 0) return(ggplot() + labs(title = "Aucune donnée disponible") + theme_mm1())

  df  <- data.frame(wait_time = wt)
  mu  <- results$meta$mu
  lam <- results$meta$lambda
  rate_theo <- mu - lam   # taux de la loi exp des temps d'attente M/M/1

  ggplot(df, aes(x = wait_time)) +
    geom_histogram(aes(y = after_stat(density)),
                   fill = "#2563eb", color = "white",
                   bins = 40, alpha = 0.75) +
    stat_function(fun = dexp, args = list(rate = rate_theo),
                  color = "#f97316", linewidth = 1.3, linetype = "solid") +
    annotate("text", x = max(wt) * 0.6, y = rate_theo * 0.9,
             label = paste0("Loi Exp(", round(rate_theo, 3), ")"),
             color = "#f97316", size = 3.5, fontface = "italic") +
    labs(
      title    = "Distribution des temps d'attente",
      subtitle = paste0(length(wt), " observations après warm-up"),
      x = "Temps d'attente", y = "Densité"
    ) +
    theme_mm1()
}


# ─── 3. Évolution temporelle de la file d'attente ────────────────────────────
plot_queue_evolution <- function(results, n_points = 2000) {
  ev <- results$events
  if (is.null(ev) || nrow(ev) == 0)
    return(ggplot() + labs(title = "Historique non disponible") + theme_mm1())

  # Sous-échantillonnage
  if (nrow(ev) > n_points) ev <- ev[round(seq(1, nrow(ev), length.out = n_points)), ]

  # Lissage
  k <- min(15, floor(nrow(ev) / 5))
  if (k >= 3) {
    ev$queue_smooth <- as.numeric(stats::filter(ev$queue_length, rep(1/k, k)))
  } else {
    ev$queue_smooth <- ev$queue_length
  }
  ev <- ev[!is.na(ev$queue_smooth), ]

  Lq_theo <- results$theoretical["Lq"]

  ggplot(ev, aes(x = time)) +
    geom_point(aes(y = queue_length), size = 0.4, alpha = 0.2, color = "#93c5fd") +
    geom_line(aes(y = queue_smooth), color = "#2563eb", linewidth = 0.9) +
    geom_hline(yintercept = Lq_theo, linetype = "dashed",
               color = "#f97316", linewidth = 1.1) +
    annotate("label", x = max(ev$time) * 0.85, y = Lq_theo + 0.3,
             label = paste0("Lq théorique = ", round(Lq_theo, 2)),
             color = "#f97316", fill = "#fff7ed",
             size = 3.5, fontface = "bold") +
    labs(
      title    = "Évolution temporelle de la file d'attente",
      subtitle = paste0("Lissage sur ", k, " événements"),
      x = "Temps (t)", y = "Longueur de la file Nq(t)"
    ) +
    theme_mm1()
}


# ─── 4. Densité des temps d'attente ET de séjour ─────────────────────────────
plot_distributions <- function(results) {
  wt <- results$stats$wait_times
  st <- results$stats$system_times
  if (length(wt) == 0) return(ggplot() + labs(title = "Aucune donnée") + theme_mm1())

  df <- data.frame(
    Temps = c(wt, st),
    Type  = rep(c("Attente (Wq)", "Séjour (W)"), c(length(wt), length(st)))
  )

  ggplot(df, aes(x = Temps, fill = Type, color = Type)) +
    geom_density(alpha = 0.4, linewidth = 0.9) +
    facet_wrap(~Type, scales = "free") +
    scale_fill_manual(values  = c("Attente (Wq)" = "#2563eb", "Séjour (W)" = "#f97316")) +
    scale_color_manual(values = c("Attente (Wq)" = "#1d4ed8", "Séjour (W)" = "#ea580c")) +
    labs(
      title    = "Distributions des temps d'attente et de séjour",
      subtitle = "Estimations par noyau (KDE)",
      x = "Temps", y = "Densité"
    ) +
    theme_mm1() +
    theme(legend.position = "none")
}


# ─── 5. QQ-plot exponentiel ──────────────────────────────────────────────────
plot_qqplot <- function(results) {
  wt <- results$stats$wait_times
  if (length(wt) < 10) return(ggplot() + labs(title = "Pas assez de données") + theme_mm1())

  rate_theo <- results$meta$mu - results$meta$lambda
  theo_q    <- qexp(ppoints(length(wt)), rate = rate_theo)
  samp_q    <- sort(wt)

  qq_df <- data.frame(Theoretical = theo_q, Sample = samp_q)

  ggplot(qq_df, aes(x = Theoretical, y = Sample)) +
    geom_point(color = "#2563eb", size = 1.8, alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "#f97316",
                linetype = "dashed", linewidth = 1.2) +
    labs(
      title    = "QQ-plot — Test de la loi exponentielle",
      subtitle = paste0("Taux théorique : μ−λ = ", round(rate_theo, 3)),
      x = "Quantiles théoriques Exp(μ−λ)",
      y = "Quantiles observés"
    ) +
    theme_mm1()
}


# ─── 6. Analyse de sensibilité (courbes L, Lq, W, Wq vs ρ) ──────────────────
plot_sensitivity <- function(sens_df) {
  library(tidyr)
  df_long <- tidyr::pivot_longer(sens_df, cols = c(L, Lq, W, Wq),
                                  names_to = "KPI", values_to = "Valeur")

  ggplot(df_long, aes(x = rho, y = Valeur, color = KPI)) +
    geom_line(linewidth = 1.2) +
    geom_vline(xintercept = 0.8, linetype = "dotted", color = "gray50") +
    annotate("text", x = 0.82, y = max(df_long$Valeur, na.rm = TRUE) * 0.9,
             label = "ρ = 0.8", color = "gray40", size = 3.2) +
    scale_color_manual(values = c(L = "#2563eb", Lq = "#7c3aed",
                                   W = "#f97316", Wq = "#16a34a")) +
    labs(
      title    = "Analyse de sensibilité — KPIs vs intensité de trafic ρ",
      subtitle = "Formules analytiques M/M/1 (μ = 1)",
      x = "Intensité de trafic ρ = λ/μ",
      y = "Valeur du KPI",
      color = "Indicateur"
    ) +
    coord_cartesian(ylim = c(0, quantile(df_long$Valeur, 0.97, na.rm = TRUE))) +
    theme_mm1()
}


# ─── 7. Fan chart Monte Carlo (multi-trajectoires Lq) ────────────────────────
plot_mc_fan <- function(lambda, mu, T_max, n_replications = 30, warm_up = 0) {
  results_list <- lapply(seq_len(n_replications), function(i) {
    res <- simulate_mm1(lambda, mu, T_max, warm_up = warm_up, seed = i,
                        record_history = FALSE)
    data.frame(rep = i, L = res$simulated["L"], Lq = res$simulated["Lq"],
               W = res$simulated["W"], Wq = res$simulated["Wq"])
  })
  df <- do.call(rbind, results_list)

  rho    <- lambda / mu
  Lq_theo <- if (rho < 1) rho^2 / (1 - rho) else NA

  ggplot(df, aes(x = rep)) +
    geom_point(aes(y = Lq), color = "#2563eb", size = 2, alpha = 0.8) +
    geom_hline(yintercept = mean(df$Lq, na.rm = TRUE),
               color = "#2563eb", linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = Lq_theo,
               color = "#f97316", linetype = "solid", linewidth = 1.2) +
    annotate("label", x = n_replications * 0.85, y = Lq_theo,
             label = paste0("Lq théo. = ", round(Lq_theo, 3)),
             color = "#f97316", fill = "#fff7ed", size = 3.3, fontface = "bold") +
    labs(
      title    = paste0("Variabilité de Lq sur ", n_replications, " réplications"),
      subtitle = "Convergence vers la valeur théorique M/M/1",
      x = "Numéro de réplication",
      y = "Lq simulé"
    ) +
    theme_mm1()
}
