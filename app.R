# =============================================================================
# app.R — Simulation Interactive M/M/1 | Shiny Application
# Auteur  : Mohamed El Otmany
# FST Errachidia — Finance & Sciences Actuarielles
# =============================================================================

# ── Packages ──────────────────────────────────────────────────────────────────
required_pkgs <- c("shiny", "shinydashboard", "ggplot2", "dplyr",
                   "DT", "R6", "tidyr", "shinyjs")

missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(paste0(
    "Packages manquants : ", paste(missing_pkgs, collapse = ", "),
    "\nInstallez-les avec : install.packages(c('",
    paste(missing_pkgs, collapse = "','"), "'))"
  ))
}

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(R6)
library(tidyr)
library(shinyjs)

# ── Modules internes ──────────────────────────────────────────────────────────
source("R/simulation.R")
source("R/plots.R")


# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(
  skin = "blue",

  # ── Header ────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(src = "logo.png", height = "28px", style = "margin-right:8px;"),
      "Simulation M/M/1"
    ),
    titleWidth = 280
  ),

  # ── Sidebar ───────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 280,
    useShinyjs(),

    sidebarMenu(
      id = "tabs",
      menuItem("🏠 Accueil",            tabName = "home",        icon = icon("home")),
      menuItem("📊 Résultats",           tabName = "results",     icon = icon("chart-bar")),
      menuItem("📈 Évolution temporelle",tabName = "temporal",    icon = icon("timeline")),
      menuItem("🔬 Analyse statistique", tabName = "stats",       icon = icon("microscope")),
      menuItem("📐 Sensibilité",         tabName = "sensitivity", icon = icon("sliders-h")),
      menuItem("🎲 Réplications MC",     tabName = "montecarlo",  icon = icon("dice")),
      menuItem("📖 Théorie",             tabName = "theory",      icon = icon("book"))
    ),

    hr(),

    # ── Paramètres ──────────────────────────────────────────────────────────
    div(style = "padding: 0 15px;",
      tags$h5("⚙️ Paramètres du modèle", style = "color:#adb5bd; font-weight:bold;"),

      sliderInput("lambda", "Taux d'arrivée λ (clients/u.t.) :",
                  min = 0.1, max = 2.5, value = 0.8, step = 0.05),

      sliderInput("mu", "Taux de service μ (clients/u.t.) :",
                  min = 0.2, max = 3.0, value = 1.0, step = 0.05),

      # ρ dynamique
      uiOutput("rho_display"),

      numericInput("T_max", "Horizon de simulation T_max :",
                   value = 20000, min = 500, max = 500000, step = 500),

      numericInput("warm_up", "Période de chauffe :",
                   value = 2000, min = 0, max = 20000, step = 100),

      numericInput("seed", "Graine aléatoire (seed) :",
                   value = 42, min = 1),

      checkboxInput("show_history", "Enregistrer l'historique des événements", TRUE),

      actionButton("run_sim", "▶  Lancer la simulation",
                   class = "btn-primary btn-block",
                   style = "margin-top:10px; font-weight:bold;"),

      actionButton("reset_btn", "↺  Réinitialiser",
                   class = "btn-default btn-block btn-sm")
    )
  ),

  # ── Body ──────────────────────────────────────────────────────────────────
  dashboardBody(

    # CSS personnalisé
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f6f9; }
        .box { border-radius: 8px; }
        .box-title { font-weight: bold; }
        .value-box .icon { font-size: 2.5rem !important; }
        .kpi-label { font-size: 0.78rem; color: #6c757d; text-transform: uppercase; letter-spacing: 1px; }
        .kpi-value { font-size: 1.8rem; font-weight: 800; }
        .stable-badge { display: inline-block; padding: 4px 12px; border-radius: 12px;
                        font-weight: bold; font-size: 0.85rem; }
        .stable   { background: #d4edda; color: #155724; }
        .unstable { background: #f8d7da; color: #721c24; }
        .formula-box { background: #eef2ff; border-left: 4px solid #4f46e5;
                       padding: 12px 16px; border-radius: 4px; margin: 8px 0; }
        table.dataTable thead { background: #2c3e6e; color: white; }
      "))
    ),

    tabItems(

      # ════════════════════════════════════════════════════════════════════════
      # Onglet Accueil
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "home",
        fluidRow(
          box(width = 12, title = "🎯 Simulation M/M/1 — Processus de Poisson",
              status = "primary", solidHeader = TRUE,
              p(style = "font-size:1.05rem;",
                "Cette application permet de simuler et d'analyser une ",
                tags$strong("file d'attente M/M/1"), " :"),
              tags$ul(
                tags$li("Arrivées selon un processus de ", tags$strong("Poisson(λ)")),
                tags$li("Temps de service distribués selon une loi ", tags$strong("Exponentielle(μ)")),
                tags$li("Un seul serveur (mono-canal), file illimitée (FIFO)"),
                tags$li("Condition de stabilité : ", tags$strong("ρ = λ/μ < 1"))
              ),
              hr(),
              fluidRow(
                column(6,
                  div(class = "formula-box",
                    tags$h5("Formules analytiques M/M/1"),
                    withMathJax(),
                    helpText("$$\\rho = \\frac{\\lambda}{\\mu} \\quad \\text{(intensité de trafic)}$$"),
                    helpText("$$L = \\frac{\\rho}{1-\\rho}, \\quad L_q = \\frac{\\rho^2}{1-\\rho}$$"),
                    helpText("$$W = \\frac{1}{\\mu - \\lambda}, \\quad W_q = \\frac{\\lambda}{\\mu(\\mu-\\lambda)}$$")
                  )
                ),
                column(6,
                  div(class = "formula-box",
                    tags$h5("Loi de Little"),
                    helpText("$$L = \\lambda \\cdot W$$"),
                    helpText("$$L_q = \\lambda \\cdot W_q$$"),
                    br(),
                    p(style = "font-size:0.9rem; color:#555;",
                      "Configurez les paramètres dans la barre latérale, puis cliquez sur ",
                      tags$strong("\"Lancer la simulation\""), ".")
                  )
                )
              )
          )
        ),
        fluidRow(
          valueBoxOutput("vb_rho",    width = 3),
          valueBoxOutput("vb_L_theo", width = 3),
          valueBoxOutput("vb_W_theo", width = 3),
          valueBoxOutput("vb_stable", width = 3)
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Onglet Résultats
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "results",
        # KPI boxes
        fluidRow(
          valueBoxOutput("kpi_L",  width = 3),
          valueBoxOutput("kpi_Lq", width = 3),
          valueBoxOutput("kpi_W",  width = 3),
          valueBoxOutput("kpi_Wq", width = 3)
        ),
        fluidRow(
          box(width = 12, title = "📋 Tableau comparatif — Simulé vs Théorique",
              status = "info", solidHeader = TRUE,
              tableOutput("results_table"))
        ),
        fluidRow(
          box(width = 7, title = "📊 Comparaison des indicateurs",
              status = "primary", solidHeader = TRUE,
              plotOutput("performance_plot", height = "340px")),
          box(width = 5, title = "⏱️ Distribution des temps d'attente",
              status = "warning", solidHeader = TRUE,
              plotOutput("wait_time_hist", height = "340px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Onglet Évolution temporelle
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "temporal",
        fluidRow(
          box(width = 12, title = "📈 Évolution de la longueur de file Nq(t)",
              status = "success", solidHeader = TRUE,
              plotOutput("queue_evolution_plot", height = "450px"))
        ),
        fluidRow(
          box(width = 12, title = "🗃️ Derniers événements simulés",
              status = "info", solidHeader = TRUE,
              DTOutput("events_dt"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Onglet Analyse statistique
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "stats",
        fluidRow(
          box(width = 6, title = "📦 Densités — Attente et Séjour",
              status = "primary", solidHeader = TRUE,
              plotOutput("distributions_plot", height = "320px")),
          box(width = 6, title = "🧮 QQ-plot — Test Exponentiel",
              status = "warning", solidHeader = TRUE,
              plotOutput("qqplot", height = "320px"))
        ),
        fluidRow(
          box(width = 6, title = "🔬 Tests statistiques",
              status = "info", solidHeader = TRUE,
              verbatimTextOutput("stat_tests")),
          box(width = 6, title = "📊 Statistiques descriptives",
              status = "success", solidHeader = TRUE,
              verbatimTextOutput("desc_stats"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Onglet Sensibilité
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "sensitivity",
        fluidRow(
          box(width = 12, title = "📐 Analyse de sensibilité — KPIs vs ρ",
              status = "primary", solidHeader = TRUE,
              plotOutput("sensitivity_plot", height = "420px"))
        ),
        fluidRow(
          box(width = 12, title = "📋 Table de sensibilité théorique",
              status = "info", solidHeader = TRUE,
              DTOutput("sensitivity_table"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Onglet Monte Carlo
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "montecarlo",
        fluidRow(
          box(width = 12, status = "warning",
              sliderInput("n_reps", "Nombre de réplications Monte Carlo :",
                          min = 5, max = 100, value = 30, step = 5, width = "50%"),
              actionButton("run_mc", "▶  Lancer les réplications",
                           class = "btn-warning", style = "font-weight:bold;")
          )
        ),
        fluidRow(
          box(width = 12, title = "🎲 Variabilité de Lq sur plusieurs réplications",
              status = "primary", solidHeader = TRUE,
              plotOutput("mc_fan_plot", height = "420px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Onglet Théorie
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "theory",
        fluidRow(
          box(width = 6, title = "📖 Modèle M/M/1 — Rappels théoriques",
              status = "primary", solidHeader = TRUE,
              withMathJax(),
              tags$h5("Notation de Kendall : A/B/c"),
              p("M/M/1 : arrivées Markoviennes, service Markovien, 1 serveur."),
              hr(),
              tags$h5("Processus d'arrivée"),
              helpText("Nombre d'arrivées dans [0,t] suit une loi de Poisson :"),
              helpText("$$P(N(t) = k) = \\frac{(\\lambda t)^k e^{-\\lambda t}}{k!}$$"),
              helpText("Intervalle inter-arrivées ~ Exponentielle(λ)"),
              hr(),
              tags$h5("Condition de stabilité"),
              div(class = "formula-box",
                helpText("$$\\rho = \\frac{\\lambda}{\\mu} < 1$$"),
                p("Si ρ ≥ 1, la file croît indéfiniment.")
              ),
              hr(),
              tags$h5("KPIs à l'état stationnaire"),
              helpText("$$L = \\frac{\\rho}{1-\\rho}$$"),
              helpText("$$L_q = \\frac{\\rho^2}{1-\\rho}$$"),
              helpText("$$W = \\frac{1}{\\mu - \\lambda}$$"),
              helpText("$$W_q = \\frac{\\lambda}{\\mu(\\mu-\\lambda)}$$")
          ),
          box(width = 6, title = "📐 Loi de Little & Probabilités",
              status = "info", solidHeader = TRUE,
              withMathJax(),
              tags$h5("Loi de Little"),
              div(class = "formula-box",
                helpText("$$L = \\lambda \\cdot W, \\quad L_q = \\lambda \\cdot W_q$$")
              ),
              hr(),
              tags$h5("Probabilité d'avoir n clients dans le système"),
              helpText("$$P(N = n) = (1 - \\rho)\\rho^n, \\quad n = 0,1,2,\\ldots$$"),
              hr(),
              tags$h5("Probabilité que le serveur soit occupé"),
              helpText("$$P(\\text{serveur occupé}) = \\rho = \\frac{\\lambda}{\\mu}$$"),
              hr(),
              tags$h5("Distribution du temps de séjour"),
              helpText("$$f_W(t) = (\\mu - \\lambda)e^{-(\\mu-\\lambda)t}, \\quad t \\geq 0$$"),
              hr(),
              tags$h5("Efficacité du serveur"),
              helpText("$$\\eta = 1 - P_0 = 1 - (1-\\rho) = \\rho$$")
          )
        )
      )

    )  # end tabItems
  )    # end dashboardBody
)


# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ── Réactivité : calcul de rho ─────────────────────────────────────────────
  rho_val <- reactive({ input$lambda / input$mu })

  # ── Affichage dynamique de ρ dans la sidebar ───────────────────────────────
  output$rho_display <- renderUI({
    rho  <- rho_val()
    col  <- if (rho < 0.7) "#28a745" else if (rho < 0.9) "#fd7e14" else "#dc3545"
    icon <- if (rho < 1) "✔" else "✘"
    div(style = paste0("text-align:center; margin: 8px 0; padding:8px;
                        background:", if (rho < 1) "#d4edda" else "#f8d7da", ";
                        border-radius:6px;"),
      tags$b(style = paste0("color:", col, "; font-size:1.1rem;"),
             paste0(icon, "  ρ = ", round(rho, 3))),
      br(),
      tags$small(style = "color:#666;",
                 if (rho < 1) paste0("Stable — capacité résiduelle : ", round((1-rho)*100, 1), "%")
                 else "⚠️ Système INSTABLE")
    )
  })

  # ── Value boxes statiques (avant simulation) ───────────────────────────────
  output$vb_rho <- renderValueBox({
    rho <- rho_val()
    valueBox(round(rho, 3), "Intensité ρ = λ/μ",
             icon = icon("tachometer-alt"),
             color = if (rho < 1) "green" else "red")
  })

  output$vb_L_theo <- renderValueBox({
    rho <- rho_val()
    L   <- if (rho < 1) round(rho/(1-rho), 3) else "∞"
    valueBox(L, "L théorique (système)", icon = icon("users"), color = "blue")
  })

  output$vb_W_theo <- renderValueBox({
    rho <- rho_val()
    lam <- input$lambda; mu <- input$mu
    W   <- if (rho < 1) round(1/(mu-lam), 3) else "∞"
    valueBox(W, "W théorique (séjour)", icon = icon("clock"), color = "purple")
  })

  output$vb_stable <- renderValueBox({
    rho <- rho_val()
    valueBox(if (rho < 1) "STABLE" else "INSTABLE",
             paste0("Condition : ρ ", if (rho < 1) "< 1" else "≥ 1"),
             icon = icon(if (rho < 1) "check-circle" else "exclamation-triangle"),
             color = if (rho < 1) "green" else "red")
  })

  # ── Simulation réactive ────────────────────────────────────────────────────
  sim_results <- eventReactive(input$run_sim, {
    req(input$lambda, input$mu, input$T_max)

    validate(
      need(input$warm_up < input$T_max,
           "La période de chauffe doit être inférieure à T_max.")
    )

    withProgress(message = "⏳ Simulation M/M/1 en cours...", value = 0, {
      setProgress(0.2, detail = "Initialisation...")
      Sys.sleep(0.1)
      setProgress(0.5, detail = "Traitement des événements...")
      res <- simulate_mm1(
        lambda         = input$lambda,
        mu             = input$mu,
        T_max          = input$T_max,
        warm_up        = input$warm_up,
        seed           = input$seed,
        record_history = input$show_history
      )
      setProgress(1.0, detail = "Terminé !")
      res
    })
  })

  # ── Réinitialisation ───────────────────────────────────────────────────────
  observeEvent(input$reset_btn, {
    updateSliderInput(session, "lambda", value = 0.8)
    updateSliderInput(session, "mu", value = 1.0)
    updateNumericInput(session, "T_max", value = 20000)
    updateNumericInput(session, "warm_up", value = 2000)
    updateNumericInput(session, "seed", value = 42)
  })

  # ── KPI value boxes (post-simulation) ─────────────────────────────────────
  make_kpi_box <- function(label, sim_val, theo_val, icon_name, color) {
    renderValueBox({
      res <- sim_results()
      req(res)
      pct <- if (theo_val != 0 && is.finite(theo_val))
        paste0("  Δ = ", round(abs(sim_val - theo_val) / theo_val * 100, 1), "%")
      else ""
      valueBox(
        paste0(round(sim_val, 3), pct),
        paste0(label, " (théo. = ", round(theo_val, 3), ")"),
        icon = icon(icon_name), color = color
      )
    })
  }

  output$kpi_L <- renderValueBox({
    res <- sim_results(); req(res)
    valueBox(round(res$simulated["L"], 3),
             paste0("L simulé (théo. = ", round(res$theoretical["L"], 3), ")"),
             icon = icon("users"), color = "blue")
  })
  output$kpi_Lq <- renderValueBox({
    res <- sim_results(); req(res)
    valueBox(round(res$simulated["Lq"], 3),
             paste0("Lq simulé (théo. = ", round(res$theoretical["Lq"], 3), ")"),
             icon = icon("list"), color = "purple")
  })
  output$kpi_W <- renderValueBox({
    res <- sim_results(); req(res)
    valueBox(round(res$simulated["W"], 3),
             paste0("W simulé (théo. = ", round(res$theoretical["W"], 3), ")"),
             icon = icon("clock"), color = "green")
  })
  output$kpi_Wq <- renderValueBox({
    res <- sim_results(); req(res)
    valueBox(round(res$simulated["Wq"], 3),
             paste0("Wq simulé (théo. = ", round(res$theoretical["Wq"], 3), ")"),
             icon = icon("hourglass-half"), color = "orange")
  })

  # ── Tableau des résultats ─────────────────────────────────────────────────
  output$results_table <- renderTable({
    res <- sim_results(); req(res)
    data.frame(
      Indicateur = c("L — nb moyen dans le système",
                     "Lq — nb moyen dans la file",
                     "W — temps moyen de séjour",
                     "Wq — temps moyen d'attente"),
      Simulé     = round(res$simulated, 4),
      Théorique  = round(res$theoretical, 4),
      `|Erreur|` = round(abs(res$simulated - res$theoretical), 4),
      `Erreur %`  = paste0(
        round(100 * abs(res$simulated - res$theoretical) /
                ifelse(is.finite(res$theoretical) & res$theoretical != 0,
                       res$theoretical, 1), 2), "%"
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrrrr")

  # ── Graphiques ────────────────────────────────────────────────────────────
  output$performance_plot    <- renderPlot({ plot_performance_comparison(sim_results()) })
  output$wait_time_hist      <- renderPlot({ plot_wait_histogram(sim_results()) })
  output$queue_evolution_plot <- renderPlot({ plot_queue_evolution(sim_results()) })
  output$distributions_plot  <- renderPlot({ plot_distributions(sim_results()) })
  output$qqplot              <- renderPlot({ plot_qqplot(sim_results()) })

  # ── Table événements (DT) ─────────────────────────────────────────────────
  output$events_dt <- renderDT({
    res <- sim_results(); req(res, !is.null(res$events))
    tail(res$events, 200) |>
      mutate(time = round(time, 4)) |>
      datatable(options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE)
  })

  # ── Tests statistiques ────────────────────────────────────────────────────
  output$stat_tests <- renderPrint({
    res <- sim_results(); req(res)
    wt  <- res$stats$wait_times
    if (length(wt) < 10) { cat("Pas assez d'observations.\n"); return() }

    cat("=== Test de Kolmogorov-Smirnov ===\n")
    rate_theo <- input$mu - input$lambda
    ks  <- ks.test(wt, "pexp", rate = rate_theo)
    cat(sprintf("D        = %.4f\n", ks$statistic))
    cat(sprintf("p-value  = %s\n",   format.pval(ks$p.value, digits = 3)))
    cat(sprintf("Décision : %s\n\n",
                if (ks$p.value > 0.05) "✔ Loi exponentielle non rejetée (α=5%)"
                else "✘ Loi exponentielle rejetée (α=5%)"))

    cat("=== Intervalles de confiance (95%) ===\n")
    n  <- length(wt)
    se <- sd(wt) / sqrt(n)
    ic_low  <- mean(wt) - 1.96 * se
    ic_high <- mean(wt) + 1.96 * se
    cat(sprintf("IC Wq : [%.4f ; %.4f]\n", ic_low, ic_high))
    cat(sprintf("Wq théorique dans l'IC : %s\n",
                if (res$theoretical["Wq"] >= ic_low && res$theoretical["Wq"] <= ic_high)
                  "✔ Oui" else "✘ Non"))
  })

  output$desc_stats <- renderPrint({
    res <- sim_results(); req(res)
    wt <- res$stats$wait_times
    if (length(wt) == 0) { cat("Aucune donnée.\n"); return() }
    cat("=== Temps d'attente (Wq) ===\n")
    cat(sprintf("n          = %d\n",     length(wt)))
    cat(sprintf("Moyenne    = %.4f\n",   mean(wt)))
    cat(sprintf("Écart-type = %.4f\n",   sd(wt)))
    cat(sprintf("Médiane    = %.4f\n",   median(wt)))
    cat(sprintf("Min        = %.4f\n",   min(wt)))
    cat(sprintf("Max        = %.4f\n",   max(wt)))
    cat(sprintf("Q1         = %.4f\n",   quantile(wt, 0.25)))
    cat(sprintf("Q3         = %.4f\n",   quantile(wt, 0.75)))
    cat(sprintf("Skewness   = %.4f\n",   (mean(wt) - median(wt)) / sd(wt)))
    cat("\n=== Méta-données ===\n")
    meta <- res$meta
    cat(sprintf("ρ              = %.4f\n",  meta$rho))
    cat(sprintf("Événements     = %d\n",    meta$event_count))
    cat(sprintf("Arrivées (post warm-up) = %d\n", meta$arrivals_after_warmup))
    cat(sprintf("Système stable : %s\n",    if (meta$stable) "✔ Oui" else "✘ Non"))
  })

  # ── Sensibilité ──────────────────────────────────────────────────────────
  output$sensitivity_plot <- renderPlot({
    sens_df <- sensitivity_analysis(mu = input$mu)
    plot_sensitivity(sens_df)
  })

  output$sensitivity_table <- renderDT({
    sens_df <- sensitivity_analysis(mu = input$mu) |>
      mutate(across(where(is.numeric), ~round(., 4)))
    datatable(sens_df, options = list(pageLength = 10), rownames = FALSE)
  })

  # ── Monte Carlo réplications ──────────────────────────────────────────────
  mc_results <- eventReactive(input$run_mc, {
    withProgress(message = "🎲 Réplications Monte Carlo...", value = 0, {
      n <- input$n_reps
      res_list <- lapply(seq_len(n), function(i) {
        setProgress(i / n, detail = paste0("Réplication ", i, "/", n))
        res <- simulate_mm1(input$lambda, input$mu, input$T_max,
                            warm_up = input$warm_up, seed = i,
                            record_history = FALSE)
        data.frame(rep = i,
                   L  = res$simulated["L"],
                   Lq = res$simulated["Lq"],
                   W  = res$simulated["W"],
                   Wq = res$simulated["Wq"])
      })
      do.call(rbind, res_list)
    })
  })

  output$mc_fan_plot <- renderPlot({
    df <- mc_results(); req(df)
    rho     <- input$lambda / input$mu
    Lq_theo <- if (rho < 1) rho^2 / (1 - rho) else NA

    ggplot(df, aes(x = rep, y = Lq)) +
      geom_point(color = "#2563eb", size = 2.5, alpha = 0.8) +
      geom_line(color = "#93c5fd", alpha = 0.5) +
      geom_hline(yintercept = mean(df$Lq), color = "#2563eb",
                 linetype = "dashed", linewidth = 1) +
      geom_hline(yintercept = Lq_theo, color = "#f97316",
                 linewidth = 1.3) +
      annotate("label", x = max(df$rep) * 0.8, y = Lq_theo,
               label = paste0("Lq théo. = ", round(Lq_theo, 3)),
               color = "#f97316", fill = "#fff7ed", size = 3.5, fontface = "bold") +
      annotate("label", x = max(df$rep) * 0.2, y = mean(df$Lq),
               label = paste0("Lq moy. = ", round(mean(df$Lq), 3)),
               color = "#2563eb", fill = "#eff6ff", size = 3.5, fontface = "bold") +
      labs(title = paste0("Lq simulé sur ", nrow(df), " réplications Monte Carlo"),
           subtitle = paste0("Convergence vers Lq théorique = ", round(Lq_theo, 3)),
           x = "Réplication #", y = "Lq simulé") +
      theme_mm1()
  })
}

# =============================================================================
# Lancement
# =============================================================================
shinyApp(ui = ui, server = server)
