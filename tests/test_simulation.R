# =============================================================================
# test_simulation.R — Tests unitaires pour simulate_mm1()
# Lancer avec : source("tests/test_simulation.R")
# =============================================================================

source("R/simulation.R")

cat("=== Tests unitaires — Simulation M/M/1 ===\n\n")

pass <- 0; fail <- 0

assert <- function(condition, msg) {
  if (condition) {
    cat(sprintf("  ✔ %s\n", msg)); pass <<- pass + 1
  } else {
    cat(sprintf("  ✘ ÉCHEC : %s\n", msg)); fail <<- fail + 1
  }
}

# ── Test 1 : Stabilité ──────────────────────────────────────────────────────
cat("Test 1 : Condition de stabilité\n")
res <- simulate_mm1(lambda = 0.5, mu = 1.0, T_max = 5000, seed = 1,
                    record_history = FALSE)
assert(res$meta$stable,  "λ=0.5, μ=1.0 → système stable")
assert(res$meta$rho == 0.5, "ρ = λ/μ = 0.5")

res2 <- simulate_mm1(lambda = 1.2, mu = 1.0, T_max = 1000, seed = 1,
                     record_history = FALSE)
assert(!res2$meta$stable, "λ=1.2 > μ=1.0 → système instable")

# ── Test 2 : Convergence vers les valeurs théoriques ────────────────────────
cat("\nTest 2 : Convergence (T_max = 100 000)\n")
res_conv <- simulate_mm1(lambda = 0.7, mu = 1.0, T_max = 100000,
                         warm_up = 5000, seed = 42, record_history = FALSE)

rho    <- 0.7
L_theo  <- rho / (1 - rho)
Lq_theo <- rho^2 / (1 - rho)
W_theo  <- 1 / (1.0 - 0.7)
Wq_theo <- 0.7 / (1.0 * (1.0 - 0.7))

tol <- 0.10  # tolérance 10%
assert(abs(res_conv$simulated["L"]  - L_theo)  / L_theo  < tol, "L  converge vers L_théo")
assert(abs(res_conv$simulated["Lq"] - Lq_theo) / Lq_theo < tol, "Lq converge vers Lq_théo")
assert(abs(res_conv$simulated["W"]  - W_theo)  / W_theo  < tol, "W  converge vers W_théo")
assert(abs(res_conv$simulated["Wq"] - Wq_theo) / Wq_theo < tol, "Wq converge vers Wq_théo")

# ── Test 3 : Reproductibilité ────────────────────────────────────────────────
cat("\nTest 3 : Reproductibilité (même seed)\n")
r1 <- simulate_mm1(0.6, 1.0, 5000, seed = 99, record_history = FALSE)
r2 <- simulate_mm1(0.6, 1.0, 5000, seed = 99, record_history = FALSE)
assert(all(r1$simulated == r2$simulated), "Même seed → mêmes résultats")

# ── Test 4 : Structure de la sortie ─────────────────────────────────────────
cat("\nTest 4 : Structure de la sortie\n")
res3 <- simulate_mm1(0.5, 1.0, 5000, seed = 1)
assert(all(c("L","Lq","Wq","W") %in% names(res3$simulated)),   "simulated contient L, Lq, Wq, W")
assert(all(c("L","Lq","Wq","W") %in% names(res3$theoretical)), "theoretical contient L, Lq, Wq, W")
assert(!is.null(res3$events),  "events non nul (record_history = TRUE)")
assert(!is.null(res3$meta),    "meta non nul")

# ── Bilan ────────────────────────────────────────────────────────────────────
cat(sprintf("\n=== Bilan : %d/✔  %d/✘ ===\n", pass, fail))
if (fail == 0) cat("Tous les tests sont passés ✔\n")
