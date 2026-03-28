# 📊 Simulation Interactive M/M/1 — Shiny App

> **Auteur :** Mohamed El Otmany  
> **Formation :** 2ème année Ingénierie — Finance & Sciences Actuarielles, FST Errachidia  
> **Framework :** R + Shiny + shinydashboard  

---

## 📌 Description

Application Shiny interactive permettant de **simuler et analyser** une file d'attente **M/M/1** basée sur les **processus de Poisson**.

Elle propose une approche pédagogique et quantitative du modèle :
- Simulation événementielle (event-driven)
- Comparaison simulé vs formules analytiques
- Tests statistiques (KS-test, IC)
- Analyse de sensibilité et réplications Monte Carlo

---

## 🧮 Modèle M/M/1

| Notation | Signification |
|----------|--------------|
| **M** | Arrivées selon un **processus de Poisson(λ)** |
| **M** | Temps de service **Exponentiel(μ)** |
| **1** | **Un seul serveur**, file FIFO illimitée |
| **ρ = λ/μ** | Intensité de trafic — doit être < 1 |

### Formules analytiques à l'état stationnaire

$$\rho = \frac{\lambda}{\mu}, \quad L = \frac{\rho}{1-\rho}, \quad L_q = \frac{\rho^2}{1-\rho}$$

$$W = \frac{1}{\mu - \lambda}, \quad W_q = \frac{\lambda}{\mu(\mu-\lambda)}$$

**Loi de Little :** $L = \lambda W$ et $L_q = \lambda W_q$

---

## 🚀 Lancement rapide

### Prérequis

```r
install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr",
                   "DT", "R6", "tidyr", "shinyjs"))
```

### Lancer l'application

```r
# Méthode 1 — depuis RStudio : ouvrir app.R puis cliquer "Run App"

# Méthode 2 — depuis la console R
shiny::runApp(".")

# Méthode 3 — depuis GitHub directement
shiny::runGitHub("NOM_DU_REPO", "elotmanymohamed")
```

---

## 📁 Structure du projet

```
mm1_shiny/
├── app.R                   # Application principale (UI + Server)
├── R/
│   ├── simulation.R        # Moteur de simulation M/M/1 (classe R6 + événements)
│   └── plots.R             # Fonctions de visualisation ggplot2
├── www/
│   └── logo.png            # Logo (optionnel)
├── tests/
│   └── test_simulation.R   # Tests unitaires
└── README.md
```

---

## 🗂️ Onglets de l'application

| Onglet | Contenu |
|--------|---------|
| 🏠 **Accueil** | Rappels théoriques, formules, KPIs pré-calculés |
| 📊 **Résultats** | Tableau simulé vs théorique, barplot, histogramme |
| 📈 **Évolution temporelle** | Courbe Nq(t), historique événements |
| 🔬 **Analyse statistique** | KDE, QQ-plot, test KS, intervalles de confiance |
| 📐 **Sensibilité** | Courbes L, Lq, W, Wq en fonction de ρ |
| 🎲 **Réplications MC** | Fan chart — convergence sur N réplications |
| 📖 **Théorie** | Formules complètes avec MathJax |

---

## ⚙️ Paramètres configurables

| Paramètre | Description | Plage |
|-----------|-------------|-------|
| **λ** | Taux d'arrivée | [0.1, 2.5] |
| **μ** | Taux de service | [0.2, 3.0] |
| **T_max** | Horizon de simulation | [500, 500 000] |
| **warm_up** | Période de chauffe exclue | [0, 20 000] |
| **seed** | Graine pour reproductibilité | entier |

---

## 📊 Indicateurs de performance

| KPI | Formule | Interprétation |
|-----|---------|----------------|
| **L** | ρ/(1−ρ) | Nb moyen de clients dans le système |
| **Lq** | ρ²/(1−ρ) | Nb moyen de clients en file |
| **W** | 1/(μ−λ) | Temps moyen de séjour |
| **Wq** | λ/[μ(μ−λ)] | Temps moyen d'attente en file |

---

## 🔬 Méthode de simulation

Le moteur utilise une **simulation à événements discrets (DES)** :

1. Deux types d'événements : **Arrivée** et **Départ**
2. Les temps inter-arrivées et de service sont générés par **inversion de la CDF** : $X = -\ln(U)/\text{rate}$, $U \sim \text{Unif}(0,1)$
3. Les aires sous $N(t)$ et $N_q(t)$ sont intégrées numériquement → donne $L$ et $L_q$
4. La période de chauffe (`warm_up`) est exclue des statistiques

---

## 📚 Références

- Kleinrock, L. (1975). *Queueing Systems, Volume 1*. Wiley.
- Law, A.M. (2007). *Simulation Modeling and Analysis*. McGraw-Hill.
- Gross, D. & Harris, C.M. (1998). *Fundamentals of Queueing Theory*.

---

## 📄 Licence

MIT License — libre d'utilisation à des fins pédagogiques et académiques.
