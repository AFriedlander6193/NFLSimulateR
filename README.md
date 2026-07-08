# NFLSimulateR

`NFLSimulateR` is an R package for simulating NFL games one play at a time using statistical and machine learning models trained on historical NFL play-by-play and PFF player data.

Rather than modeling an entire play with a single black-box model, **NFLSimulator** simulates each decision sequentially, conditioning every step on the game state and the decisions that preceded it. This produces realistic game simulations that account for personnel, formations, coverages, player ability, and game situation.

---

## Installation

### Windows Users

If installing from source on Windows, ensure that the appropriate version of **Rtools** is installed (e.g., **Rtools45** for recent versions of R).

### Recommended (`pak`)

```r
install.packages("pak")
pak::pak("AFriedlander6193/NFLSimulateR")
```

### Alternative (`remotes`)

```r
install.packages("remotes")
remotes::install_github("AFriedlander6193/NFLSimulateR")
```

---

## Features

* Simulate complete NFL games play-by-play.
* Simulate hundreds or thousands of games to estimate expected outcomes.
* Generate realistic offensive and defensive personnel packages.
* Predict offensive formations and defensive coverage schemes.
* Simulate run/pass decisions conditioned on game situation.
* Generate realistic route combinations and targeted receivers.
* Incorporate season-specific PFF player grades into every play.
* Simulate sacks, completions, interceptions, fumbles, and yards gained.
* Built using Bayesian and gradient boosting (XGBoost) models trained on historical NFL data.

---

## Simulation Pipeline

Every play is generated sequentially by conditioning each decision on previous decisions.

```text
Game Situation
      │
      ▼
Offensive Personnel
      │
      ▼
Defensive Personnel
      │
      ▼
Offensive Formation
      │
      ▼
Defensive Coverage
      │
      ▼
Play Type (Run / Pass)
      │
      ▼
Player Assignments
      │
      ▼
Play Outcome
      │
      ▼
Yards Gained
```

This sequential modeling framework allows the simulator to produce realistic play-by-play outcomes while accounting for both team tendencies and player ability.

---

## Quick Start

Simulate a single NFL game:

```r
library(NFLSimulator)

simulate_game(
  team1 = "KC",
  team2 = "PHI",
  year = 2025,
  track = "yes"
)
```

Run many simulations of the same matchup:

```r
simulations <- simulate_multiple_games(
  team1 = "KC",
  team2 = "PHI",
  year = 2025,
  n = 10
)
```

---

## Advanced Usage

In addition to full-game simulation, `NFLSimulateR` exposes the individual components of the simulation engine for users who wish to build custom workflows or inspect intermediate decisions.

Examples include:

* Choosing offensive personnel
* Choosing defensive personnel
* Choosing offensive formations
* Choosing defensive coverage
* Choosing play type (run/pass)
* Generating play details
* Simulating play outcomes and yards gained

These functions can be combined to build custom simulations or to study individual aspects of football strategy.

---

## Data Sources

`NFLSimulateR` uses publicly available NFL data together with PFF player grading data.

Primary data sources include:

* **nflverse**
* **nflreadr**
* **Pro Football Focus (PFF)**

---

## Citation

If you use `NFLSimulateR` in research or academic work, please cite the package using:

```r
citation("NFLSimulateR")
```

---

## Development Status

`NFLSimulateR` is under active development. Planned additions include:

* Special teams simulation
* Improved clock management
* Additional Coaching decisions (timeouts, fourth downs, etc.)
* Expanded support for past NFL seasons
* Weekly PFF updating for future seasons

