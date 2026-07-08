# NFLSimulator

`NFLSimulator` is an R package for simulating NFL games one play at a time using
statistical models trained on historical NFL play-by-play and PFF player
grading data. Rather than relying on fixed probabilities, the simulator models
each stage of a play sequentially, producing realistic game outcomes that adapt
to game situation, personnel, formations, coverages, and player ability.

## Installation

### Prerequisites (Windows)

If installing from source on Windows, make sure you have **Rtools45** (or the
appropriate version for your version of R) installed.

### Recommended

```r
install.packages("pak")
pak::pak("AFriedlander6193/NFLSimulator")
```

### Alternative

```r
install.packages("remotes")
remotes::install_github("AFriedlander6193/NFLSimulator")
```

## Features

- Simulate complete NFL games play-by-play.
- Generate offensive and defensive personnel packages.
- Predict offensive formations and defensive coverage schemes.
- Simulate run/pass decisions conditioned on game state.
- Generate realistic route combinations and targeted receivers.
- Incorporate season-specific PFF player grades.
- Simulate sacks, completions, interceptions, fumbles, and yards gained.
- Built using statistical and machine learning models trained on historical NFL data.

## Simulation Pipeline

Each play is simulated sequentially by conditioning each decision on the
previous decisions.

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
Run / Pass
      │
      ▼
Player Assignments
      │
      ▼
Play Outcome
```

## Example

```r
library(NFLSimulator)

offense <- full_PFF("KC", 2025)
defense <- full_PFF("PHI", 2025)

simulate_yards_gained(
  posstm = "KC",
  deftm = "PHI",
  down = 1,
  togo = 10,
  YdsBef = 75,
  posstmdiff = 0,
  quarter_secs = 900,
  quarter = 1,
  off_dat = offense,
  def_dat = defense,
  year = 2025
)
```
