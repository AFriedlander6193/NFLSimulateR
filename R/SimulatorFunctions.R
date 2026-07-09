

# Functions for Loading Data ----------------------------------------------

.nfl_env <- new.env(parent = emptyenv())

loading_ext_data <- function() {
  path <- system.file("extdata", package = "NFLSimulateR")
  files <- list.files(path, pattern = "\\.rds$", full.names = TRUE)

  data_env <- new.env(parent = emptyenv())

  for (f in files) {
    obj <- readRDS(f)
    name <- tools::file_path_sans_ext(basename(f))
    data_env[[name]] <- obj
  }

  data_env
}

get_data_env <- function() {
  if (!exists("data_env", envir = .nfl_env, inherits = FALSE)) {
    .nfl_env$data_env <- loading_ext_data()
  }
  .nfl_env$data_env
}



# Generally Useful Functions ----------------------------------------------

clean_names <- function(data){
  x <- data
  x <- gsub("\\s+(II|III|IV|V|Jr\\.|Sr\\.)$", "", x)
  x <- sub(" (Q|IR|O|SUSP|D)$", "", x)
  x <- gsub("\\.", "", x)
  x
}


age_joiner <- function(pff_data){
  agedat <- nflreadr::load_players() |>
    dplyr::select(display_name, pff_id, birth_date)
  agedat$pff_id <- as.double(agedat$pff_id)
  new_dat <- suppressMessages(left_join(pff_data, agedat))
  new_dat <- new_dat |>
    dplyr::mutate(birthyear = substr(birth_date, 1, 4))
  new_dat$birthyear <- as.numeric(new_dat$birthyear)
  new_dat <- new_dat |>
    dplyr::mutate(Age = Year - birthyear)
  new_dat |>
    dplyr::select(-display_name)
}

# Depth Chart Functions ----------------------------------------------

#' Get an NFL Team Depth Chart
#'
#' Retrieves the most recent depth chart for an NFL team or the depth chart
#' from a provided year and week
#'
#' @param tm Character string. A two- or three-letter NFL team abbreviation.
#'   Must be one of the following:
#'   \code{"ARI"}, \code{"ATL"}, \code{"BAL"}, \code{"BUF"}, \code{"CAR"},
#'   \code{"CHI"}, \code{"CIN"}, \code{"CLE"}, \code{"DAL"}, \code{"DEN"},
#'   \code{"DET"}, \code{"GB"}, \code{"HOU"}, \code{"IND"}, \code{"JAX"},
#'   \code{"KC"}, \code{"LA"}, \code{"LAC"}, \code{"LV"}, \code{"MIA"},
#'   \code{"MIN"}, \code{"NE"}, \code{"NO"}, \code{"NYG"}, \code{"NYJ"},
#'   \code{"PHI"}, \code{"PIT"}, \code{"SEA"}, \code{"SF"}, \code{"TB"},
#'   \code{"TEN"}, \code{"WAS"}.
#'
#' @param year Numeric. Season year. Must be an integer between
#'   \code{2022} and \code{2025}.
#'
#' @param week Numeric. Week of the regular season. Must be an integer
#'   between \code{1} and \code{18}.
#'
#' @param offseason Logical. Indicates if it is currently the NFL offseason
#'
#' @return A data.frame with one row per player on the team's depth chart.
#' \describe{
#'   \item{position}{Position from the official NFL depth chart.}
#'   \item{player}{Player name.}
#'   \item{depth_team}{Depth chart rank at the position (1 = starter).}
#'   \item{final_position}{Simplified position category used by the simulator.}
#'   \item{pff_id}{Unique PFF player identifier.}
#' }
#'
#' @details
#' Returns the offensive and defensive depth chart (up to three players per
#' position) for the specified team and week. Special teams positions are
#' excluded.
#'
#'
#' @export
depthchart <- function(tm, year = 2025, week = 1, offseason = FALSE){

  game_dates <- c(
    "-09-04", "-09-11", "-09-18", "-09-25",
    "-10-02", "-10-09", "-10-16", "-10-23",
    "-10-30", "-11-06", "-11-13", "-11-20",
    "-11-27", "-12-04", "-12-11", "-12-18",
    "-12-25", "-01-01"
  )

  today <- paste0("-", format(Sys.Date(), "%m-%d"))


  game_date <- game_dates[week]

  if (isTRUE(offseason)) {
    game_date <- today
  }

  baseteamchart <- nflreadr::load_depth_charts(year) |>
    dplyr::mutate(as_of_date = as.Date(dt)) |>
    dplyr::filter(as_of_date == as.Date(paste0(year, game_date))) |>
    dplyr::filter(team==tm & dt == dt[1]) |>
    dplyr::filter(!pos_abb %in% c("PK", "P", "H", "PR", "KR", "LS")) |>
    dplyr::select(team, player_name, espn_id, pos_abb, pos_rank)
  allplayers <- nflreadr::load_players() |>
    dplyr::select(pff_id, espn_id)
  newchart <- suppressMessages(left_join(baseteamchart, allplayers))
  newchart <- newchart |>
    dplyr::mutate(position = pos_abb,
           player = player_name,
           final_position = paste0(pos_abb, pos_rank)) |>
    dplyr::select(position, player, final_position, pff_id)
  newchart
}

defense_label_updater <- function(position) {
  if (position %in% c("DI", "ED", "LB")) {
    position <- "F7"
  }

  if (position %in% c("CB", "S")) {
    position <- "DB"
  }
  position
}

offensive_line_label_updater <- function(position) {
  if (position %in% c("T", "G", "C")) {
    position <- "OL"
  }
  position
}

PFF_data_grabber <- function(year, pos_file) {
  ### `pos_file` can be "QBs", "Rushing", "Receiving", "Blocking", "Defense"
  ### `cur_last` can be "last" or "cur"

  # data_env <- new.env()

  # Load the file
  file_path <- system.file(
    "extdata",
    paste0("PFF_", pos_file, "_", year, ".rds"),
    package = "NFLSimulateR",
    mustWork = TRUE
  )

  # Access it dynamically
  data1 <- readRDS(file_path)

  data1$grades_NA <- NA

  grades_stat <- switch(pos_file,
                        QBs = c("pass"),
                        Rushing = c("run"),
                        Receiving = c("pass_route"),
                        Blocking = c("pass_block", "run_block"),
                        Defense = c("pass_rush_defense",
                                    "coverage_defense",
                                    "run_defense")
                        )

  if (pos_file == "Defense") {
    data1$snap_counts_defense =
      data1$snap_counts_coverage +
      data1$snap_counts_pass_rush +
      data1$snap_counts_run_defense
  }

  attempts <- switch(pos_file,
                     QBs = "attempts",
                     Rushing = "attempts",
                     Receiving = "routes",
                     Blocking = "snap_counts_offense",
                     Defense = "snap_counts_defense"
                     )

  data2 <- data1 |>
    dplyr::group_by(player_id) |>
    dplyr::summarise(player = player[1],
              Year = year,
              position = position[1],
              games = n(),
              attempts = mean(.data[[attempts]], na.rm = TRUE),
              var1_mean = mean(.data[[paste0("grades_", grades_stat[1])]], na.rm = TRUE),
              var1_sd = sd(.data[[paste0("grades_", grades_stat[1])]], na.rm = TRUE),
              var2_mean = if_else(length(grades_stat) < 2, NA,
                                  mean(.data[[paste0("grades_", grades_stat[2])]],
                                       na.rm = TRUE)),
              var2_sd = if_else(length(grades_stat) < 2, NA,
                                  sd(.data[[paste0("grades_", grades_stat[2])]],
                                     na.rm = TRUE)),
              var3_mean = if_else(length(grades_stat) < 3, NA,
                                  mean(.data[[paste0("grades_", grades_stat[3])]],
                                       na.rm = TRUE)),
              var3_sd = if_else(length(grades_stat) < 3, NA,
                                sd(.data[[paste0("grades_", grades_stat[3])]],
                                   na.rm = TRUE))
              )

  new_column_names <- switch(pos_file,
                         QBs = c(paste0("mean_pass_attempts"),
                                 paste0("mean_pass"),
                                 paste0("sd_pass"),
                                 "NA_col", "NA_col", "NA_col", "NA_col"),
                         Rushing = c(paste0("mean_rush_attempts"),
                                 paste0("mean_rush"),
                                 paste0("sd_rush"),
                                 "NA_col", "NA_col", "NA_col", "NA_col"),
                         Receiving = c(paste0("mean_routes"),
                                 paste0("mean_rec"),
                                 paste0("sd_rec"),
                                 "NA_col", "NA_col", "NA_col", "NA_col"),
                         Blocking = c(paste0("mean_off_snaps"),
                                 paste0("mean_pass_block"),
                                 paste0("sd_pass_block"),
                                 paste0("mean_run_block"),
                                 paste0("sd_run_block"),
                                 "NA_col", "NA_col"),
                         Defense = c(paste0("mean_def_snaps"),
                                      paste0("mean_pass_rush_defense"),
                                      paste0("sd_pass_rush_defense"),
                                      paste0("mean_coverage_defense"),
                                      paste0("sd_coverage_defense"),
                                      paste0("mean_run_defense"),
                                      paste0("sd_run_defense")),
                         )

  colnames(data2) <- c("player_id", "player", "Year", "position",
                       "games",new_column_names)

  data2 |>
    dplyr::select(where(~ !all(is.na(.))))
}

#check <- PFF_data_grabber(2025, "Defense")

fill_na_p30 <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) {
      p30 <- quantile(col, 0.30, na.rm = TRUE)
      col[is.na(col)] <- p30
    }
    col
  })
  return(df)
}

PFF_data_creator <- function(year, pos) {

  pos <- defense_label_updater(pos)
  pos <- offensive_line_label_updater(pos)

  pos_file <- switch(pos,
                     QB = c("QBs", "Rushing"),
                     HB = c("Rushing", "Receiving", "Blocking"),
                     WR = c("Receiving", "Blocking"),
                     TE = c("Receiving", "Blocking"),
                     OL = c("Blocking"),
                     F7 = c("Defense"),
                     DB = c("Defense"))

  min_snaps <- switch(pos,
                      QB = 10,
                      HB = 3,
                      WR = 3,
                      TE = 3,
                      OL = 5,
                      F7 = 5,
                      DB = 5)

  # if (cur_last == "last"){
  #   year = year - 1
  # }

  base_pff_file <- PFF_data_grabber(year, pos_file[1])
  base_pff_file <- base_pff_file |>
    dplyr::mutate(position = case_when(
      position %in% c("T", "G", "C") ~ "OL",
      position %in% c("DI", "ED", "LB") ~ "F7",
      position %in% c("CB", "S") ~ "DB",
      TRUE ~ position
    )) |>
    dplyr::filter(position == pos)

  base_pff_file <- base_pff_file[base_pff_file[[6]] > min_snaps, ]
  base_pff_file$total_snaps <- base_pff_file[[6]] * base_pff_file$games
  base_pff_file <- base_pff_file |>
    dplyr::relocate(total_snaps, .after = games)

  for (i in seq_len(length(pos_file))[-1]) {
    pff_data <- PFF_data_grabber(year, pos_file[i])
    pff_data <- pff_data[-6] |>
      dplyr::select(-games)
    base_pff_file <- suppressMessages(
      dplyr::left_join(base_pff_file, pff_data)
    )
  }

  colnames(base_pff_file)[colnames(base_pff_file) == "player_id"] <- "pff_id"

  base_pff_file |>
    fill_na_p30() |>
    age_joiner()
}

#check2 <- PFF_data_creator(2025, "CB")


#' Create a Team PFF Data Frame
#'
#' Creates a team-level data frame of PFF grades and usage metrics for players
#' on a selected NFL team.
#'
#' @param tm Character string. A two- or three-letter NFL team abbreviation.
#'   Must be one of the following:
#'   \code{"ARI"}, \code{"ATL"}, \code{"BAL"}, \code{"BUF"}, \code{"CAR"},
#'   \code{"CHI"}, \code{"CIN"}, \code{"CLE"}, \code{"DAL"}, \code{"DEN"},
#'   \code{"DET"}, \code{"GB"}, \code{"HOU"}, \code{"IND"}, \code{"JAX"},
#'   \code{"KC"}, \code{"LA"}, \code{"LAC"}, \code{"LV"}, \code{"MIA"},
#'   \code{"MIN"}, \code{"NE"}, \code{"NO"}, \code{"NYG"}, \code{"NYJ"},
#'   \code{"PHI"}, \code{"PIT"}, \code{"SEA"}, \code{"SF"}, \code{"TB"},
#'   \code{"TEN"}, \code{"WAS"}.
#'
#' @param year Numeric. Season year. Must be an integer between
#'   \code{2022} and \code{2025}.
#'
#' @param week Numeric. Week of the regular season used to identify the team's
#'   depth chart. Must be an integer between \code{1} and \code{18}.
#'
#' @return A data.frame containing season-level PFF player grades and usage
#' metrics for the selected team. Each row corresponds to one player.
#' \describe{
#'   \item{final_position}{Simplified position group used by
#'   \code{NFLSimulateR} (e.g., QB, HB, WR, TE, OL, F7, DB).}
#'   \item{pff_id}{Unique PFF player identifier.}
#'   \item{player}{Player name.}
#'   \item{year}{NFL season.}
#'   \item{position}{Player's original position designation.}
#'   \item{games}{Number of games played.}
#'   \item{total_snaps}{Total offensive or defensive snaps played.}
#'   \item{mean_pass_attempts}{Average pass attempts per game (quarterbacks).}
#'   \item{mean_pass}{Mean PFF passing grade.}
#'   \item{sd_pass}{Standard deviation of PFF passing grades.}
#'   \item{mean_rush}{Mean PFF rushing grade.}
#'   \item{sd_rush}{Standard deviation of PFF rushing grades.}
#'   \item{birth_date}{Player's date of birth.}
#'   \item{birthyear}{Player's birth year.}
#'   \item{age}{Player age during the selected season.}
#'   \item{mean_rush_attempts}{Average rushing attempts per game.}
#'   \item{mean_rec}{Mean PFF receiving grade.}
#'   \item{sd_rec}{Standard deviation of PFF receiving grades.}
#'   \item{mean_pass_block}{Mean PFF pass-blocking grade.}
#'   \item{sd_pass_block}{Standard deviation of PFF pass-blocking grades.}
#'   \item{mean_run_block}{Mean PFF run-blocking grade.}
#'   \item{sd_run_block}{Standard deviation of PFF run-blocking grades.}
#'   \item{mean_routes}{Average routes run per game.}
#'   \item{mean_off_snaps}{Average offensive snaps per game.}
#'   \item{mean_def_snaps}{Average defensive snaps per game.}
#'   \item{mean_pass_rush_defense}{Mean PFF pass-rushing grade.}
#'   \item{sd_pass_rush_defense}{Standard deviation of PFF pass-rushing grades.}
#'   \item{mean_coverage_defense}{Mean PFF coverage grade.}
#'   \item{sd_coverage_defense}{Standard deviation of PFF coverage grades.}
#'   \item{mean_run_defense}{Mean PFF run-defense grade.}
#'   \item{sd_run_defense}{Standard deviation of PFF run-defense grades.}
#' }
#'
#' @details
#' This function combines team depth chart information with position-specific
#' PFF grade and usage data. Positions are grouped into simplified categories
#' used by \code{NFLSimulateR}, such as \code{OL}, \code{F7}, and \code{DB}.
#' Some columns may contain missing values for positions where the statistic is
#' not applicable.
#'
#' @export
full_PFF <- function(tm, year = 2025, week = 1){

  dpt_cht <- depthchart(tm, year, week) |>
    dplyr::select(-c(position, player))

  if (week <= 2) {
    quarterback <- PFF_data_creator(year - 1, "QB")
    runningbacks <- PFF_data_creator(year - 1, "HB")
    widereceivers <- PFF_data_creator(year - 1, "WR")
    tightends <- PFF_data_creator(year - 1, "TE")
    oline <- PFF_data_creator(year - 1, "OL")
    frontseven <- PFF_data_creator(year - 1, "F7")
    defensivebacks <- PFF_data_creator(year - 1, "DB")
  } else if (week > 2 & week < 9) {
    quarterback <- PFF_data_creator(year, "QB")
    quarterback_prev <- PFF_data_creator(year - 1, "QB") |>
      dplyr::select(pff_id, mean_pass, sd_pass)
    colnames(quarterback_prev) <- c("pff_id", "mean_pass1", "sd_pass1")
    quarterback <- dplyr::left_join(quarterback, quarterback_prev) |>
      suppressMessages()
    quarterback <- quarterback |>
      dplyr::mutate(
        mean_pass = dplyr::coalesce(
          (((week - 2) * mean_pass) + ((9 - week) * mean_pass1)) /
            ((week - 2) + (9 - week)),
          mean_pass,
          mean_pass1
        )
      ) |>
      dplyr::select(-c("mean_pass1", "sd_pass1"))

    runningbacks <- PFF_data_creator(year, "HB")
    runningbacks_prev <- PFF_data_creator(year - 1, "HB") |>
      dplyr::select(pff_id, mean_rush, mean_rec, mean_pass_block, mean_run_block)
    colnames(runningbacks_prev) <- c("pff_id", "mean_rush1", "mean_rec1",
                                     "mean_pass_block1", "mean_run_block1")

    runningbacks <- dplyr::left_join(runningbacks, runningbacks_prev) |>
      suppressMessages()
    runningbacks <- runningbacks |>
      dplyr::mutate(
        mean_rush = dplyr::coalesce(
          (((week - 2) * mean_rush) + ((9 - week) * mean_rush1)) /
            ((week - 2) + (9 - week)),
          mean_rush,
          mean_rush1
        ),
        mean_rec = dplyr::coalesce(
          (((week - 2) * mean_rec) + ((9 - week) * mean_rec1)) /
            ((week - 2) + (9 - week)),
          mean_rec,
          mean_rec1
        ),
        mean_pass_block = dplyr::coalesce(
          (((week - 2) * mean_pass_block) + ((9 - week) * mean_pass_block1)) /
            ((week - 2) + (9 - week)),
          mean_pass_block,
          mean_pass_block1
        ),
        mean_run_block = dplyr::coalesce(
          (((week - 2) * mean_run_block) + ((9 - week) * mean_run_block1)) /
            ((week - 2) + (9 - week)),
          mean_run_block,
          mean_run_block1
        )
      ) |>
      dplyr::select(-c("mean_rush1", "mean_rec1",
                       "mean_pass_block1", "mean_run_block1"))

    widereceivers <- PFF_data_creator(year, "WR")
    widereceivers_prev <- PFF_data_creator(year - 1, "WR") |>
      dplyr::select(pff_id, mean_rec, mean_pass_block, mean_run_block)
    colnames(widereceivers_prev) <- c("pff_id", "mean_rec1",
                                     "mean_pass_block1", "mean_run_block1")

    widereceivers <- dplyr::left_join(widereceivers, widereceivers_prev) |>
      suppressMessages()
    widereceivers <- widereceivers |>
      dplyr::mutate(
        mean_rec = dplyr::coalesce(
          (((week - 2) * mean_rec) + ((9 - week) * mean_rec1)) /
            ((week - 2) + (9 - week)),
          mean_rec,
          mean_rec1
        ),
        mean_pass_block = dplyr::coalesce(
          (((week - 2) * mean_pass_block) + ((9 - week) * mean_pass_block1)) /
            ((week - 2) + (9 - week)),
          mean_pass_block,
          mean_pass_block1
        ),
        mean_run_block = dplyr::coalesce(
          (((week - 2) * mean_run_block) + ((9 - week) * mean_run_block1)) /
            ((week - 2) + (9 - week)),
          mean_run_block,
          mean_run_block1
        )
      ) |>
      dplyr::select(-c("mean_rec1",
                       "mean_pass_block1", "mean_run_block1"))

    tightends <- PFF_data_creator(year, "TE")
    tightends_prev <- PFF_data_creator(year - 1, "TE") |>
      dplyr::select(pff_id, mean_rec, mean_pass_block, mean_run_block)
    colnames(tightends_prev) <- c("pff_id", "mean_rec1",
                                     "mean_pass_block1", "mean_run_block1")

    tightends <- dplyr::left_join(tightends, tightends_prev) |>
      suppressMessages()
    tightends <- tightends |>
      dplyr::mutate(
        mean_rec = dplyr::coalesce(
          (((week - 2) * mean_rec) + ((9 - week) * mean_rec1)) /
            ((week - 2) + (9 - week)),
          mean_rec,
          mean_rec1
        ),
        mean_pass_block = dplyr::coalesce(
          (((week - 2) * mean_pass_block) + ((9 - week) * mean_pass_block1)) /
            ((week - 2) + (9 - week)),
          mean_pass_block,
          mean_pass_block1
        ),
        mean_run_block = dplyr::coalesce(
          (((week - 2) * mean_run_block) + ((9 - week) * mean_run_block1)) /
            ((week - 2) + (9 - week)),
          mean_run_block,
          mean_run_block1
        )
      ) |>
      dplyr::select(-c("mean_rec1",
                       "mean_pass_block1", "mean_run_block1"))


    oline <- PFF_data_creator(year, "OL")
    oline_prev <- PFF_data_creator(year - 1, "OL") |>
      dplyr::select(pff_id, mean_pass_block, mean_run_block)
    colnames(oline_prev) <- c("pff_id",
                                  "mean_pass_block1", "mean_run_block1")

    oline <- dplyr::left_join(oline, oline_prev) |>
      suppressMessages()
    oline <- oline |>
      dplyr::mutate(
        mean_pass_block = dplyr::coalesce(
          (((week - 2) * mean_pass_block) + ((9 - week) * mean_pass_block1)) /
            ((week - 2) + (9 - week)),
          mean_pass_block,
          mean_pass_block1
        ),
        mean_run_block = dplyr::coalesce(
          (((week - 2) * mean_run_block) + ((9 - week) * mean_run_block1)) /
            ((week - 2) + (9 - week)),
          mean_run_block,
          mean_run_block1
        )
      ) |>
      dplyr::select(-c("mean_pass_block1", "mean_run_block1"))


    frontseven <- PFF_data_creator(year, "F7")
    frontseven_prev <- PFF_data_creator(year - 1, "F7") |>
      dplyr::select(pff_id, mean_pass_rush_defense, mean_coverage_defense,
                    mean_run_defense)
    colnames(frontseven_prev) <- c("pff_id",
                              "mean_pass_rush_defense1", "mean_coverage_defense1",
                              "mean_run_defense1")

    frontseven <- dplyr::left_join(frontseven, frontseven_prev) |>
      suppressMessages()
    frontseven <- frontseven |>
      dplyr::mutate(
        mean_pass_rush_defense = dplyr::coalesce(
          (((week - 2) * mean_pass_rush_defense) + ((9 - week) * mean_pass_rush_defense1)) /
            ((week - 2) + (9 - week)),
          mean_pass_rush_defense,
          mean_pass_rush_defense1
        ),
        mean_coverage_defense = dplyr::coalesce(
          (((week - 2) * mean_coverage_defense) + ((9 - week) * mean_coverage_defense1)) /
            ((week - 2) + (9 - week)),
          mean_coverage_defense,
          mean_coverage_defense1
        ),
        mean_run_defense = dplyr::coalesce(
          (((week - 2) * mean_run_defense) + ((9 - week) * mean_run_defense1)) /
            ((week - 2) + (9 - week)),
          mean_run_defense,
          mean_run_defense1
        )
      ) |>
      dplyr::select(-c("mean_pass_rush_defense1", "mean_coverage_defense1",
                       "mean_run_defense1"))

    defensivebacks <- PFF_data_creator(year, "DB")
    defensivebacks_prev <- PFF_data_creator(year - 1, "DB") |>
      dplyr::select(pff_id, mean_pass_rush_defense, mean_coverage_defense,
                    mean_run_defense)
    colnames(defensivebacks_prev) <- c("pff_id",
                                   "mean_pass_rush_defense1", "mean_coverage_defense1",
                                   "mean_run_defense1")

    defensivebacks <- dplyr::left_join(defensivebacks, defensivebacks_prev) |>
      suppressMessages()
    defensivebacks <- defensivebacks |>
      dplyr::mutate(
        mean_pass_rush_defense = dplyr::coalesce(
          (((week - 2) * mean_pass_rush_defense) + ((9 - week) * mean_pass_rush_defense1)) /
            ((week - 2) + (9 - week)),
          mean_pass_rush_defense,
          mean_pass_rush_defense1
        ),
        mean_coverage_defense = dplyr::coalesce(
          (((week - 2) * mean_coverage_defense) + ((9 - week) * mean_coverage_defense1)) /
            ((week - 2) + (9 - week)),
          mean_coverage_defense,
          mean_coverage_defense1
        ),
        mean_run_defense = dplyr::coalesce(
          (((week - 2) * mean_run_defense) + ((9 - week) * mean_run_defense1)) /
            ((week - 2) + (9 - week)),
          mean_run_defense,
          mean_run_defense1
        )
      )  |>
      dplyr::select(-c("mean_pass_rush_defense1", "mean_coverage_defense1",
                       "mean_run_defense1"))

  } else {
    quarterback <- PFF_data_creator(year, "QB")
    runningbacks <- PFF_data_creator(year, "HB")
    widereceivers <- PFF_data_creator(year, "WR")
    tightends <- PFF_data_creator(year, "TE")
    oline <- PFF_data_creator(year, "OL")
    frontseven <- PFF_data_creator(year, "F7")
    defensivebacks <- PFF_data_creator(year, "DB")
  }

  quarterback <- quarterback |>
    dplyr::mutate(mean_pass = ifelse(is.na(mean_pass), 42.5, mean_pass),
           sd_pass = ifelse(is.na(sd_pass), 12.5, sd_pass)
           )

  runningbacks <- runningbacks |>
    dplyr::mutate(mean_rush = ifelse(is.na(mean_rush), 42.5, mean_rush),
                  sd_rush = ifelse(is.na(sd_rush), 12.5, sd_rush),
                  mean_rec = ifelse(is.na(mean_rec), 42.5, mean_rec),
                  sd_rec = ifelse(is.na(sd_rush), 12.5, sd_rec),
                  mean_pass_block = ifelse(is.na(mean_pass_block), 42.5, mean_pass_block),
                  sd_pass_block = ifelse(is.na(sd_pass_block), 12.5, sd_pass_block),
                  mean_run_block = ifelse(is.na(mean_run_block), 42.5, mean_run_block),
                  sd_run_block = ifelse(is.na(sd_run_block), 12.5, sd_run_block)
    )

  widereceivers <- widereceivers |>
    dplyr::mutate(mean_rec = ifelse(is.na(mean_rec), 42.5, mean_rec),
                  sd_rec = ifelse(is.na(sd_rec), 12.5, sd_rec),
                  mean_pass_block = ifelse(is.na(mean_pass_block), 42.5, mean_pass_block),
                  sd_pass_block = ifelse(is.na(sd_pass_block), 12.5, sd_pass_block),
                  mean_run_block = ifelse(is.na(mean_run_block), 42.5, mean_run_block),
                  sd_run_block = ifelse(is.na(sd_run_block), 12.5, sd_run_block)
    )

  tightends <- tightends |>
    dplyr::mutate(mean_rec = ifelse(is.na(mean_rec), 42.5, mean_rec),
                  sd_rec = ifelse(is.na(sd_rec), 12.5, sd_rec),
                  mean_pass_block = ifelse(is.na(mean_pass_block), 42.5, mean_pass_block),
                  sd_pass_block = ifelse(is.na(sd_pass_block), 12.5, sd_pass_block),
                  mean_run_block = ifelse(is.na(mean_run_block), 42.5, mean_run_block),
                  sd_run_block = ifelse(is.na(sd_run_block), 12.5, sd_run_block)
    )

  oline <- oline |>
    dplyr::mutate(mean_pass_block = ifelse(is.na(mean_pass_block), 42.5, mean_pass_block),
                  sd_pass_block = ifelse(is.na(sd_pass_block), 12.5, sd_pass_block),
                  mean_run_block = ifelse(is.na(mean_run_block), 42.5, mean_run_block),
                  sd_run_block = ifelse(is.na(sd_run_block), 12.5, sd_run_block)
    )

  frontseven <- frontseven |>
    dplyr::mutate(mean_pass_rush_defense = ifelse(is.na(mean_pass_rush_defense), 42.5, mean_pass_rush_defense),
                  sd_pass_rush_defense = ifelse(is.na(sd_pass_rush_defense), 12.5, sd_pass_rush_defense),
                  mean_coverage_defense = ifelse(is.na(mean_coverage_defense), 42.5, mean_coverage_defense),
                  sd_coverage_defense = ifelse(is.na(sd_coverage_defense), 12.5, sd_coverage_defense),
                  mean_run_defense = ifelse(is.na(mean_run_defense), 42.5, mean_run_defense),
                  sd_run_defense = ifelse(is.na(sd_run_defense), 12.5, sd_run_defense)
    )

  defensivebacks <- defensivebacks |>
    dplyr::mutate(mean_pass_rush_defense = ifelse(is.na(mean_pass_rush_defense), 42.5, mean_pass_rush_defense),
                  sd_pass_rush_defense = ifelse(is.na(sd_pass_rush_defense), 12.5, sd_pass_rush_defense),
                  mean_coverage_defense = ifelse(is.na(mean_coverage_defense), 42.5, mean_coverage_defense),
                  sd_coverage_defense = ifelse(is.na(sd_coverage_defense), 12.5, sd_coverage_defense),
                  mean_run_defense = ifelse(is.na(mean_run_defense), 42.5, mean_run_defense),
                  sd_run_defense = ifelse(is.na(sd_run_defense), 12.5, sd_run_defense)
    )

  allpositions <- list(quarterback, runningbacks, widereceivers, tightends,
                       oline, defensivebacks, frontseven)
  finaldf <- suppressMessages(reduce(allpositions, dplyr::full_join))
  finaldf$pff_id <- as.character(finaldf$pff_id)
  colnames(finaldf)[colnames(finaldf) == "Year"] <- "year"
  colnames(finaldf)[colnames(finaldf) == "Age"] <- "age"
  dplyr::left_join(dpt_cht, finaldf) |>
    suppressMessages() |>
    dplyr::filter(!is.na(player))
}

PFF_grade_formulator <- function(offtm, deftm, year) {
  offPFF <- full_PFF(offtm, year)
  defPFF <- full_PFF(deftm, year)

  passer <- offPFF |> dplyr::filter(final_position == "QB1")
  rushers <- offPFF |> dplyr::filter(position == "HB")
  receivers <- offPFF |> dplyr::filter(position %in% c("WR", "TE"))
  oline <- offPFF |> dplyr::filter(position == "OL")
  passrush <- defPFF |> dplyr::filter(position == "F7")
  coverage <- defPFF |> dplyr::filter(position == "DB")
  rundef <- defPFF |> dplyr::filter(position %in% c("F7", "DB"))

  pass_grade_mean <- passer$mean_pass
  rush_grade_mean <- mean(rushers$mean_rush, na.rm = TRUE)
  rec_grade_mean <- mean(receivers$mean_rec, na.rm = TRUE)
  oline_grade_mean <- (mean(oline$mean_pass_block, na.rm = TRUE)
                       + mean(oline$mean_run_block, na.rm = TRUE)) / 2
  def_mean <- (mean(passrush$mean_pass_rush_defense, na.rm = TRUE) +
                 mean(coverage$mean_coverage_defense, na.rm = TRUE) +
                 mean(rundef$mean_run_defense, na.rm = TRUE)) / 3

  ((((2 * pass_grade_mean) + (1.75 * oline_grade_mean) + (1.25 * rec_grade_mean) +
      rush_grade_mean
  ) / 6) - def_mean) * 7
}


# Simulator Functions -----------------------------------------------------

loading_ext_data <- function() {

  path <- system.file("extdata", package = "NFLSimulateR")

  files <- list.files(path, pattern = "\\.rds$", full.names = TRUE)

  data_env <- new.env()

  for (f in files) {

    obj <- readRDS(f)

    name <- tools::file_path_sans_ext(basename(f))

    data_env[[name]] <- obj
  }

  data_env
}

# .nfl_env <- new.env()
#
# data_env <- loading_ext_data()

#
# # ### Testing Variables
# posstm <- "DEN"
# deftm <- "SEA"
# down <- 2
# togo <- 3
# YdsBef <- 63
# posstmdiff <- 0
# quarter_secs <- 800
# quarter <- 1
# year <- 2025
# # base_PFF_grade1 <-  PFF_grade_formulator(posstm, deftm, year)
# personnel <- "11"
# def_personnel <- "Nickel"
# formation <- "EMPTY"
# coverage <- "Cover_3"
# runorpass <- "Pass"
# ###



#' Choose Offensive Personnel
#'
#' Predicts the offensive personnel grouping for a simulated play based on the
#' current game situation.
#'
#' @param down Integer. Current down (\code{1}--\code{4}).
#' @param togo Numeric. Yards needed to gain a first down.
#' @param YdsBef Numeric. Yards from the line of scrimmage to the opponent's
#'   end zone before the play.
#' @param posstmdiff Numeric. Score differential from the perspective of the
#'   team in possession (positive if leading, negative if trailing).
#' @param quarter_secs Numeric. Seconds remaining in the current quarter.
#' @param quarter Integer. Current quarter (\code{1}--\code{4}, with
#'   overtime represented by \code{5} if applicable).
#'
#' @return A character string representing the selected offensive personnel
#' grouping. Possible values are \code{"01"}, \code{"10"}, \code{"11"},
#' \code{"12"}, \code{"13"}, \code{"21"}, and \code{"22"}.
#'
#' @details
#' Personnel groupings follow standard NFL notation, where the first digit
#' denotes the number of running backs and the second digit denotes the number
#' of tight ends. The remaining eligible players are wide receivers. For
#' example, \code{"11"} represents one running back, one tight end, and up to
#' three wide receivers.
#'
#' The returned personnel grouping is sampled from the predicted probability
#' distribution produced by an XGBoost model trained on historical NFL
#' play-by-play data. Predictions are conditioned on the current game state,
#' including down, distance, field position, score differential, quarter, and
#' time remaining in the quarter.
#'
#' @export
choose_personnel <- function(down, togo, YdsBef, posstmdiff, quarter_secs,
                             quarter){
  data_env <- get_data_env()

  modeldat <- data.frame(
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    YdstoEZBef = YdsBef,
    down = down,
    yardsToGo = togo
  )

  X <- model.matrix(
    ~ quarter + down + yardsToGo +
      YdstoEZBef + PossTmDiff +
      QuarterSeconds - 1,
    data = modeldat
  )

  dnew <- xgboost::xgb.DMatrix(data = X)

  probs <- predict(data_env$off_pers_mod, dnew)
  personnels <- sort(unique(data_env$offdf$simple_personnel))
  sample(personnels, size = 1, prob = probs)
}

# choose_personnel(down, togo, YdsBef, posstmdiff, quarter_secs,
#                  quarter)



#' Choose Defensive Personnel
#'
#' Predicts the defensive personnel grouping for a simulated play based on the
#' current game situation and the offensive personnel on the field.
#'
#' @param down Integer. Current down (\code{1}--\code{4}).
#' @param togo Numeric. Yards needed to gain a first down.
#' @param YdsBef Numeric. Yards from the line of scrimmage to the opponent's
#'   end zone before the play.
#' @param posstmdiff Numeric. Score differential from the perspective of the
#'   team in possession (positive if leading, negative if trailing).
#' @param quarter_secs Numeric. Seconds remaining in the current quarter.
#' @param quarter Integer. Current quarter (\code{1}--\code{4}, with
#'   overtime represented by \code{5} if applicable).
#' @param personnel Character string. Offensive personnel grouping returned by
#'   \code{\link{choose_personnel}}.
#'
#' @return A character string representing the selected defensive personnel
#' grouping. Possible values are \code{"2-5"}, \code{"3-4"}, \code{"4-3"},
#' \code{"5-2"}, \code{"Nickel"}, \code{"Dime"}, and \code{"Heavy"}.
#'
#' @details
#' Defensive personnel groupings correspond to common NFL defensive packages.
#' Base fronts (e.g., \code{"4-3"} and \code{"3-4"}) indicate the number of
#' defensive linemen and linebackers, while sub-packages such as
#' \code{"Nickel"} and \code{"Dime"} replace linebackers with additional
#' defensive backs to better defend against the pass.
#'
#' The returned personnel grouping is sampled from the predicted probability
#' distribution produced by an XGBoost model trained on historical NFL
#' play-by-play data. Predictions are conditioned on the current game state and
#' the offensive personnel on the field.
#'
#' @export
choose_def_personnel <- function(down, togo, YdsBef, posstmdiff, quarter_secs,
                                 quarter, personnel){
  data_env <- get_data_env()

  modeldat <- data.frame(
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    YdstoEZBef = YdsBef,
    down = down,
    yardsToGo = togo
  )

  modeldat$simple_personnel <- factor(
    personnel,
    levels = sort(unique(data_env$offdf$simple_personnel))
  )

  X <- model.matrix(
    ~ quarter + down + yardsToGo +
      simple_personnel +
      YdstoEZBef + PossTmDiff +
      QuarterSeconds - 1,
    data = modeldat
  )

  dnew <- xgboost::xgb.DMatrix(data = X)

  probs <- predict(data_env$def_pers_mod, dnew)
  def_personnels <- sort(unique(data_env$offdf$simple_def_personnel))
  sample(def_personnels, size = 1, prob = probs)
}

# choose_def_personnel(down, togo, YdsBef, posstmdiff, quarter_secs,
#                  quarter, personnel = "11")



#' Choose Offensive Formation
#'
#' Predicts the offensive formation for a simulated play based on the current
#' game situation and the offensive and defensive personnel on the field.
#'
#' @param down Integer. Current down (\code{1}--\code{4}).
#' @param togo Numeric. Yards needed to gain a first down.
#' @param YdsBef Numeric. Yards from the line of scrimmage to the opponent's
#'   end zone before the play.
#' @param posstmdiff Numeric. Score differential from the perspective of the
#'   team in possession (positive if leading, negative if trailing).
#' @param quarter_secs Numeric. Seconds remaining in the current quarter.
#' @param quarter Integer. Current quarter (\code{1}--\code{4}, with
#'   overtime represented by \code{5} if applicable).
#' @param personnel Character string. Offensive personnel grouping returned by
#'   \code{\link{choose_personnel}}.
#' @param def_personnel Character string. Defensive personnel grouping returned
#'   by \code{\link{choose_def_personnel}}.
#'
#' @return A character string representing the selected offensive formation.
#' Possible values are \code{"EMPTY"}, \code{"I_FORM"},
#' \code{"PISTOL"}, \code{"SHOTGUN"}, and \code{"SINGLEBACK"}.
#'
#' @details
#' Offensive formations describe the alignment of the offensive backfield and
#' eligible receivers before the snap. For example, \code{"SHOTGUN"} places the
#' quarterback several yards behind the center, \code{"PISTOL"} aligns the
#' quarterback closer to the line of scrimmage with a running back directly
#' behind, \code{"I_FORM"} places the quarterback under center with two
#' running backs aligned in an "I" formation, \code{"SINGLEBACK"} places a
#' single running back behind the quarterback under center, and
#' \code{"EMPTY"} aligns no running backs in the backfield.
#'
#' The returned formation is sampled from the predicted probability
#' distribution produced by an XGBoost model trained on historical NFL
#' play-by-play data. Predictions are conditioned on the current game state,
#' offensive personnel, and defensive personnel.
#'
#' @export
choose_formation <- function(down, togo, YdsBef, posstmdiff, quarter_secs,
                             quarter, personnel, def_personnel){
  data_env <- get_data_env()

  modeldat <- data.frame(
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    YdstoEZBef = YdsBef,
    down = down,
    yardsToGo = togo
  )

  modeldat$simple_personnel <- factor(
    personnel,
    levels = sort(unique(data_env$offdf$simple_personnel))
  )

  modeldat$simple_def_personnel <- factor(
    def_personnel,
    levels = sort(unique(data_env$offdf$simple_def_personnel))
  )

  X <- model.matrix(
    ~ quarter + down + yardsToGo +
      simple_personnel +
      simple_def_personnel +
      YdstoEZBef + PossTmDiff +
      QuarterSeconds - 1,
    data = modeldat
  )

  dnew <- xgboost::xgb.DMatrix(data = X)

  probs <- predict(data_env$off_form_mod, dnew)
  off_forms <- sort(unique(data_env$offdf$off_form))
  sample(off_forms, size = 1, prob = probs)
}

# choose_formation(down, togo, YdsBef, posstmdiff, quarter_secs,
#                      quarter, personnel = "11", def_personnel = "Nickel")



#' Choose Defensive Coverage
#'
#' Predicts the defensive coverage scheme for a simulated play based on the
#' current game situation and the offensive and defensive alignments.
#'
#' @param down Integer. Current down (\code{1}--\code{4}).
#' @param togo Numeric. Yards needed to gain a first down.
#' @param YdsBef Numeric. Yards from the line of scrimmage to the opponent's
#'   end zone before the play.
#' @param posstmdiff Numeric. Score differential from the perspective of the
#'   team in possession (positive if leading, negative if trailing).
#' @param quarter_secs Numeric. Seconds remaining in the current quarter.
#' @param quarter Integer. Current quarter (\code{1}--\code{4}, with
#'   overtime represented by \code{5} if applicable).
#' @param personnel Character string. Offensive personnel grouping returned by
#'   \code{\link{choose_personnel}}.
#' @param def_personnel Character string. Defensive personnel grouping returned
#'   by \code{\link{choose_def_personnel}}.
#' @param formation Character string. Offensive formation returned by
#'   \code{\link{choose_formation}}.
#'
#' @return A character string representing the selected defensive coverage
#' scheme. Possible values are \code{"Cover_0"}, \code{"Cover_1"},
#' \code{"Cover_2"}, \code{"Cover_3"}, \code{"Cover_4"},
#' \code{"Cover_6"}, \code{"Cover2Man"}, and \code{"RedZone"}.
#'
#' @details
#' Defensive coverage schemes describe how defenders are assigned to cover
#' eligible receivers after the snap. Zone coverages such as
#' \code{"Cover_2"}, \code{"Cover_3"}, \code{"Cover_4"}, and
#' \code{"Cover_6"} divide the field into coverage responsibilities, whereas
#' \code{"Cover_0"}, \code{"Cover_1"}, and \code{"Cover2Man"} rely primarily
#' on man-to-man coverage. The \code{"RedZone"} coverage represents defensive
#' coverages commonly used near the goal line.
#'
#' The returned coverage scheme is sampled from the predicted probability
#' distribution produced by an XGBoost model trained on historical NFL
#' play-by-play data. Predictions are conditioned on the current game state,
#' offensive personnel, defensive personnel, and offensive formation.
#'
#' @export
choose_def_coverage <- function(down, togo, YdsBef, posstmdiff, quarter_secs,
                                quarter, personnel, def_personnel, formation){
  data_env <- get_data_env()

  modeldat <- data.frame(
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    down = down,
    yardsToGo = togo,
    YdstoEZBef = YdsBef
  )

  modeldat$simple_personnel <- factor(
    personnel,
    levels = sort(unique(data_env$offdf$simple_personnel))
  )

  modeldat$simple_def_personnel <- factor(
    def_personnel,
    levels = sort(unique(data_env$offdf$simple_def_personnel))
  )

  modeldat$off_form <- factor(
    formation,
    levels = sort(unique(data_env$offdf$off_form))
  )

  X <- model.matrix(
    ~ quarter + down + yardsToGo +
      simple_personnel +
      simple_def_personnel +
      off_form +
      YdstoEZBef + PossTmDiff +
      QuarterSeconds - 1,
    data = modeldat
  )

  dnew <- xgboost::xgb.DMatrix(data = X)

  probs <- predict(data_env$def_cov_mod, dnew)
  def_covs <- sort(unique(data_env$offdf$def_simple_coverage))
  sample(def_covs, size = 1, prob = probs)
}

# choose_def_coverage(down, togo, YdsBef, posstmdiff, quarter_secs,
#                  quarter, personnel = "11", def_personnel = "Nickel",
#                  formation = "SHOTGUN")



#' Choose Run or Pass
#'
#' Predicts whether the offense will call a running or passing play based on
#' the current game situation and the offensive and defensive alignments.
#'
#' @param down Integer. Current down (\code{1}--\code{4}).
#' @param togo Numeric. Yards needed to gain a first down.
#' @param YdsBef Numeric. Yards from the line of scrimmage to the opponent's
#'   end zone before the play.
#' @param posstmdiff Numeric. Score differential from the perspective of the
#'   team in possession (positive if leading, negative if trailing).
#' @param quarter_secs Numeric. Seconds remaining in the current quarter.
#' @param quarter Integer. Current quarter (\code{1}--\code{4}, with
#'   overtime represented by \code{5} if applicable).
#' @param personnel Character string. Offensive personnel grouping returned by
#'   \code{\link{choose_personnel}}.
#' @param def_personnel Character string. Defensive personnel grouping returned
#'   by \code{\link{choose_def_personnel}}.
#' @param formation Character string. Offensive formation returned by
#'   \code{\link{choose_formation}}.
#' @param coverage Character string. Defensive coverage scheme returned by
#'   \code{\link{choose_def_coverage}}.
#'
#' @return A character string representing the selected play type:
#' \code{"Run"} or \code{"Pass"}.
#'
#' @details
#' The returned play type is sampled from the predicted probability
#' distribution produced by an XGBoost model trained on historical NFL
#' play-by-play data. Predictions are conditioned on the current game state,
#' offensive personnel, defensive personnel, offensive formation, and
#' defensive coverage.
#'
#' @export
runorpass <- function(down, togo, YdsBef, posstmdiff, quarter_secs,
                      quarter, personnel, formation, def_personnel, coverage){
  data_env <- get_data_env()

  modeldat <- data.frame(
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    down = down,
    yardsToGo = togo,
    YdstoEZBef = YdsBef
  )

  modeldat$simple_personnel <- factor(
    personnel,
    levels = sort(unique(data_env$offdf$simple_personnel))
  )

  modeldat$simple_def_personnel <- factor(
    def_personnel,
    levels = sort(unique(data_env$offdf$simple_def_personnel))
  )

  modeldat$off_form <- factor(
    formation,
    levels = sort(unique(data_env$offdf$off_form))
  )

  modeldat$def_simple_coverage <- factor(
    coverage,
    levels = sort(unique(data_env$offdf$def_simple_coverage))
  )

  X <- model.matrix(
    ~ quarter + down + yardsToGo +
      simple_personnel +
      simple_def_personnel +
      off_form +
      YdstoEZBef + PossTmDiff +
      QuarterSeconds - 1,
    data = modeldat
  )

  dnew <- xgboost::xgb.DMatrix(data = X)

  passprob <- predict(data_env$run_pass_mod, dnew)
  probs = c(passprob, 1 - passprob)
  labels <- c("Pass", "Run")
  sample(labels, size = 1, prob = probs)
}

# runorpass(down, togo, YdsBef, posstmdiff, quarter_secs,
#                     quarter, personnel = "11", def_personnel = "Nickel",
#                     formation = "SHOTGUN", coverage = "Cover_3")


# Route Selection Function ------------------------------------------------

# <- <- full_PFF(posstm, 2025)
# def_dat <- full_PFF(deftm, 2025)

routeselection <- function(posstm, deftm, down, togo, YdsBef, posstmdiff,
                           quarter_secs, quarter, off_dat, def_dat, year
                           ){
  data_env <- get_data_env()

  whole_offense <- off_dat |>
    dplyr::filter(!is.na(games))
  whole_defense <- def_dat |>
    dplyr::filter(!is.na(games))

  personnel <- choose_personnel(down, togo, YdsBef, posstmdiff,
                                quarter_secs, quarter)

  def_personnel <- choose_def_personnel(down, togo, YdsBef, posstmdiff,
                                        quarter_secs, quarter, personnel)

  formation <- choose_formation(down, togo, YdsBef, posstmdiff,
                                quarter_secs, quarter, personnel, def_personnel)

  coverage <- choose_def_coverage(down, togo, YdsBef, posstmdiff, quarter_secs,
                                  quarter, personnel, def_personnel, formation)

  runpassselection <- runorpass(down, togo, YdsBef, posstmdiff, quarter_secs,
                                quarter, personnel, formation, def_personnel,
                                coverage)

  if(personnel %in% c("03", "02", "01", "00")){
    runpassselection <- "Pass"
  }
  if(personnel=="13"){
    personnel <- "12"
  }

  if(runpassselection=="Pass"){
    ### SETS UP DATAFRAME FOR MODEL USAGE LATER
    modeldat <- data.frame(
      QuarterSeconds = quarter_secs,
      quarter = quarter,
      PossTmDiff = posstmdiff,
      down = down,
      yardsToGo = togo,
      YdstoEZBef = YdsBef
    )

    modeldat$simple_personnel <- factor(
      personnel,
      levels = sort(unique(data_env$offdf$simple_personnel))
    )

    modeldat$simple_def_personnel <- factor(
      def_personnel,
      levels = sort(unique(data_env$offdf$simple_def_personnel))
    )

    modeldat$off_form <- factor(
      formation,
      levels = sort(unique(data_env$offdf$off_form))
    )

    modeldat$def_simple_coverage <- factor(
      coverage,
      levels = sort(unique(data_env$offdf$def_simple_coverage))
    )

    X <- model.matrix(
      ~ quarter + down + yardsToGo +
        simple_personnel +
        simple_def_personnel +
        off_form +
        def_simple_coverage +
        YdstoEZBef + PossTmDiff +
        QuarterSeconds - 1,
      data = modeldat
    )

    dnew <- xgboost::xgb.DMatrix(data = X)

    ### FIGURES OUT NUMBER OF COVER PLAYERS AND PASS RUSHERS
    probs <- predict(data_env$cov_players_mod, dnew)
    def_cov_players <- 4:8
    coverplayers <- sample(def_cov_players, size = 1, prob = probs)
    passrushers <- 11 - coverplayers

    X <- model.matrix(
      ~ quarter + down + yardsToGo +
        simple_personnel +
        simple_def_personnel +
        off_form +
        YdstoEZBef + PossTmDiff +
        QuarterSeconds - 1,
      data = modeldat
    )

    dnew <- xgboost::xgb.DMatrix(data = X)

    ### FIGURES OUT THE NUMBER OF ROUTES RAN
    probs <- predict(data_env$routes_count_mod, dnew)
    routes_ran_nums <- 2:5
    routescount <- sample(routes_ran_nums, size = 1, prob = probs)
    passrushers <- 11 - coverplayers

    num_RB <- as.numeric(substr(personnel, 1, 1))
    num_TE <- as.numeric(substr(personnel, 2, 2))

    ### GETS ROUTE COMBINATION

    X <- cbind(X, routescount)

    dnew <- xgboost::xgb.DMatrix(data = X)

    route_combo_probs <- predict(data_env$route_combo_mod, dnew)
    route_combo <- sample(1:20, size = 1, prob = route_combo_probs)

    modeldat$route_combos <- route_combo


    #### GETS SPECIFIC OFFENSIVE AND DEFENSIVE PLAYERS ON THE FIELD
    mydf <- tibble::tibble(
      RB = as.numeric(substr(personnel, 1, 1)),
      TE = as.numeric(substr(personnel, 2, 2)),
      WR = routescount - (RB + TE)
    )


    # depth chart for offensive team
    off_depth_chart <- off_dat
    off_depth_chart <- off_depth_chart[!grepl("( O| IR| D)$", off_depth_chart$player), ]
    off_by_pos <- split(off_depth_chart, off_depth_chart$position)

    # Quarterbacks
    onfieldQB <- off_depth_chart$pff_id[off_depth_chart$final_position=="QB1"]

    # Runningbacks
    runningbacks <- off_by_pos$HB
    rbprobs <- runningbacks$mean_rush_attempts / sum(runningbacks$mean_rush_attempts)
    onfieldRBS <- sample(runningbacks$pff_id, mydf$RB, prob = rbprobs)

    # Wide Receivers
    widereceivers <- off_by_pos$WR
    wrprobs <-  widereceivers$mean_routes / sum(widereceivers$mean_routes)
    onfieldWRS <- sample(widereceivers$pff_id, mydf$WR, prob = wrprobs)

    # Tight Ends
    tightends <- off_by_pos$TE
    teprobs <- tightends$mean_routes / sum(tightends$mean_routes)
    onfieldTEs <- sample(tightends$pff_id, mydf$TE, prob = teprobs)

    # Backup OLine
    backupOL <- off_depth_chart |>
      dplyr::filter(final_position %in% c("LT2", "LG2", "C2", "RG2", "RT2",
                                   "LT3", "LG3", "C3", "RG3", "RT3"),
             !is.na(mean_off_snaps))
    backupOLprobs <- backupOL$mean_off_snaps / sum(backupOL$mean_off_snaps)

    # On field OL
    onfieldOL <- off_depth_chart$pff_id[off_depth_chart$final_position %in%
                                          c("LT1", "LG1", "C1", "RG1", "RT1")]
    onfieldOLcount <- 10 - sum(mydf[1,])
    if(onfieldOLcount >5){
      onfieldOL <- c(onfieldOL, sample(backupOL$pff_id, onfieldOLcount-5, prob = backupOLprobs))
    }

    ### FIGURING OUT DEFENSE ON FIELD PLAYERS
    if(def_personnel=="Nickel"){
      frontsevenamount <- 6
    } else if(def_personnel=="Heavy"){
      frontsevenamount <- 8
    } else if(def_personnel=="Goalline"){
      frontsevenamount <- 9
    } else if (def_personnel=="Dime"){
      frontsevenamount <- 5
    } else if (def_personnel=="Quarter"){
      frontsevenamount <- 4
    } else{
      frontsevenamount <- sum(as.numeric(str_split_fixed(def_personnel, "-", 2)))
    }

    secondarycount <- 11 - frontsevenamount

    if(coverplayers > secondarycount){
      frontsevencoverplayers <- coverplayers - secondarycount
      secondaryrushers <- 0
    } else{
      frontsevencoverplayers <- 0
      secondaryrushers <- secondarycount - coverplayers
    }

    def_depth_chart <- def_dat
    def_depth_chart <- def_depth_chart[!grepl("( O| IR| D)$", def_depth_chart$player), ]
    def_by_pos <- split(def_depth_chart, def_depth_chart$position)

    f7_players <- def_by_pos$F7

    on_field_F7 <- sample(f7_players$pff_id, frontsevenamount,
                        prob = f7_players$mean_def_snaps / sum(f7_players$mean_def_snaps))

    onfieldF7 <- f7_players |>
      dplyr::filter(pff_id %in% on_field_F7)

    secondary_players <- def_by_pos$DB

    on_field_secondary <- sample(secondary_players$pff_id, secondarycount,
                                 prob = secondary_players$mean_def_snaps / sum(secondary_players$mean_def_snaps))

    onfieldsecondary <- secondary_players |>
      dplyr::filter(pff_id %in% on_field_secondary)

    if (frontsevencoverplayers > 0) {
      on_field_rushers <- sample(onfieldF7$pff_id, passrushers,
                                 prob = onfieldF7$mean_def_snaps / sum(onfieldF7$mean_def_snaps))
      on_field_coverage <- c(on_field_F7[(!on_field_F7 %in% on_field_rushers)],
                             on_field_secondary)
    } else if (secondaryrushers > 0) {
      on_field_coverage <- sample(onfieldsecondary$pff_id, coverplayers,
                                  prob = onfieldsecondary$mean_def_snaps / sum(onfieldsecondary$mean_def_snaps))
      on_field_rushers <- sample(on_field_secondary[(!on_field_secondary %in% on_field_coverage)],
                                 on_field_F7)
    } else {
      on_field_rushers <- on_field_F7
      on_field_coverage <- on_field_secondary
    }

    covertgtplyrcnt <- 1

    potentialtgtcovers <- def_depth_chart |> filter(pff_id %in% on_field_coverage)

    covertgtprobs <- potentialtgtcovers$mean_def_snaps / sum(potentialtgtcovers$mean_def_snaps)
    covertargetplayers <- sample(potentialtgtcovers$pff_id, covertgtplyrcnt, prob = covertgtprobs)
    noncovertgtplayers <- potentialtgtcovers |>
      dplyr::filter(!pff_id %in% covertargetplayers)

    modeldat$passrushers <- passrushers

    ttt_pred_dist <- posterior_predict(data_env$timeToThrowmodel, newdata = modeldat)
    timetoThrow <- round(apply(ttt_pred_dist, 2, sample, size = 1),3)
    modeldat$timeToThrow <- timetoThrow

    personneldf <- mydf

    X <- model.matrix(
      ~ quarter + down + yardsToGo +
        simple_personnel +
        simple_def_personnel +
        off_form +
        routescount +
        route_combos +
        YdstoEZBef + PossTmDiff +
        QuarterSeconds - 1,
      data = modeldat
    )

    dnew <- xgboost::xgb.DMatrix(data = X)

    tgtrouteprobs <- predict(data_env$targetted_route_mod, dnew)
    route_list <- c("ANGLE", "CORNER", "CROSS", "FLAT", "GO", "HITCH", "IN",
                    "OUT", "POST", "SCREEN", "SLANT", "WHEEL")

    targettedroute <- sample(route_list, 1, prob = tgtrouteprobs)

    modeldat$targettedroute <- factor(
      targettedroute,
      levels = c("ANGLE", "CORNER", "CROSS", "FLAT", "GO", "HITCH", "IN",
                 "OUT", "POST", "SCREEN", "SLANT", "WHEEL")
    )

    X <- model.matrix(
      ~ quarter + down + yardsToGo +
        simple_personnel +
        simple_def_personnel +
        off_form +
        routescount +
        route_combos +
        targettedroute +
        YdstoEZBef + PossTmDiff +
        QuarterSeconds - 1,
      data = modeldat
    )

    dnew <- xgboost::xgb.DMatrix(data = X)

    tgt_pos_probs <- predict(data_env$targetted_pos_mod, dnew)
    pos_list <- colnames(personneldf)

    targetted_pos <- sample(pos_list, 1, prob = tgt_pos_probs)

    if (targetted_pos == "WR") {
      options <- widereceivers |> dplyr::filter(pff_id %in% onfieldWRS)
      probs <- c(sqrt(options$total_snaps) / sum(sqrt(options$total_snaps)))
      players <- options$pff_id
      tgt_player <- sample(players, 1, prob = probs)
    } else if (targetted_pos == "TE") {
      options <- tightends |> dplyr::filter(pff_id %in% onfieldTEs)
      probs <- c(sqrt(options$total_snaps) / sum(sqrt(options$total_snaps)))
      players <- options$pff_id
      tgt_player <- sample(players, 1, prob = probs)
    } else {
      options <- runningbacks |> dplyr::filter(pff_id %in% onfieldRBS)
      probs <- c(sqrt(options$total_snaps) / sum(sqrt(options$total_snaps)))
      players <- options$pff_id
      tgt_player <- sample(players, 1, prob = probs)
    }


    list(pers = personnel, form = formation, runorpass = "Pass",
         def_pers = def_personnel, coverage = coverage,
         route_combo = route_combo,
         quarterback = onfieldQB,
         routesran = routescount,
         oline = onfieldOL,
         passrushers = on_field_rushers,
         coveringtgtplayer = covertargetplayers,
         othercoverplayers = noncovertgtplayers$pff_id,
         tgt_route = targettedroute,
         tgt_pos = targetted_pos, tgt_player = tgt_player,
         ttt = timetoThrow)

  } else if(runpassselection == "Run"){
    RBs <- as.numeric(substr(personnel, 1, 1))
    TEs <- as.numeric(substr(personnel, 2, 2))
    WRs <- 5 - RBs - TEs

    # QB
    onfieldQB <- whole_offense$pff_id[whole_offense$position=="QB"]

    # RBs
    runningbacks <- whole_offense |>
      dplyr::filter(position=="HB")
    rbprobs <- runningbacks$mean_rush_attempts / sum(runningbacks$mean_rush_attempts)
    onfieldRBS <- sample(runningbacks$pff_id, RBs, prob = rbprobs)

    # WRs
    widereceivers <- whole_offense |>
      dplyr::filter(position=="WR")
    wrprobs <- widereceivers$mean_routes / sum(widereceivers$mean_routes)
    onfieldWRS <- sample(widereceivers$pff_id, WRs, prob = wrprobs)

    # TEs
    tightends <- whole_offense |>
      dplyr::filter(position=="TE")
    teprobs <- tightends$mean_routes / sum(tightends$mean_routes)
    onfieldTES <- sample(tightends$pff_id, TEs, prob = teprobs)

    olinecount <- 10 - (RBs + WRs + TEs)
    onfieldOL <- whole_offense$pff_id[whole_offense$final_position %in%
                                        c("LT1", "LG1", "C1", "RG1", "RT1")]
    if(olinecount > 5){
      backupOL <- whole_offense |>
        dplyr::filter(position=="OL" & depth_next!="OL1")
      backupOLprobs <- backupOL$final_predicted_snap_ct / sum(backupOL$final_predicted_snap_ct)
      backupOL <- sample(backupOL$pff_id, (olinecount - 5), prob = backupOLprobs)
      onfieldOL <- c(onfieldOL, backupOL)
    }
    potential_rush_posdf <- whole_offense |>
      dplyr::filter(pff_id %in% c(onfieldRBS))

    rushposprobs <- potential_rush_posdf$mean_rush_attempts /
      sum(potential_rush_posdf$mean_rush_attempts)

    rushplayer <- sample(potential_rush_posdf$pff_id, 1, prob = rushposprobs)
    rushpos <- potential_rush_posdf$position[potential_rush_posdf$pff_id==rushplayer]

    rest_of_offense <- c(onfieldRBS, onfieldWRS, onfieldTES)
    rest_of_offense <- rest_of_offense[-which(rest_of_offense==rushplayer)]

    frontsevenamount <- suppressWarnings(case_when(
      def_personnel=="Nickel" ~ 6,
      def_personnel=="Heavy" ~ 8,
      def_personnel=="Goalline" ~ 9,
      def_personnel=="Dime" ~ 5,
      def_personnel=="Quarter" ~ 4,
      TRUE ~ sum(as.numeric(str_split_fixed(def_personnel, "-", 2)))
    ))

    f7df <- whole_defense |>
      dplyr::filter(position=="F7")

    f7probs <- f7df$mean_def_snaps / sum(f7df$mean_def_snaps)
    f7players <- sample(f7df$pff_id, frontsevenamount, prob = f7probs)

    secondaryamount <- 11 - frontsevenamount
    secondarydf <- whole_defense |>
      dplyr::filter(position=="DB")
    secondaryprobs <- secondarydf$mean_def_snaps / sum(secondarydf$mean_def_snaps)
    secondaryplayers <- sample(secondarydf$pff_id, secondaryamount, prob = secondaryprobs)

    list(pers = personnel, form = formation, runorpass = "Run",
         def_pers = def_personnel, coverage = coverage,
         rushpos = rushpos, rushplayer = rushplayer,
         quarterback = onfieldQB,
         rest_of_offense = rest_of_offense,
         oline = onfieldOL,
         defense = c(f7players, secondaryplayers))
  }
}


#' Generate Play Details
#'
#' Generates offensive and defensive personnel, alignments, player assignments,
#' and play-specific details for a simulated NFL play.
#'
#' @param posstm Character string. Team abbreviation for the team with
#'   possession of the ball.
#' @param deftm Character string. Team abbreviation for the defensive team.
#' @param down Integer. Current down (\code{1}--\code{4}).
#' @param togo Numeric. Yards needed to gain a first down.
#' @param YdsBef Numeric. Yards from the line of scrimmage to the opponent's
#'   end zone before the play.
#' @param posstmdiff Numeric. Score differential from the perspective of the
#'   team in possession (positive if leading, negative if trailing).
#' @param quarter_secs Numeric. Seconds remaining in the current quarter.
#' @param quarter Integer. Current quarter (\code{1}--\code{4}, with overtime
#'   represented by \code{5} if applicable).
#' @param off_dat A data.frame containing PFF player grades and usage metrics
#'   for the offensive team, such as the output of \code{\link{full_PFF}}.
#' @param def_dat A data.frame containing PFF player grades and usage metrics
#'   for the defensive team, such as the output of \code{\link{full_PFF}}.
#' @param year Numeric. Season year. Must be either \code{2024} or \code{2025}.
#'
#' @return A named list containing simulated play details. All outputs include:
#' \describe{
#'   \item{pers}{Selected offensive personnel grouping.}
#'   \item{form}{Selected offensive formation.}
#'   \item{runorpass}{Selected play type, either \code{"Run"} or \code{"Pass"}.}
#'   \item{def_pers}{Selected defensive personnel grouping.}
#'   \item{coverage}{Selected defensive coverage scheme.}
#' }
#'
#' If \code{runorpass = "Run"}, the list also contains:
#' \describe{
#'   \item{rushpos}{Position of the rushing player, usually \code{"HB"} or
#'   \code{"QB"}.}
#'   \item{rushplayer}{PFF identifier for the rushing player.}
#'   \item{quarterback}{PFF identifier for the quarterback on the field.}
#'   \item{rest_of_offense}{PFF identifiers for eligible offensive players who
#'   are not the rushing player, quarterback, or offensive linemen.}
#'   \item{oline}{PFF identifiers for offensive linemen on the field.}
#'   \item{defense}{PFF identifiers for defensive players on the field.}
#' }
#'
#' If \code{runorpass = "Pass"}, the list also contains:
#' \describe{
#'   \item{route_combo}{Route combination identifier selected by the route
#'   combination model.}
#'   \item{quarterback}{PFF identifier for the quarterback on the field.}
#'   \item{routesran}{Number of routes run on the play.}
#'   \item{oline}{PFF identifiers for offensive linemen on the field.}
#'   \item{passrushers}{PFF identifiers for defensive players rushing the
#'   passer.}
#'   \item{coveringtgtplayer}{PFF identifier for the defensive player covering
#'   the targeted receiver.}
#'   \item{othercoverplayers}{PFF identifiers for defensive players who are not
#'   rushing the passer or covering the targeted receiver.}
#'   \item{tgt_route}{Targeted route. Possible values include \code{"ANGLE"},
#'   \code{"CORNER"}, \code{"CROSS"}, \code{"FLAT"}, \code{"GO"},
#'   \code{"HITCH"}, \code{"IN"}, \code{"OUT"}, \code{"POST"},
#'   \code{"SCREEN"}, \code{"SLANT"}, and \code{"WHEEL"}.}
#'   \item{tgt_pos}{Position of the targeted player, such as \code{"RB"},
#'   \code{"WR"}, or \code{"TE"}.}
#'   \item{tgt_player}{PFF identifier for the targeted player.}
#' }
#'
#' @details
#' This function combines the simulator's sequential decision models to generate
#' the full set of play-level inputs needed for a simulated outcome. It first
#' samples personnel, formation, coverage, and play type, then assigns players
#' and play-specific roles based on whether the simulated play is a run or pass.
#'
#' @export
generate_play_details <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                               off_dat, def_dat, year){
  data_env <- get_data_env()

  saferoutefun <- safely(routeselection)
  out <- suppressWarnings(saferoutefun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                                       off_dat, def_dat, year))
  while(!is.null(out$error) | any(is.na(out$result))){
    out <- suppressWarnings(saferoutefun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                                         off_dat, def_dat, year))
  }
  out$result
}

# routeselection(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
#                    off_dat, def_dat, year = 2025)

# off_dat <- full_PFF("DEN", 2025)
# def_dat <- full_PFF("LV", 2025)
#
# routeselection("DEN", "LV", 3, 20, 70, 0, 800, 1,
#             off_dat, def_dat, 2025)

#'
# Yards Gained Function ---------------------------------------------------

yardsgained <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                        off_dat, def_dat, year){
  data_env <- get_data_env()

  output <- suppressWarnings(generate_play_details(posstm, deftm, down, togo,
                                                YdsBef, posstmdiff, quarter_secs, quarter,
                                                off_dat, def_dat, year))
  whole_offense <- off_dat
  whole_defense <- def_dat
  if(output$runorpass=="Pass"){
    if(output$coverage %in% c("Cover_1", "RedZone", "GoalLine", "Cover_0",
                              "Cover2Man", "Bracket")){
      manzone <- "Man"
    } else{
      manzone <- "Zone"
    }

    tgtroute <- output$tgt_route

    dummydf <- tibble::tibble(
      down = down,
      quarter = quarter,
      QuarterSeconds = quarter_secs,
      simple_personnel = output$pers,
      simple_def_personnel = output$def_pers,
      routescount = output$routesran,
      def_simple_coverage = output$coverage,
      off_form = output$form,
      PossTmDiff = posstmdiff,
      route_combos = output$route_combo,
      YdstoEZBef = YdsBef,
      yardsToGo = togo,
      timeToThrow = output$ttt,
      targettedroute = tgtroute,
      targettedposition = output$tgt_pos,
      passrushers = length(output$passrushers),
      inCoverage = 11 - passrushers
    )

    quarterback <- whole_offense |> dplyr::filter(pff_id == output$quarterback)
    pass_grade_mean <- quarterback$mean_pass
    pass_grade_sd <- quarterback$sd_pass
    pass_grade <- rnorm(1, mean = pass_grade_mean, sd = pass_grade_sd)

    oline <- whole_offense |> dplyr::filter(pff_id %in% output$oline)
    oline_grade_mean <- mean(oline$mean_pass_block)
    oline_grade_sd <- mean(oline$sd_pass_block) * 1.5
    oline_grade <- rnorm(1, mean = oline_grade_mean, sd = oline_grade_sd)


    receiver <- whole_offense |> dplyr::filter(pff_id == output$tgt_player)
    rec_grade_mean <- receiver$mean_rec
    rec_grade_sd <- receiver$sd_rec
    rec_grade <- rnorm(1, mean = rec_grade_mean, sd = rec_grade_sd)

    passrush <- whole_defense |> dplyr::filter(pff_id %in% output$passrushers)
    coverage <- whole_defense |> dplyr::filter(pff_id %in% output$othercoverplayers)
    tgt_coverage <- whole_defense |> dplyr::filter(pff_id %in% output$coveringtgtplayer)

    def_mean <- (mean(passrush$mean_pass_rush_defense, na.rm = TRUE) +
                   mean(coverage$mean_coverage_defense, na.rm = TRUE) +
                   (2 * tgt_coverage$mean_coverage_defense)) / 4

    def_sd <- (mean(passrush$sd_pass_rush_defense, na.rm = TRUE) +
                   mean(coverage$sd_coverage_defense, na.rm = TRUE) +
                   (tgt_coverage$sd_coverage_defense))

    def_grade <- rnorm(1, def_mean, def_sd)

    if (pass_grade < 30) pass_grade <- 30
    if (pass_grade > 95) pass_grade <- 95
    if (oline_grade < 30) oline_grade <- 30
    if (oline_grade > 95) oline_grade <- 95
    if (rec_grade < 30) rec_grade <- 30
    if (rec_grade > 95) rec_grade <- 95
    if (def_grade < 30) def_grade <- 30
    if (def_grade > 95) def_grade <- 95

    dummydf$pff_player_grade_component <-
      ((((2 * pass_grade) + (1.75 * oline_grade) + (2.25 * rec_grade)
      ) / 6) - def_grade) * 7


    dummydf$simple_personnel <- factor(
      dummydf$simple_personnel,
      levels = sort(unique(data_env$offdf$simple_personnel))
    )

    dummydf$simple_def_personnel <- factor(
      dummydf$simple_def_personnel,
      levels = sort(unique(data_env$offdf$simple_def_personnel))
    )

    dummydf$off_form <- factor(
      dummydf$off_form,
      levels = sort(unique(data_env$offdf$off_form))
    )

    dummydf$def_simple_coverage <- factor(
      dummydf$def_simple_coverage,
      levels = sort(unique(data_env$offdf$def_simple_coverage))
    )


    dummydf$targettedroute <- factor(
      dummydf$targettedroute,
      levels = sort(unique(data_env$offdf$targettedroute))
    )


    X <- model.matrix(
      ~ quarter + down + yardsToGo +
        simple_personnel +
        simple_def_personnel +
        off_form +
        def_simple_coverage +
        inCoverage +
        routescount +
        route_combos +
        passrushers +
        targettedroute +
        YdstoEZBef + PossTmDiff +
        QuarterSeconds - 1,
      data = dummydf
    )

    dnew <- xgboost::xgb.DMatrix(data = X)


    pressure_player_probs <- predict(data_env$pressure_players_mod, dnew)

    pressureplayers <- sample(c(0:2), 1, prob = pressure_player_probs)

    dummydf$pressureplayers <- pressureplayers

    dummydf$oline_blocking_grade_mean <- oline_grade_mean

    X <- model.matrix(
      ~ quarter + down + yardsToGo +
        simple_personnel +
        simple_def_personnel +
        def_simple_coverage +
        passrushers +
        timeToThrow +
        pressureplayers +
        off_form +
        YdstoEZBef + PossTmDiff +
        pff_player_grade_component +
        QuarterSeconds - 1,
      data = dummydf
    )

    dnew <- xgboost::xgb.DMatrix(data = X)

    sackprob <- predict(data_env$sack_mod, dnew)
    sack <- sample(c("Yes", "No"), 1, prob = c(sackprob, 1-sackprob))

    if(sack=="Yes"){
      random_yards <- round(apply(posterior_predict(
        data_env$sack_yards_model, newdata = dummydf), 2, sample, size = 1),0)
      list(runpass = "Pass", result = "Sack", yards = random_yards)
    } else{

      dummydf$targettedposition <- factor(
        dummydf$targettedposition,
        levels = sort(unique(data_env$offdf$targettedposition))
      )

      dummydf$pass_grade_mean <-  pass_grade_mean
      dummydf$receiving_grade_mean <-  rec_grade_mean
      dummydf$cover_target_route_grade_mean <- tgt_coverage$mean_coverage_defense

      X <- model.matrix(
        ~ quarter + down + yardsToGo +
          simple_personnel +
          simple_def_personnel +
          off_form +
          def_simple_coverage +
          routescount +
          route_combos +
          targettedroute +
          targettedposition +
          passrushers +
          timeToThrow +
          YdstoEZBef + PossTmDiff +
          pff_player_grade_component +
          QuarterSeconds - 1,
        data = dummydf
      )

      dnew <- xgboost::xgb.DMatrix(data = X)

      pass_result_options <- c("Complete", "Fumble", "Incomplete", "Interception")
      pass_result_probs <- predict(data_env$pass_result_mod, dnew)

      passresult <- sample(pass_result_options, 1, prob = pass_result_probs)

      if(passresult=="Complete"){
        pass_yards <- round(apply(posterior_predict(
          data_env$pass_yards_model, newdata = dummydf), 2, sample, size = 1),0)
        list(runpass = "Pass", result = "Complete", yards = pass_yards)
      } else if(passresult=="Interception"){
        pass_yards <- round(apply(posterior_predict(
          data_env$pass_yards_model, newdata = dummydf), 2, sample, size = 1),0)
        int_yards <- round(apply(posterior_predict(
          data_env$int_yards_model, newdata = dummydf), 2, sample, size = 1),0)
        list(runpass = "Pass", result = "Interception", yards = pass_yards - int_yards)
      } else if(passresult=="Incomplete"){
        list(runpass = "Pass", result = "Incomplete", yards = 0)
      } else{
        fumblepos <- sample(c("QB", output$tgt_pos), 1, prob = c(.6, .4))
        if(fumblepos=="QB"){
          fumbleprob <- data_env$fumblostdf$lostprob[data_env$fumblostdf$fumbleposition=="QB"]
          fumblost <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
          if(fumblost=="Yes"){
            fumbresult <- "Sack_Fumble_Retained"
          } else{
            fumbresult <- "Sack_Fumble_Lost"
          }
          fumbleyards <- -5
        } else{
          fumbleprob <- data_env$fumblostdf$lostprob[data_env$fumblostdf$fumbleposition==output$tgt_pos]
          fumblost <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
          if(fumblost=="Yes"){
            fumbresult <- "Complete_Fumble_Retained"
          } else{
            fumbresult <- "Complete_Fumble_Lost"
          }
          fumbleyards <- round(apply(posterior_predict(
            data_env$pass_yards_model, newdata = dummydf), 2, sample, size = 1),0)
        }
        list(runpass = "Pass", result = fumbresult, yards = fumbleyards)
      }
    }
  } else if(output$runorpass=="Run"){

    runningback <- whole_offense |> dplyr::filter(pff_id == output$rushplayer)
    rushing_mean <- runningback$mean_rush
    rushing_sd <- runningback$sd_rush
    rushing_grade <- rnorm(1, rushing_mean, rushing_sd)

    oline <- whole_offense |> dplyr::filter(pff_id %in% output$oline)
    oline_grade_mean <- mean(oline$mean_run_block)
    oline_grade_sd <- mean(oline$sd_run_block) * 1.5
    oline_grade <- rnorm(1, oline_grade_mean, oline_grade_sd)

    dummydf <- data.frame(
      down = down,
      simple_personnel = output$pers,
      simple_def_personnel = output$def_pers,
      def_simple_coverage = output$coverage,
      PossTmDiff = posstmdiff,
      off_form = output$form,
      YdstoEZBef = YdsBef,
      yardsToGo = togo,
      rush_grade_mean = rushing_grade,
      oline_blocking_grade_mean = oline_grade,
      QuarterSeconds = quarter_secs
    )

    rush_yards <- round(apply(posterior_predict(
      data_env$rush_yards_model, newdata = dummydf), 2, sample, size = 1),0)

    rushdf <- data_env$offdf
    rushdf <- rushdf |>
      dplyr::filter(simple_personnel != "01")

    dummydf$simple_personnel <- factor(
      dummydf$simple_personnel,
      levels = sort(unique(rushdf$simple_personnel))
    )

    dummydf$simple_def_personnel <- factor(
      dummydf$simple_def_personnel,
      levels = sort(unique(data_env$offdf$simple_def_personnel))
    )

    dummydf$off_form <- factor(
      dummydf$off_form,
      levels = sort(unique(data_env$offdf$off_form))
    )

    dummydf$def_simple_coverage <- factor(
      dummydf$def_simple_coverage,
      levels = sort(unique(data_env$offdf$def_simple_coverage))
    )

    X <- model.matrix(
      ~ quarter + down + yardsToGo +
        simple_personnel +
        simple_def_personnel +
        def_simple_coverage ++
        off_form +
        YdstoEZBef + PossTmDiff +
        rush_grade_mean +
        oline_blocking_grade_mean +
        QuarterSeconds - 1,
      data = dummydf
    )

    dnew <- xgboost::xgb.DMatrix(data = X)

    fumbleprob <- predict(data_env$rush_fumb_mod, dnew)
    fumble <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
    if(fumble=="No"){
      result <- "No_Fumble"
    } else{
      fldf <- data_env$fumblostdf |> dplyr::filter(fumbleposition=="RB")
      fumblelost <- sample(c("Yes", "No"), 1, prob = c(fldf$lostprob, 1-fldf$lostprob))
      if(fumblelost=="No"){
        result <- "Fumble_Retained"
      } else{
        result <- "Fumble_Lost"
      }
    }
    list(runpass = "Run", result = result, yards = rush_yards)
  }
}


#' Simulate Yards Gained
#'
#' Simulates the outcome of an offensive play and returns the resulting play
#' type, play result, and net yards gained.
#'
#' @param posstm Character string. Team abbreviation for the team with
#'   possession of the ball.
#' @param deftm Character string. Team abbreviation for the defensive team.
#' @param down Integer. Current down (\code{1}--\code{4}).
#' @param togo Numeric. Yards needed to gain a first down.
#' @param YdsBef Numeric. Yards from the line of scrimmage to the opponent's
#'   end zone before the play.
#' @param posstmdiff Numeric. Score differential from the perspective of the
#'   team in possession (positive if leading, negative if trailing).
#' @param quarter_secs Numeric. Seconds remaining in the current quarter.
#' @param quarter Integer. Current quarter (\code{1}--\code{4}, with overtime
#'   represented by \code{5} if applicable).
#' @param off_dat A data.frame containing PFF player grades and usage metrics
#'   for the offensive team, such as the output of \code{\link{full_PFF}}.
#' @param def_dat A data.frame containing PFF player grades and usage metrics
#'   for the defensive team, such as the output of \code{\link{full_PFF}}.
#' @param year Numeric. Season year. Must be either \code{2024} or
#'   \code{2025}.
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{runpass}{Play type, either \code{"Run"} or \code{"Pass"}.}
#'   \item{result}{The simulated play result.}
#'   \item{yards}{Net yards gained on the play. Negative values indicate a loss
#'   of yardage. On turnovers or fumbles, this value includes any return
#'   yardage.}
#' }
#'
#' Possible values of \code{result} are:
#' \itemize{
#'   \item Run plays: \code{"No_Fumble"} or \code{"Fumble"}.
#'   \item Pass plays: \code{"Complete"}, \code{"Incomplete"},
#'   \code{"Sack"}, \code{"Interception"},
#'   \code{"Sack_Fumble_Retained"},
#'   \code{"Sack_Fumble_Lost"},
#'   \code{"Complete_Fumble_Retained"}, or
#'   \code{"Complete_Fumble_Lost"}.
#' }
#'
#' @details
#' This function first generates play details using
#' \code{\link{generate_play_details}}, then sequentially simulates each stage
#' of the play (e.g., sack, completion, interception, fumble, and yards after
#' catch or contact) using statistical models trained on historical NFL
#' play-by-play data. The returned yardage represents the final net result of
#' the simulated play.
#'
#' @export
simulate_yards_gained <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                            off_dat, def_dat, year){
  data_env <- get_data_env()

  safeydsfun <- safely(yardsgained)
  out <- suppressWarnings(safeydsfun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                                     off_dat, def_dat, year))
  while(!is.null(out$error) | is.null(out$result) | any(is.na(out$result))){
    out <- suppressWarnings(safeydsfun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                                       off_dat, def_dat, year))
  }
  out$result
}


# yardsgained("DEN", "LV", 1, 10, 70, 0, 800, 1,
#             off_dat, def_dat, 2025)

# load_simulator_data <- function() {
#   .nfl_env$data_env <- loading_ext_data()
#   invisible(.nfl_env$data_env)
# }


#' Simulate an NFL Game
#'
#' Simulates a complete NFL game between two teams using statistical models
#' trained on historical NFL play-by-play and PFF player data. Each play is
#' generated sequentially by simulating personnel, formations, coverages,
#' play type, player assignments, and play outcomes.
#'
#' @param team1 Character string. A two- or three-letter NFL team abbreviation.
#'   Must be one of the following:
#'   \code{"ARI"}, \code{"ATL"}, \code{"BAL"}, \code{"BUF"}, \code{"CAR"},
#'   \code{"CHI"}, \code{"CIN"}, \code{"CLE"}, \code{"DAL"}, \code{"DEN"},
#'   \code{"DET"}, \code{"GB"}, \code{"HOU"}, \code{"IND"}, \code{"JAX"},
#'   \code{"KC"}, \code{"LA"}, \code{"LAC"}, \code{"LV"}, \code{"MIA"},
#'   \code{"MIN"}, \code{"NE"}, \code{"NO"}, \code{"NYG"}, \code{"NYJ"},
#'   \code{"PHI"}, \code{"PIT"}, \code{"SEA"}, \code{"SF"}, \code{"TB"},
#'   \code{"TEN"}, \code{"WAS"}.
#'
#' @param team2 Character string. A two- or three-letter NFL team abbreviation.
#'   Must be one of the following:
#'   \code{"ARI"}, \code{"ATL"}, \code{"BAL"}, \code{"BUF"}, \code{"CAR"},
#'   \code{"CHI"}, \code{"CIN"}, \code{"CLE"}, \code{"DAL"}, \code{"DEN"},
#'   \code{"DET"}, \code{"GB"}, \code{"HOU"}, \code{"IND"}, \code{"JAX"},
#'   \code{"KC"}, \code{"LA"}, \code{"LAC"}, \code{"LV"}, \code{"MIA"},
#'   \code{"MIN"}, \code{"NE"}, \code{"NO"}, \code{"NYG"}, \code{"NYJ"},
#'   \code{"PHI"}, \code{"PIT"}, \code{"SEA"}, \code{"SF"}, \code{"TB"},
#'   \code{"TEN"}, \code{"WAS"}.
#'   Must be different from \code{team1}.
#'
#' @param year Numeric. Season year. Must be an integer between
#'   \code{2022} and \code{2025}.
#'
#' @param week Numeric. NFL regular season week to use for the simulation.
#'   Must be an integer between \code{1} and \code{18}. The specified week
#'   determines the active rosters, depth charts, and player PFF grades used
#'   during the simulation.
#'
#' @param offseason Logical. Indicates if it is currently the NFL offseason.
#'
#' @param track Logical. If \code{TRUE}, the simulated play-by-play is printed
#'   to the console as the game progresses. If \code{FALSE} (default), no
#'   play-by-play output is displayed.
#'
#' @param create_data Logical. If \code{TRUE}, a play-by-play data frame
#'   containing information for every simulated play is returned along with the
#'   final score. If \code{FALSE} (default), only the final score is returned.
#'
#' @return If \code{create_data = FALSE}, returns a one-row data frame
#' containing the simulated final score. The column names correspond to the
#' two teams provided, and the values are the simulated final scores.
#'
#' If \code{create_data = TRUE}, returns a list containing:
#' \describe{
#'   \item{\code{score}}{A one-row data frame containing the simulated final score.}
#'   \item{\code{play_by_play}}{A data frame containing one row for each simulated play and the associated play information.}
#' }
#'
#' @details
#' The simulation proceeds one play at a time until the game is complete.
#' Offensive personnel, defensive personnel, formations, coverages, play type,
#' player assignments, and play outcomes are sampled from probability
#' distributions estimated from historical NFL data. Player-specific PFF grades
#' are incorporated throughout the simulation to produce realistic team and
#' player behavior. When requested, a complete play-by-play data set is
#' generated that can be used for downstream analysis or visualization.
#'
#' @export
simulate_game <- function(team1, team2, year = 2025, week = 1,
                          offseason = FALSE,
                          track = FALSE,
                          create_data = FALSE) {

  data_env <- get_data_env()

  play_count <- 1

  # track <- track
  play <- list(yards = 0, runpass = "Run", result = "Default")
  # Initialize all game state variables
  game_state <- list(
    quarter = 1,
    secondsleft = 3600,
    quartersecondsleft = 900,
    down = 1,
    togo = 10,
    ydsbef = 70,
    teams = c(team1, team2),
    scoredf = data.frame(x1 = 0, x2 = 0),
    posstm = NULL,
    deftm = NULL,
    posstmmargin = 0,
    playtime = NULL,
    afterplaytime = NULL,
    option = NULL,
    basePFFgrade = NULL
  )

  whole_team1 <- full_PFF(team1, year)
  whole_team2 <- full_PFF(team2, year)

  # basePFFgrade_team1 <- PFF_grade_formulator(team1, team2, year)
  # basePFFgrade_team2 <- PFF_grade_formulator(team2, team1, year)
  #
  # avg_basePFF_grade <- (abs(basePFFgrade_team1) + abs(basePFFgrade_team2)) / 2



  colnames(game_state$scoredf) <- c(team1, team2)
  game_state$posstm <- sample(game_state$teams, 1, prob = c(.5, .5))
  game_state$deftm <- game_state$teams[which(c(team1, team2) != game_state$posstm)]
  game_state$startoffteam <- game_state$posstm
  game_state$startdefteam <- game_state$deftm

  # if (game_state$posstm == team1) {
  #   game_state$basePFFgrade <- sign(basePFFgrade_team1) * avg_basePFF_grade
  # } else {
  #   game_state$basePFFgrade <- sign(basePFFgrade_team2) * avg_basePFF_grade
  # }


  # Helper functions (all take and return game_state)
  timeupdater <- function(game_state, playtime, afterplaytime) {
    game_state$secondsleft <- round(game_state$secondsleft - playtime - afterplaytime)
    game_state$quartersecondsleft <- round(game_state$quartersecondsleft - playtime - afterplaytime)
    return(game_state)
  }

  playtimecorrector <- function(game_state) {
    if(is.null(game_state$playtime) || game_state$playtime < 3) {
      game_state$playtime <- 3
    }
    return(game_state)
  }

  togocorrector <- function(game_state) {
    if(game_state$togo > game_state$ydsbef) {
      game_state$togo <- game_state$ydsbef
    }
    return(game_state)
  }

  posschange <- function(game_state) {
    temp <- game_state$posstm
    game_state$posstm <- game_state$deftm
    game_state$deftm <- temp
    # game_state$basePFFgrade <- -game_state$basePFFgrade

    return(game_state)
  }

  touchdown <- function(game_state) {
    if(game_state$posstmmargin %in% c(-1, -5, -8, -11, -15)) {
      mypoints <- sample(c(6,8), 1, prob = c(.49, .51))
    } else {
      mypoints <- sample(c(6,7), 1, prob = c(.025, .975))
    }
    return(mypoints)
  }

  afterplaytimegenerator <- function(game_state) {
    if(game_state$posstmmargin < 0 & game_state$secondsleft <= 240) {
      game_state$afterplaytime <- round(rnorm(1, 9.5, 1))
    } else {
      game_state$afterplaytime <- round(rnorm(1, 35, 2.5))
    }
    return(game_state)
  }

  posstmmargin_updater <- function(game_state) {
    game_state$posstmmargin <- as.numeric(game_state$scoredf[[game_state$posstm]] -
                                            game_state$scoredf[[game_state$deftm]])
    return(game_state)
  }

  quartercheck <- function(game_state) {
    if (game_state$quartersecondsleft <= 0) {
      game_state$quarter <- game_state$quarter + 1

      if (game_state$quarter == 5 && game_state$posstmmargin == 0) {
        game_state$quartersecondsleft <- 600
        game_state$secondsleft <- 600
      } else {
        game_state$quartersecondsleft <- 900
        game_state$secondsleft <- 3600 - ((game_state$quarter - 1) * 900)
      }

      if (game_state$quarter == 3) {
        game_state$posstm <- game_state$startdefteam
        game_state$deftm <- game_state$startoffteam
        game_state$down <- 1
        game_state$togo <- 10
        game_state$ydsbef <- 70
        game_state <- posstmmargin_updater(game_state)
      }
    }

    game_state
  }

  deftd <- function(game_state, playtimemean, playtimesd) {
    game_state$scoredf[[game_state$deftm]] <-
      as.numeric(game_state$scoredf[[game_state$deftm]]) + touchdown(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 70
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  touchback <- function(game_state, playtimemean, playtimesd) {
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 80
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  reg_turnover <- function(game_state, playtimemean, playtimesd){
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 100 - as.numeric(game_state$ydsbef - play$yards)
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  safety <- function(game_state, playtimemean, playtimesd){
    game_state$scoredf[[game_state$deftm]] <-
      as.numeric(game_state$scoredf[[game_state$deftm]]) + 2
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 65
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  offtd <- function(game_state, playtimemean, playtimesd){
    game_state$scoredf[[game_state$posstm]] <-
      as.numeric(game_state$scoredf[[game_state$posstm]]) + touchdown(game_state)
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 70
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  firstdown <- function(game_state, playtimemean, playtimesd){
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- game_state$ydsbef - play$yards
    game_state <- togocorrector(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state <- afterplaytimegenerator(game_state)
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  turnover_on_downs <- function(game_state, playtimemean, playtimesd){
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 100 - (game_state$ydsbef - play$yards)
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  regular_gain <- function(game_state, playtimemean, playtimesd){
    game_state$down <- game_state$down + 1
    game_state$togo <- game_state$togo - play$yards
    game_state$ydsbef <- game_state$ydsbef - play$yards
    game_state <- togocorrector(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state <- afterplaytimegenerator(game_state)
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  made_fg <- function(game_state){
    game_state$option <- "field goal attempt"
    game_state$scoredf[[game_state$posstm]] <-
      as.numeric(game_state$scoredf[[game_state$posstm]]) + 3
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 70
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, 5, 1))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  missed_fg <- function(game_state){
    game_state$option <- "field goal attempt"
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- (100-game_state$ydsbef) - 5
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, 5, 1))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  punt <- function(game_state){
    game_state$option <- "punt"
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- (100-game_state$ydsbef) + round(rnorm(1, 45, 5))
    ### TOUCHBACK
    if(game_state$ydsbef>=100){
      game_state$ydsbef <- 80
    }
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, 9, 1.5))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  game_state <- posstmmargin_updater(game_state)

  # Main game loop
  while(TRUE) {
    game_state$scoredf$Quarter <- game_state$quarter
    game_state$scoredf$SecondsLeft <- game_state$quartersecondsleft
    game_state$scoredf$Possession <- game_state$posstm
    game_state$scoredf$Down <- game_state$down
    game_state$scoredf$ToGo <- game_state$togo
    game_state$scoredf$YdstoEZ <- game_state$ydsbef

    if (game_state$quarter == 5 && game_state$posstmmargin != 0
        && is.null(game_state$ot_started)) {
      finalscoredf <- game_state$scoredf[7:8]
      break
    }

    ## OT implementation
    if (game_state$quarter == 5 && game_state$posstmmargin == 0 &&
        is.null(game_state$ot_started)) {
      game_state$posstm <- sample(game_state$teams, 1)
      game_state$deftm <- game_state$teams[game_state$teams != game_state$posstm]
      game_state$down <- 1
      game_state$togo <- 10
      game_state$ydsbef <- 70

      game_state$ot_started <- TRUE

      possessions_tracker <- c(game_state$posstm)
    }

    if (game_state$quarter == 5) {
      possessions_tracker <- unique(c(possessions_tracker, game_state$posstm))

      if (length(possessions_tracker) == 2 && game_state$posstmmargin != 0) {
        finalscoredf <- game_state$scoredf[7:8]
        break
      }
    }

    if (game_state$quarter == 6) {
      finalscoredf <- game_state$scoredf[7:8]
      break
    }
    ##

    if(game_state$posstm==team1){
      off_dat <- whole_team1
      def_dat <- whole_team2
    } else if(game_state$posstm==team2){
      off_dat <- whole_team2
      def_dat <- whole_team1
    }


    if(game_state$down==4){
      if(game_state$ydsbef>40){
        ### GO FOR IT ON FOURTH
        if(game_state$quartersecondsleft<=240 & game_state$posstmmargin<0
           & game_state$togo<=10){
          game_state$option <- "goforit"
        } else{ ### PUNT
          game_state <- punt(game_state)
        }
      } else{
        ### GO FOR IT ON FOURTH
        if((game_state$quarter %in% c(2,4) & game_state$quartersecondsleft<=240 & game_state$
            posstmmargin<0 & game_state$togo<=3) |
           (game_state$quarter %in% c(2,4) & game_state$quartersecondsleft<=120 &
            game_state$posstmmargin<0)){
          game_state$option <- "goforit"
        } else{ ### FIELD GOAL
          game_state$option <- "field goal attempt"
          fgmakedf <- data_env$fgmakedf
          fgpredvardf <- data.frame(
            FGDist = game_state$ydsbef + 17,
            Quarter = game_state$quarter,
            Time2 = game_state$quartersecondsleft,
            PossTmMargin = game_state$posstmmargin,
            Down = game_state$down,
            ToGo = game_state$togo
          )
          fgattprob <- .9 # temporary to fix later (model was too big)
          fgattselection <- sample(c("Yes", "No"), 1, prob = c(fgattprob, 1-fgattprob))
          ### FGATT
          if(fgattselection=="Yes"){
            game_state$option <- "field goal attempt"
            fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==game_state$ydsbef+17]
            fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
            ### MADE FIELD GOAL
            if(fgmake=="Yes"){
              game_state <- made_fg(game_state)
              ### MISSED FIELD GOAL
            } else{
              game_state <- missed_fg(game_state)
            }
            ### GO FOR IT ON FOURTH
          } else{
            game_state$option <- "goforit"
          }
        }
      }
    } else{
      game_state$option <- "regular"
    }
    ### OTHER field goal scenario
    if((game_state$secondsleft<=10 & game_state$posstmmargin>=-3 &
        game_state$posstmmargin<=0) |
       (game_state$quarter==2 & game_state$quartersecondsleft<=10)){
      game_state$option <- "field goal attempt"
      fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==game_state$ydsbef+17]
      fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
      ### MADE FIELD GOAL
      if(fgmake=="Yes"){
        game_state <- made_fg(game_state)
        ### MISSED FIELD GOAL
      } else{
        game_state <- missed_fg(game_state)
      }
    }

    if((game_state$down!=4 | game_state$option=="goforit") &
       game_state$option!="field goal attempt"){

      play <- simulate_yards_gained(game_state$posstm, game_state$deftm, game_state$down,
                              game_state$togo, game_state$ydsbef, game_state$posstmmargin,
                              game_state$quartersecondsleft, game_state$quarter,
                              off_dat = off_dat, def_dat = def_dat, year = year)
    }
    if(play$result=="Sack" & play$yards > 0){
      play$yards <- -5
    }
    if(play$yards < -100){
      play$yards <- -100
    }
    if(play$yards > 100){
      play$yards <- 100
    }
    newyardsbef <- game_state$ydsbef - play$yards
    ################ PASS
    if(play$runpass=="Pass"){
      # INTERCEPTION
      if(play$result=="Interception"){
        ### PICK SIX
        if(newyardsbef>=100){
          game_state <- deftd(game_state, 8.5, .85)
          ### INTERCEPTION TOUCHBACK
        } else if(newyardsbef <= 0){
          game_state <- touchback(game_state, 5.5, .55)
          ### REGULAR INTERCEPTION
        } else{
          game_state <- reg_turnover(game_state, 6.5, .65)
        }
        # FUMBLE
      } else if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained",
                                   "Sack_Fumble_Lost", "Complete_Fumble_Lost")){
        ## FUMBLE LOST
        if(play$result %in% c("Sack_Fumble_Lost", "Complete_Fumble_Lost")){
          ### DEF TD
          if(newyardsbef>=100){
            game_state <- deftd(game_state, 5, .5)
            ### TOUCHBACK
          } else if(newyardsbef<=0){
            game_state <- touchback(game_state, 5, .5)
            ### REGULAR TURNOVER
          } else{
            game_state <- reg_turnover(game_state, 5, .5)
          }
          ## FUMBLE RETAINED
        } else if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained")){
          ### SAFETY
          if(newyardsbef>=100){
            game_state <- safety(game_state, 5.5, .5)
            ### OFF TD
          } else if(newyardsbef<=0){
            game_state <- offtd(game_state, 7.5, .75)
            ### FIRST DOWN
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
            game_state <- firstdown(game_state, 7.5, .75)
            ### TURNOVER ON DOWNS
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                    & game_state$down==4){
            game_state <- turnover_on_downs(game_state, 6, .6)
            ### NON FIRST DOWN REGULAR PLAY
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                    & game_state$down!=4){
            game_state <- regular_gain(game_state, 6, .6)
          } else{
            print("FUMBLE RETAINED SCENARIO NOT CAPTURED")
          }
        }
        # SACK
      } else if(play$result=="Sack"){
        ### SAFETY
        if(newyardsbef >= 100){
          game_state <- safety(game_state, 4.5, .45)
          ### TURNOVER ON DOWNS
        } else if(game_state$down==4 & (newyardsbef < 100)){
          game_state <- turnover_on_downs(game_state, 4.5, .45)
          ### REGULAR SACK
        } else if(game_state$down!=4 & (newyardsbef < 100)){
          game_state <- regular_gain(game_state, 4.5, .45)
        } else{
          print("SACK SCENARIO NOT CAPTURED")
        }
        # COMPLETE PASSES
      } else if(play$result=="Complete"){
        ### SAFETY
        if(newyardsbef>=100){
          game_state <- safety(game_state, 5, .5)
          ### OFF TOUCHDOWN
        } else if(newyardsbef<=0){
          game_state <- offtd(game_state, 7.5, .75)
          ### FIRST DOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
          game_state <- firstdown(game_state, 7.5, .75)
          ### TURNOVER ON DOWNS
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                  & game_state$down==4){
          game_state <- turnover_on_downs(game_state, 6.5, .65)
          ### REGULAR NON FOURTH DOWN NO FIRST DOWN NO TOUCHDOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                  & game_state$down!=4){
          game_state <- regular_gain(game_state, 6.5, .65)
        } else{
          print("COMPLETE PASSES SCENARIO NOT CAPTURED")
        }
        # INCOMPLETE PASSES
      } else if(play$result=="Incomplete"){
        ### TURNOVER ON DOWNS
        if(game_state$down==4){
          game_state <- turnover_on_downs(game_state, 6, .6)
          ### REGULAR INCOMPLETION
        } else if(game_state$down!=4){
          game_state$down <- game_state$down + 1
          game_state$playtime <- round(rnorm(1, 6, .6))
          game_state <- playtimecorrector(game_state)
          game_state$afterplaytime <- 0
          game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
          game_state <- quartercheck(game_state)
        } else{
          print("INCOMPLETE PASSES SCENARIO NOT CAPTURED")
        }
      } else{
        print("PASS SCENARIO NOT CAPTURED")
      }
      ################ RUN
    } else if(play$runpass=="Run"){
      # FUMBLE
      if(play$result %in% c("Fumble_Retained", "Fumble_Lost")){
        ## FUMBLE LOST
        if(play$result=="Fumble_Lost"){
          ### DEF TD
          if(newyardsbef>=100){
            game_state <- deftd(game_state, 5, .5)
            ### TOUCHBACK
          } else if(newyardsbef<=0){
            game_state <- touchback(game_state, 5, .5)
            ### REGULAR TURNOVER
          } else{
            game_state <- reg_turnover(game_state, 5, .5)
          }
          ## FUMBLE RETAINED
        } else if(play$result=="Fumble_Retained"){
          ### SAFETY
          if(newyardsbef>=100){
            game_state <- safety(game_state, 4.5, .45)
            ### OFF TD
          } else if(newyardsbef<=0){
            game_state <- offtd(game_state, 7.5, .75)
            ### FIRST DOWN
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
            game_state <- firstdown(game_state, 6.5, .65)
            ### TURNOVER ON DOWNS
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                    & game_state$down==4){
            game_state <- turnover_on_downs(game_state, 4.5, .45)
            ### NON FIRST DOWN REGULAR PLAY
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                    & game_state$down!=4){
            game_state <- regular_gain(game_state, 5, .5)
          } else{
            print("RUN FUMBLE RETAINED SCENARIO NOT CAPTURED")
          }
        }
        # NON FUMBLE RUNS
      } else if(play$result=="No_Fumble"){
        ### SAFETY
        if(newyardsbef>=100){
          game_state <- safety(game_state, 4.5, .45)
          ### OFF TD
        } else if(newyardsbef<=0){
          game_state <- offtd(game_state, 7.5, .75)
          ### FIRST DOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
          game_state <- firstdown(game_state, 6, .6)
          ### TURNOVER ON DOWNS
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                  & game_state$down==4){
          game_state <- turnover_on_downs(game_state, 4.5, .45)
          ### NON FIRST DOWN REGULAR PLAY
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                  & game_state$down!=4){
          game_state <- regular_gain(game_state, 5, .5)
        } else{
          print("NON FUMBLE RUN SCENARIO NOT CAPTURED")
        }
      }
      else{
        print("RUN SCENARIO NOT CAPTURED")
      }
    }
    else{
      print("RUNPASS IS NOT A RUN OR PASS")
    }
    game_state$scoredf$Detail <- paste0(play$runpass, "; ", play$result, "; Yards: ", play$yards)
    game_state$scoredf <- game_state$scoredf |> relocate((Quarter:YdstoEZ), .before = all_of(team1))
    if (isTRUE(track)){
      print(game_state$scoredf)
    }
    if (isTRUE(create_data) && play_count == 1) {
      output_df <- game_state$scoredf[0, ]
      play_count <- play_count + 1
    }
    if (isTRUE(create_data)) {
      output_df <- rbind(output_df, game_state$scoredf)
    }

  }
  if (isTRUE(create_data)) {
    finalscoredf <- output_df
  }
  finalscoredf
}

# simulator("DEN", "SEA", year = 2025, track = "YES")


#' Simulate Multiple NFL Games
#'
#' Simulates the same NFL matchup multiple times and summarizes the results.
#'
#' @param team1 Character string. A two- or three-letter NFL team abbreviation.
#'   Must be one of the following:
#'   \code{"ARI"}, \code{"ATL"}, \code{"BAL"}, \code{"BUF"}, \code{"CAR"},
#'   \code{"CHI"}, \code{"CIN"}, \code{"CLE"}, \code{"DAL"}, \code{"DEN"},
#'   \code{"DET"}, \code{"GB"}, \code{"HOU"}, \code{"IND"}, \code{"JAX"},
#'   \code{"KC"}, \code{"LA"}, \code{"LAC"}, \code{"LV"}, \code{"MIA"},
#'   \code{"MIN"}, \code{"NE"}, \code{"NO"}, \code{"NYG"}, \code{"NYJ"},
#'   \code{"PHI"}, \code{"PIT"}, \code{"SEA"}, \code{"SF"}, \code{"TB"},
#'   \code{"TEN"}, \code{"WAS"}.
#'
#' @param team2 Character string. A two- or three-letter NFL team abbreviation.
#'   Must be one of the following:
#'   \code{"ARI"}, \code{"ATL"}, \code{"BAL"}, \code{"BUF"}, \code{"CAR"},
#'   \code{"CHI"}, \code{"CIN"}, \code{"CLE"}, \code{"DAL"}, \code{"DEN"},
#'   \code{"DET"}, \code{"GB"}, \code{"HOU"}, \code{"IND"}, \code{"JAX"},
#'   \code{"KC"}, \code{"LA"}, \code{"LAC"}, \code{"LV"}, \code{"MIA"},
#'   \code{"MIN"}, \code{"NE"}, \code{"NO"}, \code{"NYG"}, \code{"NYJ"},
#'   \code{"PHI"}, \code{"PIT"}, \code{"SEA"}, \code{"SF"}, \code{"TB"},
#'   \code{"TEN"}, \code{"WAS"}.
#'   Must be different from \code{team1}.
#'
#' @param year Numeric. Season year. Must be an integer between
#'   \code{2022} and \code{2025}.
#'
#' @param week Numeric. NFL regular season week to use for the simulation.
#'   Must be an integer between \code{1} and \code{18}. The specified week
#'   determines the active rosters, depth charts, and player PFF grades used
#'   during the simulation.
#'
#' @param offseason Logical. Indicates if it is currently the NFL offseason
#'
#' @param n Integer. Number of game simulations to perform. The default is
#'   \code{100}.
#'
#' @param max_attempts Integer. Maximum number of failed simulation attempts
#'   allowed before stopping.
#'
#' @return A one-row data.frame summarizing the simulated games.
#' \describe{
#'   \item{samplesize}{Number of successful simulations performed.}
#'   \item{team1}{Name of the first team.}
#'   \item{team1wins}{Number of simulations won by \code{team1}.}
#'   \item{team1mean}{Mean score of \code{team1} across all simulations.}
#'   \item{team1sd}{Standard deviation of \code{team1}'s scores.}
#'   \item{team2}{Name of the second team.}
#'   \item{team2wins}{Number of simulations won by \code{team2}.}
#'   \item{team2mean}{Mean score of \code{team2} across all simulations.}
#'   \item{team2sd}{Standard deviation of \code{team2}'s scores.}
#' }
#'
#' @details
#' This function repeatedly calls \code{\link{simulate_game}} to simulate the
#' specified matchup. Summary statistics are computed from the completed
#' simulations, including the number of wins for each team and the mean and
#' standard deviation of each team's score. Simulations that fail are retried
#' until either \code{n} successful simulations have been completed or
#' \code{max_attempts} consecutive failures have occurred.
#'
#' @export
multiple_simulations <- function(team1, team2, year = 2025, week = 1,
                                 offseason = FALSE,
                                 n = 100, max_attempts = 3) {
  resultlist <- vector("list", n)
  successful_runs <- 0
  attempts <- 0

  while(successful_runs < n && attempts < n * max_attempts) {
    attempts <- attempts + 1
    start <- Sys.time()
    message(paste0("Attempt ", attempts, " Start Time: ", start))

    # Initialize res as NULL before the try block
    res <- NULL
    timed_out <- FALSE

    # Try with time limit
    try_result <- try({
      setTimeLimit(elapsed = 240, transient = TRUE)  # 4 minutes = 240 seconds
      res <- simulate_game(team1, team2, year, week)
      setTimeLimit(elapsed = Inf, transient = TRUE)  # Reset time limit
    }, silent = TRUE)

    if(inherits(res, "try-error")) {
      # Check if the error was due to a timeout
      if(grepl("reached elapsed time limit", try_result[1])) {
        timed_out <- TRUE
        message("Simulation timed out after 4 minutes - retrying...")
      } else {
        message(sprintf("Simulation failed with error: %s", try_result[1]))
      }
      next
    }

    successful_runs <- successful_runs + 1
    resultlist[[successful_runs]] <- res
    message(paste0("Completed ", successful_runs, "/", n, " in ",
                   difftime(Sys.time(), start, units = "secs"), " secs"))
  }

  if(successful_runs == 0) {
    warning("All simulations failed")
    return(NULL)
  }

  # Combine only successful runs
  combined_results <- do.call(rbind, resultlist[1:successful_runs])

  data.frame(
    samplesize = successful_runs,
    team1 = team1,
    team1wins = sum(combined_results[[team1]] > combined_results[[team2]]),
    team1mean = mean(as.numeric(combined_results[[team1]])),
    team1sd = sd(as.numeric(combined_results[[team1]])),
    team2 = team2,
    team2wins = sum(combined_results[[team2]] > combined_results[[team1]]),
    team2mean = mean(as.numeric(combined_results[[team2]])),
    team2sd = sd(as.numeric(combined_results[[team2]]))
  )
}


#' Simulate a full NFL week
#'
#' Runs multiple simulations for every game in a given NFL week. By default,
#' the matchups are pulled from the NFL schedule using the given year and week.
#' Custom matchup vectors can also be supplied.
#'
#' @param year A year between 2022 and 2025.
#' @param weeknum A week number between 1 and 18.
#' @param tm1vec Optional vector of team abbreviations for the first team in each matchup.
#' @param tm2vec Optional vector of team abbreviations for the second team in each matchup.
#' @param offseason Logical. Indicates if it is currently the NFL offseason.
#' @param sims Numeric. Must be a positive integer. Sets number of simulations desired
#' for each matchup.
#'
#' @return A list where each element contains the output from
#'   \code{multiple_simulations()} for one matchup.
#'
#' @examples
#' \dontrun{
#' whole_week_simulations(year = 2025, weeknum = 1)
#'
#' whole_week_simulations(
#'   year = 2025,
#'   weeknum = 1,
#'   tm1vec = c("DAL", "KC"),
#'   tm2vec = c("PHI", "BUF")
#' )
#' }
#'
#' @export
whole_week_simulations <- function(year, weeknum, tm1vec = c(), tm2vec = c(),
                                   offseason = FALSE,
                                   sims = 100){
  if(is_empty(tm1vec) & is_empty(tm2vec)){
    weekdf <- nflreadr::load_schedules() |>
      filter(season == year & week == weeknum) |>
      select(season, week, away_team, home_team)
    tm1 <- weekdf$away_team
    tm2 <- weekdf$home_team
  } else{
    tm1 <- tm1vec
    tm2 <- tm2vec
  }
  all_simulations <- list()
  for(i in 1:length(tm1)){
    print(paste0("Team 1: ", tm1[i], " vs. Team 2: ", tm2[i]))
    simulation <- multiple_simulations(team1 = tm1[i], team2 = tm2[i], n = sims)
    all_simulations <- list.append(all_simulations, simulation)
    print(simulation)
  }
  all_simulations
}


