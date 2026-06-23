
### NOTE TO SELF (4/11/26): The models need A LOT of work


# Loading All Data ----------------------------------------------

# Environment to hold all data/models from extdata
.nfl_env <- new.env()

# Function to load all .rds files once
.load_extdata <- function() {
  # Only load if the environment is empty
  if (length(ls(.nfl_env)) == 0) {
    files <- list.files(system.file("extdata", package = "NFLSimulator"),
                        pattern = "\\.rds$", full.names = TRUE)
    for (f in files) {
      readRDS(f, envir = .nfl_env)
    }
  }
  invisible(NULL)
}

# .load_extdata()


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

#' Get the Most Updated NFL Depth Chart for a Team
#'
#' Returns the most updated depth chart for a selected NFL team
#'
#' @param team Character string. A two- or three-letter NFL team abbreviation.
#'   Must be one of the following:
#'   \code{"ARI"}, \code{"ATL"}, \code{"BAL"}, \code{"BUF"}, \code{"CAR"},
#'   \code{"CHI"}, \code{"CIN"}, \code{"CLE"}, \code{"DAL"}, \code{"DEN"},
#'   \code{"DET"}, \code{"GB"}, \code{"HOU"}, \code{"IND"}, \code{"JAX"},
#'   \code{"KC"}, \code{"LA"}, \code{"LAC"}, \code{"LV"}, \code{"MIA"},
#'   \code{"MIN"}, \code{"NE"}, \code{"NO"}, \code{"NYG"}, \code{"NYJ"},
#'   \code{"PHI"}, \code{"PIT"}, \code{"SEA"}, \code{"SF"}, \code{"TB"},
#'   \code{"TEN"}, \code{"WAS"}.
#'
#'
#' @return A \code{data.frame} representing the depth chart, including player names
#'   and their positions.
#'
#' @usage
#' get_depth_chart("NE")
#' get_depth_chart("DAL")
#'
#' @details This function returns a depth chart (3 deep) for each position on
#' offense and defense (not special teams).
#'
#'
#' @export
depthchart <- function(tm, year){
  baseteamchart <- nflreadr::load_depth_charts(year) |>
    dplyr::mutate(as_of_date = as.Date(dt)) |>
    dplyr::filter(as_of_date == as.Date(paste0(year, "-09-05"))) |>
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
  file_path <- file.path("inst",
                         "extdata",
                         paste0("PFF_", pos_file, "_", year, ".rds"))

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


full_PFF <- function(tm, year){
  dpt_cht <- depthchart(tm, year) |>
    dplyr::select(-c(position, player))
  quarterback <- PFF_data_creator(year, "QB")
  runningbacks <- PFF_data_creator(year, "HB")
  widereceivers <- PFF_data_creator(year, "WR")
  tightends <- PFF_data_creator(year, "TE")
  oline <- PFF_data_creator(year, "OL")
  frontseven <- PFF_data_creator(year, "F7")
  defensivebacks <- PFF_data_creator(year, "DB")

  allpositions <- list(quarterback, runningbacks, widereceivers, tightends,
                       oline, defensivebacks, frontseven)
  finaldf <- suppressMessages(reduce(allpositions, dplyr::full_join))
  finaldf$pff_id <- as.character(finaldf$pff_id)
  dplyr::left_join(dpt_cht, finaldf) |>
    suppressMessages() |>
    dplyr::filter(!is.na(player))
}

PFF_grade_formulator <- function(offtm, deftm, year) {
  offPFF <- full_PFF(offtm, year)
  defPFF <- full_PFF(deftm, year)

  passer <- offPFF |> filter(final_position == "QB1")
  rushers <- offPFF |> filter(position == "HB")
  receivers <- offPFF |> filter(position %in% c("WR", "TE"))
  oline <- offPFF |> filter(position == "OL")
  passrush <- defPFF |> filter(position == "F7")
  coverage <- defPFF |> filter(position == "DB")
  rundef <- defPFF |> filter(position %in% c("F7", "DB"))

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

  path <- system.file("extdata", package = "NFLSimulator")

  files <- list.files(path, pattern = "\\.rds$", full.names = TRUE)

  data_env <- new.env()

  for (f in files) {

    obj <- readRDS(f)

    name <- tools::file_path_sans_ext(basename(f))

    data_env[[name]] <- obj
  }

  data_env
}

.nfl_env <- new.env()

data_env <- loading_ext_data()
#
# ### Testing Variables
posstm <- "DEN"
deftm <- "SEA"
down <- 3
togo <- 15
YdsBef <- 63
posstmdiff <- 0
quarter_secs <- 800
quarter <- 1
year <- 2025
base_PFF_grade <-  PFF_grade_formulator(posstm, deftm, year)
personnel <- "11"
def_personnel <- "Nickel"
formation <- "EMPTY"
coverage <- "Cover_3"
runorpass <- "Pass"
# ###



#' @export
choose_personnel <- function(down, togo, YdsBef, posstmdiff, quarter_secs,
                             quarter, base_PFF_grade){
  modeldat <- data.frame(
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    YdstoEZBef = YdsBef,
    down = down,
    yardsToGo = togo,
    pff_player_grade_component = base_PFF_grade
  )

  X <- model.matrix(
    ~ quarter + down + yardsToGo +
      YdstoEZBef + PossTmDiff +
      pff_player_grade_component +
      QuarterSeconds - 1,
    data = modeldat
  )

  probs <- predict(data_env$off_pers_mod, X, type = "probs")
  personnels <- sort(unique(data_env$offdf$simple_personnel))
  sample(personnels, size = 1, prob = probs)
}

#' @export
choose_def_personnel <- function(down, togo, YdsBef, posstmdiff, quarter_secs,
                                 quarter, personnel, base_PFF_grade){
  modeldat <- data.frame(
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    YdstoEZBef = YdsBef,
    down = down,
    yardsToGo = togo,
    pff_player_grade_component = base_PFF_grade
  )

  modeldat$simple_personnel <- factor(
    personnel,
    levels = sort(unique(data_env$offdf$simple_personnel))
  )

  X <- model.matrix(
    ~ quarter + down + yardsToGo +
      simple_personnel +
      YdstoEZBef + PossTmDiff +
      pff_player_grade_component +
      QuarterSeconds - 1,
    data = modeldat
  )

  probs <- predict(data_env$def_pers_mod, X, type = "probs")
  def_personnels <- sort(unique(data_env$offdf$simple_def_personnel))
  sample(def_personnels, size = 1, prob = probs)
}

#' @export
choose_formation <- function(down, togo, YdsBef, posstmdiff, quarter_secs,
                             quarter, personnel, def_personnel, base_PFF_grade){
  modeldat <- data.frame(
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    YdstoEZBef = YdsBef,
    down = down,
    yardsToGo = togo,
    pff_player_grade_component = base_PFF_grade
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
      pff_player_grade_component +
      QuarterSeconds - 1,
    data = modeldat
  )

  probs <- predict(data_env$off_form_mod, X, type = "probs")
  off_forms <- sort(unique(data_env$offdf$off_form))
  sample(off_forms, size = 1, prob = probs)
}

#' @export
choose_def_coverage <- function(down, togo, YdsBef, posstmdiff, quarter_secs,
                                quarter, personnel, def_personnel, formation, base_PFF_grade){
  modeldat <- data.frame(
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    down = down,
    yardsToGo = togo,
    YdstoEZBef = YdsBef,
    pff_player_grade_component = base_PFF_grade
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
      pff_player_grade_component +
      QuarterSeconds - 1,
    data = modeldat
  )

  probs <- predict(data_env$def_cov_mod, X, type = "probs")
  def_covs <- sort(unique(data_env$offdf$def_simple_coverage))
  sample(def_covs, size = 1, prob = probs)
}

#' @export
runorpass <- function(down, togo, YdsBef, posstmdiff, quarter_secs,
                      quarter, personnel, formation, def_personnel, coverage, base_PFF_grade){
  modeldat <- data.frame(
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    down = down,
    yardsToGo = togo,
    YdstoEZBef = YdsBef,
    pff_player_grade_component = base_PFF_grade
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
      pff_player_grade_component +
      QuarterSeconds - 1,
    data = modeldat
  )

  passprob <- predict(data_env$run_pass_mod, X, type = "probs")
  probs = c(passprob, 1 - passprob)
  labels <- c("Pass", "Run")
  sample(labels, size = 1, prob = probs)
}


# Route Selection Function ------------------------------------------------

off_dat <- full_PFF(posstm, 2025)
def_dat <- full_PFF(deftm, 2025)

#' @export
routeselection <- function(posstm, deftm, down, togo, YdsBef, posstmdiff,
                           quarter_secs, quarter, off_dat, def_dat, year,
                           base_PFF_grade
                           ){
  whole_offense <- off_dat |>
    filter(!is.na(games))
  whole_defense <- def_dat |>
    filter(!is.na(games))

  personnel <- choose_personnel(down, togo, YdsBef, posstmdiff,
                                quarter_secs, quarter, base_PFF_grade)

  def_personnel <- choose_def_personnel(down, togo, YdsBef, posstmdiff,
                                        quarter_secs, quarter, personnel,
                                        base_PFF_grade)

  formation <- choose_formation(down, togo, YdsBef, posstmdiff,
                                quarter_secs, quarter, personnel, def_personnel,
                                base_PFF_grade)

  coverage <- choose_def_coverage(down, togo, YdsBef, posstmdiff, quarter_secs,
                                  quarter, personnel, def_personnel, formation,
                                  base_PFF_grade)

  runpassselection <- runorpass(down, togo, YdsBef, posstmdiff, quarter_secs,
                                quarter, personnel, formation, def_personnel,
                                coverage, base_PFF_grade)

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
      YdstoEZBef = YdsBef,
      pff_player_grade_component = base_PFF_grade
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
        pff_player_grade_component +
        QuarterSeconds - 1,
      data = modeldat
    )

    ### FIGURES OUT NUMBER OF COVER PLAYERS AND PASS RUSHERS
    probs <- predict(data_env$cov_players_mod, X, type = "probs")
    def_cov_players <- 4:8
    coverplayers <- sample(def_cov_players, size = 1, prob = probs)
    passrushers <- 11 - coverplayers

    X <- model.matrix(
      ~ quarter + down + yardsToGo +
        simple_personnel +
        simple_def_personnel +
        off_form +
        YdstoEZBef + PossTmDiff +
        pff_player_grade_component +
        QuarterSeconds - 1,
      data = modeldat
    )

    ### FIGURES OUT THE NUMBER OF ROUTES RAN
    probs <- predict(data_env$routes_count_mod, X, type = "probs")
    routes_ran_nums <- 2:5
    routescount <- sample(routes_ran_nums, size = 1, prob = probs)
    passrushers <- 11 - coverplayers

    num_RB <- as.numeric(substr(personnel, 1, 1))
    num_TE <- as.numeric(substr(personnel, 2, 2))

    ### GETS ROUTE COMBINATION

    X <- cbind(X, routescount)
    route_combo_probs <- predict(data_env$route_combo_mod, X, type = "probs")
    route_combo <- sample(1:20, size = 1, prob = route_combo_probs)


    #### GETS SPECIFIC OFFENSIVE AND DEFENSIVE PLAYERS ON THE FIELD
    mydf <- tibble::tibble(
      RB = as.numeric(substr(personnel, 1, 1)),
      TE = as.numeric(substr(personnel, 2, 2)),
      WR = routesrun - (RB + TE)
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
      filter(final_position %in% c("LT2", "LG2", "C2", "RG2", "RT2",
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

    # pass_rush_depth_chart <- snaps_to_depth_chart(deftm, "pass_rush")
    # coverage_depth_chart <- snaps_to_depth_chart(deftm, "coverage")

    def_depth_chart <- def_dat
    def_depth_chart <- def_depth_chart[!grepl("( O| IR| D)$", def_depth_chart$player), ]
    def_by_pos <- split(def_depth_chart, def_depth_chart$position)

    f7_players <- def_by_pos$F7

    on_field_F7 <- sample(f7_players$pff_id, frontsevenamount,
                        prob = f7_players$mean_def_snaps / sum(f7_players$mean_def_snaps))

    onfieldF7 <- f7_players |>
      filter(pff_id %in% on_field_F7)

    secondary_players <- def_by_pos$DB

    on_field_secondary <- sample(secondary_players$pff_id, secondarycount,
                                 prob = secondary_players$mean_def_snaps / sum(secondary_players$mean_def_snaps))

    onfieldsecondary <- secondary_players |>
      filter(pff_id %in% on_field_secondary)

    # f7passrushprobs <- f7passrushers$final_predicted_snap_ct/sum(f7passrushers$final_predicted_snap_ct)
    # secondarycoverprobs <- secondary_coverplayers$final_predicted_snap_ct/sum(secondary_coverplayers$final_predicted_snap_ct)

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

    # allcoverplayers <- c(on_field_F7_cover_players, on_field_secondary_cover_players)
    covertgtplyrcnt <- sample(c(0,1,2), 1, prob = c(.09, .9, .01))

    potentialtgtcovers <- def_depth_chart |> filter(pff_id %in% on_field_coverage)

    covertgtprobs <- potentialtgtcovers$mean_def_snaps / sum(potentialtgtcovers$mean_def_snaps)
    covertargetplayers <- sample(potentialtgtcovers$pff_id, covertgtplyrcnt, prob = covertgtprobs)
    noncovertgtplayers <- potentialtgtcovers |>
      filter(!pff_id %in% covertargetplayers)

    modeldat$passrushers <- passrushers

    ttt_pred_dist <- posterior_predict(data_env$timeToThrowmodel, newdata = modeldat)
    timetoThrow <- round(apply(ttt_pred_dist, 2, sample, size = 1),3)
    modeldat$timeToThrow <- timetoThrow

    chosenroutelist <- toupper(chosenroutelist)

    ################ STOPPING POINT 6/22/26 7:25 PM

    personneldf <- data.frame(
      RB = as.numeric(substr(personnel, 1, 1)),
      WR = length(chosenroutelist) - as.numeric(substr(personnel, 1, 1)) -
        as.numeric(substr(personnel, 2, 2)),
      TE = as.numeric(substr(personnel, 2, 2))
    )

    poschosenroutelist <- sort(chosenroutelist)

    if(personneldf$RB!=0){
      rbroutedf <- data_env$posroutedf |> filter(position=="RB" & route %in% poschosenroutelist)
      RBproblist <- c()
      for(route in poschosenroutelist){
        RBprob <- rbroutedf$prob[rbroutedf$route==route]
        RBproblist <- append(RBproblist, RBprob)
      }
      RBroutes <- sample(poschosenroutelist, personneldf$RB, prob = RBproblist/sum(RBproblist))
      rbroutelist <- c()
      for(r in 1:personneldf$RB){
        RBindex <- match(RBroutes[r], poschosenroutelist)
        poschosenroutelist <- poschosenroutelist[-RBindex]
        rbroutelist <- append(rbroutelist, paste0("RB: ", RBroutes[r]))
      }
    } else{
      rbroutelist <- c()
    }

    if(personneldf$TE!=0){
      teroutedf <- data_env$posroutedf |> filter(position=="TE" & route %in% poschosenroutelist)
      TEproblist <- c()
      for(route in poschosenroutelist){
        TEprob <- teroutedf$prob[teroutedf$route==route]
        TEproblist <- append(TEproblist, TEprob)
      }
      TEroutes <- sample(poschosenroutelist, personneldf$TE, prob = TEproblist/sum(TEproblist))
      teroutelist <- c()
      for(t in 1:personneldf$TE){
        TEindex <- match(TEroutes[t], poschosenroutelist)
        poschosenroutelist <- poschosenroutelist[-TEindex]
        teroutelist <- append(teroutelist, paste0("TE: ", TEroutes[t]))
      }
    } else{
      teroutelist <- c()
    }

    if(personneldf$WR!=0){
      wrroutedf <- data_env$posroutedf |> filter(position=="WR" & route %in% poschosenroutelist)
      WRproblist <- c()
      for(route in poschosenroutelist){
        WRprob <- wrroutedf$prob[wrroutedf$route==route]
        WRproblist <- append(WRproblist, WRprob)
      }
      WRroutes <- sample(poschosenroutelist, personneldf$WR, prob = WRproblist/sum(WRproblist))
      wrroutelist <- c()
      for(w in 1:personneldf$WR){
        WRindex <- match(WRroutes[w], poschosenroutelist)
        poschosenroutelist <- poschosenroutelist[-WRindex]
        wrroutelist <- append(wrroutelist, paste0("WR: ", WRroutes[w]))
      }
    } else{
      wrroutelist <- c()
    }

    finalvec <- c(rbroutelist, wrroutelist, teroutelist)

    tgtrouteprobs <- data.frame(t(predict(data_env$targetted_route_model, modeldat, type = "probs")))
    tgtprobs <- c()
    for(route in chosenroutelist){
      colkey <- which(colnames(tgtrouteprobs)==route)
      prob <- as.numeric(tgtrouteprobs[colkey])
      tgtprobs <- append(tgtprobs, prob)
    }
    tgtprobs1 <- tgtprobs/sum(tgtprobs)
    targetted_route <- sample(chosenroutelist, 1, prob = tgtprobs1)

    tgtposoptions <- finalvec[grepl(targetted_route, finalvec)]
    tgtposselection <- sample(tgtposoptions, 1,
                              prob = rep(1/length(tgtposoptions), length(tgtposoptions)))
    tgtpos <- str_split_fixed(tgtposselection, ":", 2)[,1]

    on_field_recs <- whole_offense |>
      filter(pff_id %in% c(onfieldRBS, onfieldWRS, onfieldTEs))

    if(tgtpos %in% c("HB", "RB")){
      newRBs <- on_field_recs |>
        filter(position=="HB")
      tgtprobs <- newRBs$mean_rush_attempts / sum(newRBs$mean_rush_attempts)
      tgtplayer <- sample(newRBs$pff_id, 1, prob = tgtprobs)
    } else if(tgtpos=="WR"){
      newWRs <- on_field_recs |>
        filter(position=="WR")
      tgtprobs <- newWRs$mean_routes/ sum(newWRs$mean_routes)
      tgtplayer <- sample(newWRs$pff_id, 1, prob = tgtprobs)
    } else if(tgtpos=="TE"){
      newTEs <- on_field_recs |>
        filter(position=="TE")
      tgtprobs <- newTEs$mean_routes / sum(newTEs$mean_routes)
      tgtplayer <- sample(newTEs$pff_id, 1, prob = tgtprobs)
    } else{
      tgtplayer <- NA
    }

    otherroutes <- on_field_recs$pff_id[on_field_recs$pff_id != tgtplayer]

    list(pers = personnel, form = formation, runorpass = "Pass",
         def_pers = def_personnel, coverage = coverage,
         routes = finalvec, tgt_route = targetted_route, otherroutes = otherroutes,
         quarterback = onfieldQB,
         oline = onfieldOL,
         passrushers = on_field_rushers,
         coveringtgtplayer = covertargetplayers,
         othercoverplayers = noncovertgtplayers$pff_id,
         tgt_pos = tgtpos, tgt_player = tgtplayer)

  } else if(runpassselection == "Run"){
    RBs <- as.numeric(substr(personnel, 1, 1))
    TEs <- as.numeric(substr(personnel, 2, 2))
    WRs <- 5 - RBs - TEs

    # QB
    onfieldQB <- whole_offense$pff_id[whole_offense$position=="QB"]

    # RBs
    runningbacks <- whole_offense |>
      filter(position=="HB")
    rbprobs <- runningbacks$mean_rush_attempts / sum(runningbacks$mean_rush_attempts)
    onfieldRBS <- sample(runningbacks$pff_id, RBs, prob = rbprobs)

    # WRs
    widereceivers <- whole_offense |>
      filter(position=="WR")
    wrprobs <- widereceivers$mean_routes / sum(widereceivers$mean_routes)
    onfieldWRS <- sample(widereceivers$pff_id, WRs, prob = wrprobs)

    # TEs
    tightends <- whole_offense |>
      filter(position=="TE")
    teprobs <- tightends$mean_routes / sum(tightends$mean_routes)
    onfieldTES <- sample(tightends$pff_id, TEs, prob = teprobs)

    olinecount <- 10 - (RBs + WRs + TEs)
    onfieldOL <- whole_offense$pff_id[whole_offense$final_position %in%
                                        c("LT1", "LG1", "C1", "RG1", "RT1")]
    if(olinecount > 5){
      backupOL <- whole_offense |>
        filter(position=="OL" & depth_next!="OL1")
      backupOLprobs <- backupOL$final_predicted_snap_ct / sum(backupOL$final_predicted_snap_ct)
      backupOL <- sample(backupOL$pff_id, (olinecount - 5), prob = backupOLprobs)
      onfieldOL <- c(onfieldOL, backupOL)
    }
    potential_rush_posdf <- whole_offense |>
      filter(pff_id %in% c(onfieldRBS))

    rushposprobs <- potential_rush_posdf$mean_rush_attempts /
      sum(potential_rush_posdf$mean_rush_attempts)

    rushplayer <- sample(potential_rush_posdf$pff_id, 1, prob = rushposprobs)
    rushpos <- potential_rush_posdf$position[potential_rush_posdf$pff_id==rushplayer]

    rest_of_offense <- c(onfieldQB, onfieldRBS, onfieldWRS, onfieldTES)
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
      filter(position=="F7")

    f7probs <- f7df$mean_def_snaps / sum(f7df$mean_def_snaps)
    f7players <- sample(f7df$pff_id, frontsevenamount, prob = f7probs)

    secondaryamount <- 11 - frontsevenamount
    secondarydf <- whole_defense |>
      filter(position=="DB")
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

saferouteselection <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                               off_dat, def_dat, year){
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

# routeselection("DEN", "LV", 3, 20, 70, 0, 800, 1,
#             full_PFF("DEN", 2025), full_PFF("LV", 2025), 2025)

#'
#' # Yards Gained Function ---------------------------------------------------
#'
#' #' @export
#' yardsgained <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
#'                         off_dat, def_dat, year){
#'   output <- suppressWarnings(saferouteselection(posstm, deftm, down, togo,
#'                                                 YdsBef, posstmdiff, quarter_secs, quarter,
#'                                                 off_dat, def_dat, year))
#'   whole_offense <- off_dat
#'   whole_defense <- def_dat
#'   if(output$runorpass=="Pass"){
#'     if(output$coverage %in% c("Cover_1", "RedZone", "GoalLine", "Cover_0",
#'                               "Cover2Man", "Bracket")){
#'       manzone <- "Man"
#'     } else{
#'       manzone <- "Zone"
#'     }
#'
#'     tgtroute <- output$tgt_route
#'
#'     qbdf <- whole_offense |>
#'       filter(pff_id==output$quarterback)
#'     passing_grade <- rnorm(1, mean = qbdf$mean_pass, sd = qbdf$sd_pass)
#'
#'     recdf <- whole_offense |>
#'       filter(pff_id==output$tgt_player)
#'     recgrade <- rnorm(1, mean = recdf$mean_rec, sd = recdf$sd_rec)
#'
#'     cover_tgtplayer_df <- whole_defense |>
#'       filter(pff_id %in% output$coveringtgtplayer)
#'     ### NOTE: FOR NOW, I DON'T HAVE SEPARATE MAN AND ZONE COVER GRADES. THAT WOULD BE
#'     ### EASY TO IMPLEMENT THOUGH (9/3/25)
#'     if(nrow(cover_tgtplayer_df) > 0){
#'       cover_tgt_grade <- mean(mapply(rnorm, n=1, mean = cover_tgtplayer_df$mean_coverage_defense,
#'                                      sd = cover_tgtplayer_df$sd_coverage_defense))
#'     } else{
#'       cover_tgt_grade <- 30
#'     }
#'     ###
#'
#'     olinedf <- whole_offense |>
#'       filter(pff_id %in% output$oline)
#'     olineblockgrade <- mean(mapply(rnorm, n=1, mean = olinedf$mean_pass_block,
#'                                    sd = olinedf$sd_pass_block))
#'
#'     passrushdf <- whole_defense |>
#'       filter(pff_id %in% output$passrushers)
#'     passrushgrade <- mean(mapply(rnorm, n=1, mean = passrushdf$mean_pass_rush_defense,
#'                                  sd = passrushdf$sd_pass_rush_defense))
#'
#'     dummydf <- data.frame(
#'       down = down,
#'       simple_personnel = output$pers,
#'       simple_def_personnel = output$def_pers,
#'       def_simple_coverage = output$coverage,
#'       off_form = output$form,
#'       YdstoEZBef = YdsBef,
#'       yardsToGo = togo,
#'       possessionTeam = posstm,
#'       defensiveTeam = deftm,
#'       runorpass = output$runorpass,
#'       targettedroute = tgtroute,
#'       targettedposition = output$tgt_pos,
#'       oline_brocking_grade_mean = olineblockgrade,
#'       passrushers = length(output$passrushers),
#'       pffpassrush = olineblockgrade - passrushgrade,
#'       pass_grade_mean = passing_grade,
#'       receiving_grade_mean = recgrade,
#'       oline_brocking_grade_mean = olineblockgrade,
#'       cover_target_route_grade_mean = cover_tgt_grade,
#'       passrush_players_grade_mean = passrushgrade
#'     )
#'
#'     pressureplayers <- sample(c(0:4), 1,
#'                               prob = predict(data_env$pressureplayersmodel, newdata = dummydf, type = "probs"))
#'     dummydf$pressureplayers <- pressureplayers
#'
#'     sackprob <- predict(data_env$sack_model, dummydf, type = "response")
#'     sack <- sample(c("Yes", "No"), 1, prob = c(sackprob, 1-sackprob))
#'
#'     if(sack=="Yes"){
#'       random_yards <- round(apply(posterior_predict(
#'         data_env$sack_yards_model, newdata = dummydf), 2, sample, size = 1),0)
#'       list(runpass = "Pass", result = "Sack", yards = random_yards)
#'     } else{
#'       ttt <- round(apply(posterior_predict(
#'         data_env$timeToThrowmodel, newdata = dummydf), 2, sample, size = 1),3)
#'       pressure_component <- if_else(pressureplayers>=1, "pressure", "no_pressure")
#'       ttt_component <- if_else(ttt<2.5, "less", "more")
#'       if(tgtroute %in% c("ANGLE", "FLAT", "IN", "OUT", "SLANT")){
#'         distance_component <- "short"
#'       } else if(tgtroute %in% c("CORNER", "CROSS", "HITCH")){
#'         distance_component <- "medium"
#'       } else if(tgtroute %in% c("GO", "POST", "WHEEL")){
#'         distance_component <- "deep"
#'       } else{
#'         distance_component <- "behind_los"
#'       }
#'       passresult <- sample(c("Complete", "Fumble", "Incomplete", "Interception"), 1,
#'                            prob = predict(data_env$passresultmodel, dummydf, type = "probs"))
#'       if(passresult=="Complete"){
#'         pass_yards <- round(apply(posterior_predict(
#'           data_env$passing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
#'         list(runpass = "Pass", result = "Complete", yards = pass_yards)
#'       } else if(passresult=="Interception"){
#'         pass_yards <- round(apply(posterior_predict(
#'           data_env$passing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
#'         int_yards <- round(apply(posterior_predict(
#'           data_env$int_yds_model, newdata = dummydf), 2, sample, size = 1),0)
#'         list(runpass = "Pass", result = "Interception", yards = pass_yards - int_yards)
#'       } else if(passresult=="Incomplete"){
#'         list(runpass = "Pass", result = "Incomplete", yards = 0)
#'       } else{
#'         fumblepos <- sample(c("QB", output$tgt_pos), 1, prob = c(.6, .4))
#'         if(fumblepos=="QB"){
#'           fumbleprob <- data_env$fumblostdf$lostprob[data_env$fumblostdf$fumbleposition=="QB"]
#'           fumblost <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
#'           if(fumblost=="Yes"){
#'             fumbresult <- "Sack_Fumble_Retained"
#'           } else{
#'             fumbresult <- "Sack_Fumble_Lost"
#'           }
#'           fumbleyards <- -5
#'         } else{
#'           fumbleprob <- data_env$fumblostdf$lostprob[data_env$fumblostdf$fumbleposition==output$tgt_pos]
#'           fumblost <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
#'           if(fumblost=="Yes"){
#'             fumbresult <- "Complete_Fumble_Retained"
#'           } else{
#'             fumbresult <- "Complete_Fumble_Lost"
#'           }
#'           fumbleyards <- round(apply(posterior_predict(
#'             data_env$passing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
#'         }
#'         list(runpass = "Pass", result = fumbresult, yards = fumbleyards)
#'       }
#'     }
#'   } else if(output$runorpass=="Run"){
#'     rushingpos <- output$rushpos
#'
#'     olinedf <- whole_offense |>
#'       filter(pff_id %in% output$oline)
#'     olineblockgrade <- mean(mapply(rnorm, n=1, mean = olinedf$mean_run_block,
#'                                    sd = olinedf$sd_run_block))
#'     dummydf <- data.frame(
#'       down = down,
#'       simple_personnel = output$pers,
#'       simple_def_personnel = output$def_pers,
#'       def_simple_coverage = output$coverage,
#'       off_form = output$form,
#'       YdstoEZBef = YdsBef,
#'       yardsToGo = togo,
#'       possessionTeam = posstm,
#'       defensiveTeam = deftm,
#'       runorpass = output$runorpass,
#'       oline_brocking_grade_mean = olineblockgrade,
#'       rushingposition = rushingpos
#'     )
#'
#'     runningplayerdf <- whole_offense |>
#'       filter(pff_id==output$rushplayer)
#'     runninggrade <- rnorm(1, mean = runningplayerdf$mean_rush,
#'                           sd = runningplayerdf$sd_rush)
#'
#'     othersdf <- whole_offense |>
#'       filter(pff_id %in% output$rest_of_offense) |>
#'       filter(pff_id != output$quarterback)
#'     othersgrade <- mean(mapply(rnorm, 1, mean = othersdf$mean_run_block,
#'                                sd = othersdf$sd_run_block))
#'
#'     ### NOTE, I NEED TO REDO FEATURE ENGINEERING FOR HOW THE GRADES CORRELATE TO
#'     ### RUSH YARDS. I HOPE TO DO THAT SOON (9/3/25)
#'     totaloffensegrade <- (4*olineblockgrade + 3*runninggrade + 2*othersgrade)/9
#'     ###
#'
#'     defensedf <- whole_defense |>
#'       filter(pff_id %in% output$defense)
#'     rundefgrade <- mean(mapply(rnorm, 1, mean = defensedf$mean_run_defense,
#'                                sd = defensedf$sd_run_defense))
#'
#'     totaldefensegrade <- rundefgrade
#'
#'     ### SAME THING ABOUT FEATURE ENGINEERING
#'     dummydf$pff_player_grade_component <- totaloffensegrade - totaldefensegrade
#'     ###
#'
#'     if(dummydf$rushingposition == "HB") {
#'       dummydf$rushingposition <- "RB"
#'       rushingpos <- "RB"
#'     }
#'
#'     rush_yards <- round(apply(posterior_predict(
#'       data_env$rushing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
#'     fumbleprob <- predict(data_env$rushfumble_model, newdata = dummydf, type = "response")
#'     fumble <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
#'     if(fumble=="No"){
#'       result <- "No_Fumble"
#'     } else{
#'       fldf <- data_env$fumblostdf |> filter(fumbleposition==rushingpos)
#'       fumblelost <- sample(c("Yes", "No"), 1, prob = c(fldf$lostprob, 1-fldf$lostprob))
#'       if(fumblelost=="No"){
#'         result <- "Fumble_Retained"
#'       } else{
#'         result <- "Fumble_Lost"
#'       }
#'     }
#'     list(runpass = "Run", result = result, yards = rush_yards)
#'   }
#' }
#'
#' safeyardsgained <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
#'                             off_dat, def_dat, year){
#'   safeydsfun <- safely(yardsgained)
#'   out <- suppressWarnings(safeydsfun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
#'                                      off_dat, def_dat, year))
#'   while(!is.null(out$error) | is.null(out$result) | any(is.na(out$result))){
#'     out <- suppressWarnings(safeydsfun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
#'                                        off_dat, def_dat, year))
#'   }
#'   out$result
#' }
#'
#' # yardsgained("DEN", "LV", 1, 10, 70, 0, 800, 1,
#' #             full_PFF("DEN", 2025), full_PFF("LV", 2025), 2025)
#'
#' #' @export
#' simulator <- function(team1, team2, year = 2025, track = "NO") {
#'   # track <- track
#'   play <- list(yards = 0, runpass = "Run", result = "Default")
#'   # Initialize all game state variables
#'   game_state <- list(
#'     quarter = 1,
#'     secondsleft = 3600,
#'     quartersecondsleft = 900,
#'     down = 1,
#'     togo = 10,
#'     ydsbef = 70,
#'     teams = c(team1, team2),
#'     scoredf = data.frame(x1 = 0, x2 = 0),
#'     posstm = NULL,
#'     deftm = NULL,
#'     posstmmargin = 0,
#'     playtime = NULL,
#'     afterplaytime = NULL,
#'     option = NULL
#'   )
#'
#'   whole_team1 <- full_PFF(team1, year)
#'   whole_team2 <- full_PFF(team2, year)
#'
#'   colnames(game_state$scoredf) <- c(team1, team2)
#'   game_state$posstm <- sample(game_state$teams, 1, prob = c(.5, .5))
#'   game_state$deftm <- game_state$teams[which(c(team1, team2) != game_state$posstm)]
#'   game_state$startoffteam <- game_state$posstm
#'   game_state$startdefteam <- game_state$deftm
#'
#'   # Helper functions (all take and return game_state)
#'   timeupdater <- function(game_state, playtime, afterplaytime) {
#'     game_state$secondsleft <- round(game_state$secondsleft - playtime - afterplaytime)
#'     game_state$quartersecondsleft <- round(game_state$quartersecondsleft - playtime - afterplaytime)
#'     return(game_state)
#'   }
#'
#'   playtimecorrector <- function(game_state) {
#'     if(is.null(game_state$playtime) || game_state$playtime < 3) {
#'       game_state$playtime <- 3
#'     }
#'     return(game_state)
#'   }
#'
#'   togocorrector <- function(game_state) {
#'     if(game_state$togo > game_state$ydsbef) {
#'       game_state$togo <- game_state$ydsbef
#'     }
#'     return(game_state)
#'   }
#'
#'   posschange <- function(game_state) {
#'     temp <- game_state$posstm
#'     game_state$posstm <- game_state$deftm
#'     game_state$deftm <- temp
#'     return(game_state)
#'   }
#'
#'   touchdown <- function(game_state) {
#'     if(game_state$posstmmargin %in% c(-1, -5, -8, -11, -15)) {
#'       mypoints <- sample(c(6,8), 1, prob = c(.49, .51))
#'     } else {
#'       mypoints <- sample(c(6,7), 1, prob = c(.025, .975))
#'     }
#'     return(mypoints)
#'   }
#'
#'   afterplaytimegenerator <- function(game_state) {
#'     if(game_state$posstmmargin < 0 & game_state$secondsleft <= 240) {
#'       game_state$afterplaytime <- round(rnorm(1, 8.5, 1))
#'     } else {
#'       game_state$afterplaytime <- round(rnorm(1, 30, 3.5))
#'     }
#'     return(game_state)
#'   }
#'
#'   posstmmargin_updater <- function(game_state) {
#'     game_state$posstmmargin <- as.numeric(game_state$scoredf[[game_state$posstm]] -
#'                                             game_state$scoredf[[game_state$deftm]])
#'     return(game_state)
#'   }
#'
#'   quartercheck <- function(game_state) {
#'     if(game_state$quartersecondsleft <= 0) {
#'       game_state$quartersecondsleft <- 900
#'       game_state$secondsleft <- 3600 - (game_state$quarter * 900)
#'       game_state$quarter <- game_state$quarter + 1
#'       if(game_state$quarter == 3) {
#'         game_state$posstm <- game_state$startdefteam
#'         game_state$deftm <- game_state$startoffteam
#'         game_state$down <- 1
#'         game_state$togo <- 10
#'         game_state$ydsbef <- 70
#'         game_state <- posstmmargin_updater(game_state)
#'       }
#'     }
#'     return(game_state)
#'   }
#'
#'   deftd <- function(game_state, playtimemean, playtimesd) {
#'     game_state$scoredf[[game_state$deftm]] <-
#'       as.numeric(game_state$scoredf[[game_state$deftm]]) + touchdown(game_state)
#'     game_state$down <- 1
#'     game_state$togo <- 10
#'     game_state$ydsbef <- 70
#'     game_state <- togocorrector(game_state)
#'     game_state <- posstmmargin_updater(game_state)
#'     game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
#'     game_state <- playtimecorrector(game_state)
#'     game_state$afterplaytime <- 0
#'     game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'     game_state <- quartercheck(game_state)
#'     return(game_state)
#'   }
#'
#'   touchback <- function(game_state, playtimemean, playtimesd) {
#'     game_state <- posschange(game_state)
#'     game_state$down <- 1
#'     game_state$togo <- 10
#'     game_state$ydsbef <- 80
#'     game_state <- togocorrector(game_state)
#'     game_state <- posstmmargin_updater(game_state)
#'     game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
#'     game_state <- playtimecorrector(game_state)
#'     game_state$afterplaytime <- 0
#'     game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'     game_state <- quartercheck(game_state)
#'     return(game_state)
#'   }
#'
#'   reg_turnover <- function(game_state, playtimemean, playtimesd){
#'     game_state <- posschange(game_state)
#'     game_state$down <- 1
#'     game_state$togo <- 10
#'     game_state$ydsbef <- 100 - as.numeric(game_state$ydsbef - play$yards)
#'     game_state <- togocorrector(game_state)
#'     game_state <- posstmmargin_updater(game_state)
#'     game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
#'     game_state <- playtimecorrector(game_state)
#'     game_state$afterplaytime <- 0
#'     game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'     game_state <- quartercheck(game_state)
#'     return(game_state)
#'   }
#'
#'   safety <- function(game_state, playtimemean, playtimesd){
#'     game_state$scoredf[[game_state$deftm]] <-
#'       as.numeric(game_state$scoredf[[game_state$deftm]]) + 2
#'     game_state <- posschange(game_state)
#'     game_state$down <- 1
#'     game_state$togo <- 10
#'     game_state$ydsbef <- 65
#'     game_state <- togocorrector(game_state)
#'     game_state <- posstmmargin_updater(game_state)
#'     game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
#'     game_state <- playtimecorrector(game_state)
#'     game_state$afterplaytime <- 0
#'     game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'     game_state <- quartercheck(game_state)
#'     return(game_state)
#'   }
#'
#'   offtd <- function(game_state, playtimemean, playtimesd){
#'     game_state$scoredf[[game_state$posstm]] <-
#'       as.numeric(game_state$scoredf[[game_state$posstm]]) + touchdown(game_state)
#'     game_state <- posschange(game_state)
#'     game_state$down <- 1
#'     game_state$togo <- 10
#'     game_state$ydsbef <- 70
#'     game_state <- togocorrector(game_state)
#'     game_state <- posstmmargin_updater(game_state)
#'     game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
#'     game_state <- playtimecorrector(game_state)
#'     game_state$afterplaytime <- 0
#'     game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'     game_state <- quartercheck(game_state)
#'     return(game_state)
#'   }
#'
#'   firstdown <- function(game_state, playtimemean, playtimesd){
#'     game_state$down <- 1
#'     game_state$togo <- 10
#'     game_state$ydsbef <- game_state$ydsbef - play$yards
#'     game_state <- togocorrector(game_state)
#'     game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
#'     game_state <- playtimecorrector(game_state)
#'     game_state <- afterplaytimegenerator(game_state)
#'     game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'     game_state <- quartercheck(game_state)
#'     return(game_state)
#'   }
#'
#'   turnover_on_downs <- function(game_state, playtimemean, playtimesd){
#'     game_state <- posschange(game_state)
#'     game_state$down <- 1
#'     game_state$togo <- 10
#'     game_state$ydsbef <- 100 - (game_state$ydsbef - play$yards)
#'     game_state <- togocorrector(game_state)
#'     game_state <- posstmmargin_updater(game_state)
#'     game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
#'     game_state <- playtimecorrector(game_state)
#'     game_state$afterplaytime <- 0
#'     game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'     game_state <- quartercheck(game_state)
#'     return(game_state)
#'   }
#'
#'   regular_gain <- function(game_state, playtimemean, playtimesd){
#'     game_state$down <- game_state$down + 1
#'     game_state$togo <- game_state$togo - play$yards
#'     game_state$ydsbef <- game_state$ydsbef - play$yards
#'     game_state <- togocorrector(game_state)
#'     game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
#'     game_state <- playtimecorrector(game_state)
#'     game_state <- afterplaytimegenerator(game_state)
#'     game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'     game_state <- quartercheck(game_state)
#'     return(game_state)
#'   }
#'
#'   made_fg <- function(game_state){
#'     game_state$option <- "field goal attempt"
#'     game_state$scoredf[[game_state$posstm]] <-
#'       as.numeric(game_state$scoredf[[game_state$posstm]]) + 3
#'     game_state <- posschange(game_state)
#'     game_state$down <- 1
#'     game_state$togo <- 10
#'     game_state$ydsbef <- 70
#'     game_state <- togocorrector(game_state)
#'     game_state <- posstmmargin_updater(game_state)
#'     game_state$playtime <- round(rnorm(1, 5, 1))
#'     game_state <- playtimecorrector(game_state)
#'     game_state$afterplaytime <- 0
#'     game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'     game_state <- quartercheck(game_state)
#'     return(game_state)
#'   }
#'
#'   missed_fg <- function(game_state){
#'     game_state$option <- "field goal attempt"
#'     game_state <- posschange(game_state)
#'     game_state$down <- 1
#'     game_state$togo <- 10
#'     game_state$ydsbef <- (100-game_state$ydsbef) - 5
#'     game_state <- togocorrector(game_state)
#'     game_state <- posstmmargin_updater(game_state)
#'     game_state$playtime <- round(rnorm(1, 5, 1))
#'     game_state <- playtimecorrector(game_state)
#'     game_state$afterplaytime <- 0
#'     game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'     game_state <- quartercheck(game_state)
#'     return(game_state)
#'   }
#'
#'   punt <- function(game_state){
#'     game_state$option <- "punt"
#'     game_state <- posschange(game_state)
#'     game_state$down <- 1
#'     game_state$togo <- 10
#'     game_state$ydsbef <- (100-game_state$ydsbef) + round(rnorm(1, 45, 5))
#'     ### TOUCHBACK
#'     if(game_state$ydsbef>=100){
#'       game_state$ydsbef <- 80
#'     }
#'     game_state <- togocorrector(game_state)
#'     game_state <- posstmmargin_updater(game_state)
#'     game_state$playtime <- round(rnorm(1, 9, 1.5))
#'     game_state <- playtimecorrector(game_state)
#'     game_state$afterplaytime <- 0
#'     game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'     game_state <- quartercheck(game_state)
#'     return(game_state)
#'   }
#'
#'   game_state <- posstmmargin_updater(game_state)
#'
#'   # Main game loop
#'   while(game_state$secondsleft > 0) {
#'     game_state$scoredf$Quarter <- game_state$quarter
#'     game_state$scoredf$SecondsLeft <- game_state$quartersecondsleft
#'     game_state$scoredf$Possession <- game_state$posstm
#'     game_state$scoredf$Down <- game_state$down
#'     game_state$scoredf$ToGo <- game_state$togo
#'     game_state$scoredf$YdstoEZ <- game_state$ydsbef
#'
#'     if(game_state$posstm==team1){
#'       off_dat <- whole_team1
#'       def_dat <- whole_team2
#'     } else if(game_state$posstm==team2){
#'       off_dat <- whole_team2
#'       def_dat <- whole_team1
#'     }
#'
#'
#'     if(game_state$down==4){
#'       if(game_state$ydsbef>40){
#'         ### GO FOR IT ON FOURTH
#'         if(game_state$quartersecondsleft<=240 & game_state$posstmmargin<0
#'            & game_state$togo<=10){
#'           game_state$option <- "goforit"
#'         } else{ ### PUNT
#'           game_state <- punt(game_state)
#'         }
#'       } else{
#'         ### GO FOR IT ON FOURTH
#'         if((game_state$quarter %in% c(2,4) & game_state$quartersecondsleft<=240 & game_state$
#'             posstmmargin<0 & game_state$togo<=3) |
#'            (game_state$quarter %in% c(2,4) & game_state$quartersecondsleft<=120 &
#'             game_state$posstmmargin<0)){
#'           game_state$option <- "goforit"
#'         } else{ ### FIELD GOAL
#'           game_state$option <- "field goal attempt"
#'           fgmakedf <- data_env$fgmakedf
#'           fgpredvardf <- data.frame(
#'             FGDist = game_state$ydsbef + 17,
#'             Quarter = game_state$quarter,
#'             Time2 = game_state$quartersecondsleft,
#'             PossTmMargin = game_state$posstmmargin,
#'             Down = game_state$down,
#'             ToGo = game_state$togo
#'           )
#'           fgattprob <- .9 # temporary to fix later (model was too big)
#'           fgattselection <- sample(c("Yes", "No"), 1, prob = c(fgattprob, 1-fgattprob))
#'           ### FGATT
#'           if(fgattselection=="Yes"){
#'             game_state$option <- "field goal attempt"
#'             fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==game_state$ydsbef+17]
#'             fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
#'             ### MADE FIELD GOAL
#'             if(fgmake=="Yes"){
#'               game_state <- made_fg(game_state)
#'               ### MISSED FIELD GOAL
#'             } else{
#'               game_state <- missed_fg(game_state)
#'             }
#'             ### GO FOR IT ON FOURTH
#'           } else{
#'             game_state$option <- "goforit"
#'           }
#'         }
#'       }
#'     } else{
#'       game_state$option <- "regular"
#'     }
#'     ### OTHER field goal scenario
#'     if((game_state$secondsleft<=10 & game_state$posstmmargin>=-3 &
#'         game_state$posstmmargin<=0) |
#'        (game_state$quarter==2 & game_state$quartersecondsleft<=10)){
#'       game_state$option <- "field goal attempt"
#'       fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==game_state$ydsbef+17]
#'       fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
#'       ### MADE FIELD GOAL
#'       if(fgmake=="Yes"){
#'         game_state <- made_fg(game_state)
#'         ### MISSED FIELD GOAL
#'       } else{
#'         game_state <- missed_fg(game_state)
#'       }
#'     }
#'
#'     if((game_state$down!=4 | game_state$option=="goforit") &
#'        game_state$option!="field goal attempt"){
#'
#'       play <- safeyardsgained(game_state$posstm, game_state$deftm, game_state$down,
#'                               game_state$togo, game_state$ydsbef, game_state$posstmmargin,
#'                               game_state$quartersecondsleft, game_state$quarter,
#'                               off_dat = off_dat, def_dat = def_dat, year = year)
#'     }
#'     if(play$result=="Sack" & play$yards > 0){
#'       play$yards <- -5
#'     }
#'     if(play$yards < -100){
#'       play$yards <- -100
#'     }
#'     if(play$yards > 100){
#'       play$yards <- 100
#'     }
#'     newyardsbef <- game_state$ydsbef - play$yards
#'     ################ PASS
#'     if(play$runpass=="Pass"){
#'       # INTERCEPTION
#'       if(play$result=="Interception"){
#'         ### PICK SIX
#'         if(newyardsbef>=100){
#'           game_state <- deftd(game_state, 8.5, .85)
#'           ### INTERCEPTION TOUCHBACK
#'         } else if(newyardsbef <= 0){
#'           game_state <- touchback(game_state, 5.5, .55)
#'           ### REGULAR INTERCEPTION
#'         } else{
#'           game_state <- reg_turnover(game_state, 6.5, .65)
#'         }
#'         # FUMBLE
#'       } else if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained",
#'                                    "Sack_Fumble_Lost", "Complete_Fumble_Lost")){
#'         ## FUMBLE LOST
#'         if(play$result %in% c("Sack_Fumble_Lost", "Complete_Fumble_Lost")){
#'           ### DEF TD
#'           if(newyardsbef>=100){
#'             game_state <- deftd(game_state, 5, .5)
#'             ### TOUCHBACK
#'           } else if(newyardsbef<=0){
#'             game_state <- touchback(game_state, 5, .5)
#'             ### REGULAR TURNOVER
#'           } else{
#'             game_state <- reg_turnover(game_state, 5, .5)
#'           }
#'           ## FUMBLE RETAINED
#'         } else if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained")){
#'           ### SAFETY
#'           if(newyardsbef>=100){
#'             game_state <- safety(game_state, 5.5, .5)
#'             ### OFF TD
#'           } else if(newyardsbef<=0){
#'             game_state <- offtd(game_state, 7.5, .75)
#'             ### FIRST DOWN
#'           } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
#'             game_state <- firstdown(game_state, 7.5, .75)
#'             ### TURNOVER ON DOWNS
#'           } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
#'                     & game_state$down==4){
#'             game_state <- turnover_on_downs(game_state, 6, .6)
#'             ### NON FIRST DOWN REGULAR PLAY
#'           } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
#'                     & game_state$down!=4){
#'             game_state <- regular_gain(game_state, 6, .6)
#'           } else{
#'             print("FUMBLE RETAINED SCENARIO NOT CAPTURED")
#'           }
#'         }
#'         # SACK
#'       } else if(play$result=="Sack"){
#'         ### SAFETY
#'         if(newyardsbef >= 100){
#'           game_state <- safety(game_state, 4.5, .45)
#'           ### TURNOVER ON DOWNS
#'         } else if(game_state$down==4 & (newyardsbef < 100)){
#'           game_state <- turnover_on_downs(game_state, 4.5, .45)
#'           ### REGULAR SACK
#'         } else if(game_state$down!=4 & (newyardsbef < 100)){
#'           game_state <- regular_gain(game_state, 4.5, .45)
#'         } else{
#'           print("SACK SCENARIO NOT CAPTURED")
#'         }
#'         # COMPLETE PASSES
#'       } else if(play$result=="Complete"){
#'         ### SAFETY
#'         if(newyardsbef>=100){
#'           game_state <- safety(game_state, 5, .5)
#'           ### OFF TOUCHDOWN
#'         } else if(newyardsbef<=0){
#'           game_state <- offtd(game_state, 7.5, .75)
#'           ### FIRST DOWN
#'         } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
#'           game_state <- firstdown(game_state, 7.5, .75)
#'           ### TURNOVER ON DOWNS
#'         } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
#'                   & game_state$down==4){
#'           game_state <- turnover_on_downs(game_state, 6.5, .65)
#'           ### REGULAR NON FOURTH DOWN NO FIRST DOWN NO TOUCHDOWN
#'         } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
#'                   & game_state$down!=4){
#'           game_state <- regular_gain(game_state, 6.5, .65)
#'         } else{
#'           print("COMPLETE PASSES SCENARIO NOT CAPTURED")
#'         }
#'         # INCOMPLETE PASSES
#'       } else if(play$result=="Incomplete"){
#'         ### TURNOVER ON DOWNS
#'         if(game_state$down==4){
#'           game_state <- turnover_on_downs(game_state, 6, .6)
#'           ### REGULAR INCOMPLETION
#'         } else if(game_state$down!=4){
#'           game_state$down <- game_state$down + 1
#'           game_state$playtime <- round(rnorm(1, 6, .6))
#'           game_state <- playtimecorrector(game_state)
#'           game_state$afterplaytime <- 0
#'           game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
#'           game_state <- quartercheck(game_state)
#'         } else{
#'           print("INCOMPLETE PASSES SCENARIO NOT CAPTURED")
#'         }
#'       } else{
#'         print("PASS SCENARIO NOT CAPTURED")
#'       }
#'       ################ RUN
#'     } else if(play$runpass=="Run"){
#'       # FUMBLE
#'       if(play$result %in% c("Fumble_Retained", "Fumble_Lost")){
#'         ## FUMBLE LOST
#'         if(play$result=="Fumble_Lost"){
#'           ### DEF TD
#'           if(newyardsbef>=100){
#'             game_state <- deftd(game_state, 5, .5)
#'             ### TOUCHBACK
#'           } else if(newyardsbef<=0){
#'             game_state <- touchback(game_state, 5, .5)
#'             ### REGULAR TURNOVER
#'           } else{
#'             game_state <- reg_turnover(game_state, 5, .5)
#'           }
#'           ## FUMBLE RETAINED
#'         } else if(play$result=="Fumble_Retained"){
#'           ### SAFETY
#'           if(newyardsbef>=100){
#'             game_state <- safety(game_state, 4.5, .45)
#'             ### OFF TD
#'           } else if(newyardsbef<=0){
#'             game_state <- offtd(game_state, 7.5, .75)
#'             ### FIRST DOWN
#'           } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
#'             game_state <- firstdown(game_state, 6.5, .65)
#'             ### TURNOVER ON DOWNS
#'           } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
#'                     & game_state$down==4){
#'             game_state <- turnover_on_downs(game_state, 4.5, .45)
#'             ### NON FIRST DOWN REGULAR PLAY
#'           } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
#'                     & game_state$down!=4){
#'             game_state <- regular_gain(game_state, 5, .5)
#'           } else{
#'             print("RUN FUMBLE RETAINED SCENARIO NOT CAPTURED")
#'           }
#'         }
#'         # NON FUMBLE RUNS
#'       } else if(play$result=="No_Fumble"){
#'         ### SAFETY
#'         if(newyardsbef>=100){
#'           game_state <- safety(game_state, 4.5, .45)
#'           ### OFF TD
#'         } else if(newyardsbef<=0){
#'           game_state <- offtd(game_state, 7.5, .75)
#'           ### FIRST DOWN
#'         } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
#'           game_state <- firstdown(game_state, 6, .6)
#'           ### TURNOVER ON DOWNS
#'         } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
#'                   & game_state$down==4){
#'           game_state <- turnover_on_downs(game_state, 4.5, .45)
#'           ### NON FIRST DOWN REGULAR PLAY
#'         } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
#'                   & game_state$down!=4){
#'           game_state <- regular_gain(game_state, 5, .5)
#'         } else{
#'           print("NON FUMBLE RUN SCENARIO NOT CAPTURED")
#'         }
#'       }
#'       else{
#'         print("RUN SCENARIO NOT CAPTURED")
#'       }
#'     }
#'     else{
#'       print("RUNPASS IS NOT A RUN OR PASS")
#'     }
#'     game_state$scoredf$Detail <- paste0(play$runpass, "; ", play$result, "; Yards: ", play$yards)
#'     game_state$scoredf <- game_state$scoredf |> relocate((Quarter:YdstoEZ), .before = all_of(team1))
#'     if(track %in% c("YES", "Y", "Yes", "yes", "y")){
#'       print(game_state$scoredf)
#'     }
#'   }
#'   return(game_state$scoredf)
#' }
#'
#' simulator("DEN", "LV", year = 2025, track = "YES")
#'
#'
#' multiple_simulations <- function(team1, team2, n = 100, max_attempts = 3) {
#'   resultlist <- vector("list", n)
#'   successful_runs <- 0
#'   attempts <- 0
#'
#'   while(successful_runs < n && attempts < n * max_attempts) {
#'     attempts <- attempts + 1
#'     start <- Sys.time()
#'     message(paste0("Attempt ", attempts, " Start Time: ", start))
#'
#'     # Initialize res as NULL before the try block
#'     res <- NULL
#'     timed_out <- FALSE
#'
#'     # Try with time limit
#'     try_result <- try({
#'       setTimeLimit(elapsed = 240, transient = TRUE)  # 4 minutes = 240 seconds
#'       res <- simulator(team1, team2)
#'       setTimeLimit(elapsed = Inf, transient = TRUE)  # Reset time limit
#'     }, silent = TRUE)
#'
#'     if(inherits(res, "try-error")) {
#'       # Check if the error was due to a timeout
#'       if(grepl("reached elapsed time limit", try_result[1])) {
#'         timed_out <- TRUE
#'         message("Simulation timed out after 4 minutes - retrying...")
#'       } else {
#'         message(sprintf("Simulation failed with error: %s", try_result[1]))
#'       }
#'       next
#'     }
#'
#'     successful_runs <- successful_runs + 1
#'     resultlist[[successful_runs]] <- res
#'     message(paste0("Completed ", successful_runs, "/", n, " in ",
#'                    difftime(Sys.time(), start, units = "secs"), " secs"))
#'   }
#'
#'   if(successful_runs == 0) {
#'     warning("All simulations failed")
#'     return(NULL)
#'   }
#'
#'   # Combine only successful runs
#'   combined_results <- do.call(rbind, resultlist[1:successful_runs])
#'
#'   data.frame(
#'     samplesize = successful_runs,
#'     team1 = team1,
#'     team1wins = sum(combined_results[[team1]] > combined_results[[team2]]),
#'     team1mean = mean(as.numeric(combined_results[[team1]])),
#'     team1sd = sd(as.numeric(combined_results[[team1]])),
#'     team2 = team2,
#'     team2wins = sum(combined_results[[team2]] > combined_results[[team1]]),
#'     team2mean = mean(as.numeric(combined_results[[team2]])),
#'     team2sd = sd(as.numeric(combined_results[[team2]]))
#'   )
#' }
#'
#' whole_week_simulations <- function(year, weeknum, tm1vec = c(), tm2vec = c()){
#'   if(is_empty(tm1vec) & is_empty(tm2vec)){
#'     weekdf <- nflreadr::load_schedules() |>
#'       filter(season == year & week == weeknum) |>
#'       select(season, week, away_team, home_team)
#'     tm1 <- weekdf$away_team
#'     tm2 <- weekdf$home_team
#'   } else{
#'     tm1 <- tm1vec
#'     tm2 <- tm2vec
#'   }
#'   all_simulations <- list()
#'   for(i in 1:length(tm1)){
#'     print(paste0("Team 1: ", tm1[i], " vs. Team 2: ", tm2[i]))
#'     simulation <- multiple_simulations(team1 = tm1[i], team2 = tm2[i])
#'     all_simulations <- list.append(all_simulations, simulation)
#'     print(simulation)
#'   }
#'   all_simulations
#' }
#'
#'
#'
#'
#'
#'
#' # ARCHIVE -----------------------------------------------------------------
#'
#' #' snaps_to_depth_chart <- function(team, def_assignment){
#' #'   dpt_cht <- depthchart(team)
#' #'   dpt_cht$cleaned_player <- clean_names(dpt_cht$player)
#' #'   dpt_cht <- dpt_cht |>
#' #'     mutate(position = case_when(
#' #'       position %in% c("LT", "LG", "C", "RG", "RT") ~ "OL",
#' #'       position %in% c("LDE", "NT", "RDE", "WLB", "LILB", "RILB", "SLB",
#' #'                       "LDT", "RDT", "MLB") ~ "F7",
#' #'       position %in% c("LCB", "SS", "FS", "RCB", "NB") ~ "DB",
#' #'       TRUE ~ position
#' #'     ))
#' #'   dpt_cht <- dpt_cht |>
#' #'     mutate(depth_next = case_when(
#' #'       final_position %in% c("WR1", "WR2", "WR3") ~ "WR1",
#' #'       final_position %in% c("WR4", "WR5", "WR6") ~ "WR2",
#' #'       final_position %in% c("WR7", "WR8", "WR9") ~ "WR3",
#' #'       final_position %in% c("WR10", "WR11", "WR12") ~ "WR4",
#' #'       final_position %in% c("LT1", "LG1", "C1", "RG1", "RT1") ~ "OL1",
#' #'       final_position %in% c("LT2", "LG2", "C2", "RG2", "RT2") ~ "OL2",
#' #'       final_position %in% c("LT3", "LG3", "C3", "RG3", "RT3") ~ "OL3",
#' #'       final_position %in% c("LDE1", "NT1", "RDE1", "WLB1", "LILB1", "RILB1", "SLB1",
#' #'                             "LDT1", "RDT1", "MLB1") ~ "F71",
#' #'       final_position %in% c("LDE2", "NT2", "RDE2", "WLB2", "LILB2", "RILB2", "SLB2",
#' #'                             "LCB2", "LDT2", "RDT2", "MLB2") ~ "F72",
#' #'       final_position %in% c("LDE3", "NT3", "RDE3", "WLB3", "LILB3", "RILB3", "SLB3",
#' #'                             "LDT3", "RDT3", "MLB3") ~ "F73",
#' #'       final_position %in% c("LCB1", "SS1", "FS1", "RCB1", "NB1") ~ "DB1",
#' #'       final_position %in% c("LCB2", "SS2", "FS2", "RCB2", "NB2") ~ "DB2",
#' #'       final_position %in% c("LCB3", "SS3", "FS3", "RCB3", "NB3") ~ "DB3",
#' #'       TRUE ~ final_position
#' #'     ))
#' #'
#' #'   ### QBs
#' #'   qbPFF <- qbsnapsdf2024
#' #'   qbPFF$cleaned_player <- clean_names(qbPFF$player)
#' #'   qbPFF <- qbPFF[-1]
#' #'   dpt_cht <- suppressMessages(left_join(dpt_cht, qbPFF))
#' #'   dpt_cht <- dpt_cht |>
#' #'     mutate(mean_att = if_else(position=="QB" & is.na(mean_att), 1, mean_att))
#' #'   colnames(dpt_cht)[colnames(dpt_cht)=="mean_att"] <- "attempts"
#' #'   dpt_cht_QB <- dpt_cht |>
#' #'     filter(position=="QB") |>
#' #'     mutate(depth_next = "QB")
#' #'   dpt_cht_QB$predicted_QB_attempts = predict(rush_att_model,dpt_cht_QB)
#' #'   dpt_cht_QB <- dpt_cht_QB |>
#' #'     dplyr::select(-c(depth_next, attempts))
#' #'   dpt_cht <- suppressMessages(left_join(dpt_cht, dpt_cht_QB))
#' #'   dpt_cht <- dpt_cht |>
#' #'     select(-attempts)
#' #'
#' #'   ### RBs
#' #'   rushingPFF <- rushdf
#' #'   rushingPFF$cleaned_player <- clean_names(rushingPFF$player)
#' #'   rushingPFF <- rushingPFF[-1]
#' #'   dpt_cht <- suppressMessages(left_join(dpt_cht, rushingPFF))
#' #'   dpt_cht <- dpt_cht |>
#' #'     mutate(mean_att = if_else(position=="RB" & is.na(mean_att), 1, mean_att))
#' #'   colnames(dpt_cht)[colnames(dpt_cht)=="mean_att"] <- "attempts"
#' #'   dpt_cht_RB <- dpt_cht |>
#' #'     filter(position=="RB") |>
#' #'     filter(depth_next %in% c("RB1", "RB2", "RB3"))
#' #'   dpt_cht_RB$predicted_attempts = predict(rush_att_model,dpt_cht_RB)
#' #'   dpt_cht <- suppressMessages(left_join(dpt_cht, dpt_cht_RB))
#' #'
#' #'   ### WRs/TEs
#' #'   receivingPFF <- recdf
#' #'   receivingPFF$cleaned_player <- clean_names(receivingPFF$player)
#' #'   receivingPFF <- receivingPFF[-1]
#' #'   dpt_cht <- suppressMessages(left_join(dpt_cht, receivingPFF))
#' #'   dpt_cht <- dpt_cht |>
#' #'     mutate(mean_routes = if_else(position %in% c("WR", "TE") & is.na(mean_routes), 5, mean_routes))
#' #'   colnames(dpt_cht)[colnames(dpt_cht)=="mean_routes"] <- "routes"
#' #'   dpt_cht_rec <- dpt_cht |>
#' #'     filter(position %in% c("WR", "TE")) |>
#' #'     filter(depth_next %in% c("WR1", "WR2", "WR3", "TE1", "TE2", "TE3"))
#' #'   dpt_cht_rec$predicted_routes = predict(rec_route_model,dpt_cht_rec)
#' #'   dpt_cht <- suppressMessages(left_join(dpt_cht, dpt_cht_rec))
#' #'
#' #'   ### OL
#' #'   blockingPFF <- blockdf
#' #'   blockingPFF$cleaned_player <- clean_names(blockingPFF$player)
#' #'   blockingPFF <- blockingPFF[-1]
#' #'   dpt_cht <- suppressMessages(left_join(dpt_cht, blockingPFF))
#' #'   dpt_cht <- dpt_cht |>
#' #'     mutate(mean_snaps = if_else(position=="OL" & is.na(mean_snaps), 5, mean_snaps))
#' #'   colnames(dpt_cht)[colnames(dpt_cht)=="mean_snaps"] <- "snap_counts_offense"
#' #'   dpt_cht_block <- dpt_cht |>
#' #'     filter(position %in% c("OL")) |>
#' #'     filter(depth_next %in% c("OL1", "OL2", "OL3"))
#' #'   dpt_cht_block$predicted_snaps = predict(block_model,dpt_cht_block)
#' #'   dpt_cht <- suppressMessages(left_join(dpt_cht, dpt_cht_block))
#' #'
#' #'   ### Defense
#' #'   defPFF <- defdf
#' #'   defPFF$cleaned_player <- clean_names(defPFF$player)
#' #'   defPFF <- defPFF[-1]
#' #'   dpt_cht <- suppressMessages(left_join(dpt_cht, defPFF))
#' #'   dpt_cht <- dpt_cht |>
#' #'     mutate(mean_cov_snaps = if_else(position %in% c("F7", "DB") & is.na(mean_cov_snaps),
#' #'                                     5, mean_cov_snaps),
#' #'            mean_pass_rush_snaps = if_else(position %in% c("F7", "DB") & is.na(mean_pass_rush_snaps),
#' #'                                           5, mean_pass_rush_snaps),
#' #'            mean_run_def_snaps = if_else(position %in% c("F7", "DB") & is.na(mean_run_def_snaps),
#' #'                                         5, mean_run_def_snaps))
#' #'   colnames(dpt_cht)[colnames(dpt_cht)=="mean_cov_snaps"] <- "snap_counts_coverage"
#' #'   colnames(dpt_cht)[colnames(dpt_cht)=="mean_run_def_snaps"] <- "snap_counts_run_defense"
#' #'   colnames(dpt_cht)[colnames(dpt_cht)=="mean_pass_rush_snaps"] <- "snap_counts_pass_rush"
#' #'   dpt_cht_def <- dpt_cht |>
#' #'     filter(position %in% c("F7", "DB")) |>
#' #'     filter(depth_next %in% c("F71", "F72", "F73", "DB1", "DB2", "DB3"))
#' #'   dpt_cht_def$predicted_cov_snaps = predict(def_cover_model,dpt_cht_def)
#' #'   dpt_cht_def$predicted_run_def_snaps = predict(def_run_model,dpt_cht_def)
#' #'   dpt_cht_def$predicted_pass_rush_snaps = predict(def_pass_rush_model,dpt_cht_def)
#' #'   dpt_cht <- suppressMessages(left_join(dpt_cht, dpt_cht_def))
#' #'
#' #'   dpt_cht <- dpt_cht |>
#' #'     mutate(final_snap_ct = case_when(
#' #'       def_assignment=="coverage" ~ coalesce(attempts, routes, snap_counts_offense, snap_counts_coverage),
#' #'       def_assignment=="run_defense" ~ coalesce(attempts, routes, snap_counts_offense, snap_counts_run_defense),
#' #'       def_assignment=="pass_rush" ~ coalesce(attempts, routes, snap_counts_offense, snap_counts_pass_rush)
#' #'     ),
#' #'     final_predicted_snap_ct = case_when(
#' #'       def_assignment=="coverage" ~ coalesce(predicted_QB_attempts, predicted_attempts, predicted_routes,
#' #'                                             predicted_snaps, predicted_cov_snaps),
#' #'       def_assignment=="run_defense" ~ coalesce(predicted_QB_attempts, predicted_attempts, predicted_routes,
#' #'                                                predicted_snaps, predicted_run_def_snaps),
#' #'       def_assignment=="pass_rush" ~ coalesce(predicted_QB_attempts, predicted_attempts, predicted_routes,
#' #'                                              predicted_snaps, predicted_pass_rush_snaps)
#' #'     ))
#' #'
#' #'   dpt_cht_final <- dpt_cht[c("position", "player", "cleaned_player", "final_position",
#' #'                              "depth_next", "pff_id",
#' #'                              "final_predicted_snap_ct")]
#' #'   dpt_cht_final <- dpt_cht_final |>
#' #'     filter(!(position!="QB" & is.na(final_predicted_snap_ct))) |>
#' #'     filter(player!="-")
#' #'   dpt_cht_final
#' #' }
#' #'
#'
#'
#' # # PFF Grade Predictor Functions -------------------------------------------
#' # QB_PFF_grade <- function(year){
#' #
#' #   last_data = PFF_data_grabber("QBs", year - 1)
#' #
#' #   PFF_passing_year_last <- last_data |>
#' #     filter(position=="QB" & attempts >= 10) |>
#' #     group_by(player_id) |>
#' #     summarise(player = player[1],
#' #               Year = year,
#' #               position = position[1],
#' #               mean_pass_attempts = round(mean(attempts, na.rm = T), 2),
#' #               pass_grade_mean_last = mean(grades_pass, na.rm = T),
#' #               pass_grade_sd = sd(grades_pass, na.rm = T),
#' #               rush_grade_mean_last = mean(grades_run, na.rm = T),
#' #               rush_grade_sd = sd(grades_run, na.rm = T),
#' #               games_last = n())
#' #
#' #   cur_data <- PFF_data_grabber("QBs", 2025)
#' #
#' #   PFF_passing_year_cur <- cur_data
#' #
#' #   if(nrow(PFF_passing_year_cur) > 1){
#' #     PFF_passing_year_cur <- PFF_passing_year_cur |>
#' #       filter(position=="QB" & attempts >= 10) |>
#' #       group_by(player_id) |>
#' #       summarise(player = player[1],
#' #                 Week = Week[1],
#' #                 mean_pass_attempts_cur = mean(attempts, na.rm = T),
#' #                 pass_grade_mean_cur = mean(grades_pass, na.rm = T),
#' #                 rush_grade_mean_cur = mean(grades_run, na.rm = T),
#' #                 pass_grade_sd_cur = sd(grades_pass, na.rm = T),
#' #                 rush_grade_sd_cur = sd(grades_run, na.rm = T),
#' #                 games_cur = n())
#' #     total_pff <- suppressMessages(full_join(PFF_passing_year_last, PFF_passing_year_cur))
#' #     cur_weight <- unique(PFF_passing_year_cur$Week) * .2
#' #     cur_weight <- max(1, cur_weight)
#' #     total_pff <- total_pff |>
#' #       mutate(pass_grade_mean = case_when(
#' #         is.na(pass_grade_mean_last) ~ pass_grade_mean_cur,
#' #         is.na(pass_grade_mean_cur) ~ pass_grade_mean_last,
#' #         TRUE ~ (cur_weight * pass_grade_mean_cur) +
#' #           ((1 - cur_weight) * pass_grade_mean_last)
#' #       ),
#' #       rush_grade_mean = case_when(
#' #         is.na(rush_grade_mean_last) ~ rush_grade_mean_cur,
#' #         is.na(rush_grade_mean_cur) ~ rush_grade_mean_last,
#' #         TRUE ~ (cur_weight * rush_grade_mean_cur) +
#' #           ((1 - cur_weight) * rush_grade_mean_last)
#' #       ),
#' #       games = case_when(
#' #         is.na(games_last) ~ games_cur,
#' #         is.na(games_cur) ~ games_last,
#' #         (games_cur + games_last) <= 17 ~ (games_cur + games_last),
#' #         TRUE ~ 17
#' #       ))
#' #   } else{
#' #     total_pff <- PFF_passing_year_last |>
#' #       mutate(pass_grade_mean = pass_grade_mean_last,
#' #              rush_grade_mean = rush_grade_mean_last,
#' #              games = games_last)
#' #   }
#' #   colnames(total_pff)[colnames(total_pff) == "player_id"] <- "pff_id"
#' #   total_pff$pff_id <- as.character(total_pff$pff_id)
#' #   with_age <- total_pff
#' #   with_age <- with_age |>
#' #     mutate(Year = if_else(is.na(Year), year, Year),
#' #            position = if_else(is.na(position), "QB", position))
#' #   with_age <- age_joiner(with_age)
#' #   with_age <- with_age |>
#' #     mutate(Age_next = Age + 1) |>
#' #     filter(position == "QB")
#' #   pass_predictions <- predict(.nfl_env$PFF_passing_grade_mean_model, with_age)
#' #   rush_predictions <- predict(.nfl_env$PFF_QB_rushing_grade_mean_model, with_age)
#' #   finaldf <- tibble(pff_id = with_age$pff_id,
#' #                     mean_pass_attempts = if_else(with_age$games_cur >= 3,
#' #                                                  with_age$mean_pass_attempts_cur,
#' #                                                  with_age$mean_pass_attempts),
#' #                     pass_grade_mean = pass_predictions,
#' #                     rush_grade_mean = rush_predictions,
#' #                     pass_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                             with_age$pass_grade_sd_cur,
#' #                                             with_age$pass_grade_sd),
#' #                     rush_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                             with_age$rush_grade_sd_cur,
#' #                                             with_age$rush_grade_sd))
#' #
#' #   ### DEPTH CHART PART
#' #   finaldf |>
#' #     mutate(mean_pass_attempts = if_else(is.na(mean_pass_attempts), 28, mean_pass_attempts),
#' #            rush_grade_mean = if_else(is.na(rush_grade_mean), 60, rush_grade_mean),
#' #            rush_grade_sd = if_else(is.na(rush_grade_sd), 15, rush_grade_sd),
#' #            pass_grade_mean = if_else(is.na(pass_grade_mean), 61.5, pass_grade_mean),
#' #            pass_grade_sd = if_else(is.na(pass_grade_sd), 17.5, pass_grade_sd))
#' # }
#' #
#' # RB_PFF_grade <- function(year){
#' #
#' #   last_data = PFF_data_grabber("Rushing", year - 1)
#' #
#' #   PFF_year_last <- last_rush_data |>
#' #     filter(position=="HB" & attempts >= 5) |>
#' #     group_by(player_id) |>
#' #     summarise(player = player[1],
#' #               Year = year,
#' #               position = position[1],
#' #               mean_rush_attempts = mean(attempts, na.rm = T),
#' #               rush_grade_mean_last = mean(grades_run, na.rm = T),
#' #               rush_grade_sd = sd(grades_run, na.rm = T),
#' #               games_last = n())
#' #
#' #   last_rec_varname <- paste0("PFF_Receiving_", year - 1)
#' #   last_rec_data <- get(last_rec_varname, envir = parent.env(environment())) |>
#' #     filter(position=="HB") |>
#' #     group_by(player_id) |>
#' #     summarise(player = player[1],
#' #               Year = year,
#' #               position = position[1],
#' #               rec_grade_mean_last = mean(grades_pass_route, na.rm = T),
#' #               rec_grade_sd = sd(grades_pass_route, na.rm = T))
#' #   PFF_year_last <- suppressMessages(left_join(PFF_year_last, last_rec_data))
#' #   last_block_varname <- paste0("PFF_Blocking_", year - 1)
#' #   last_block_data <- get(last_block_varname, envir = parent.env(environment())) |>
#' #     filter(position=="HB") |>
#' #     group_by(player_id) |>
#' #     summarise(player = player[1],
#' #               Year = year,
#' #               position = position[1],
#' #               pass_block_grade_mean_last = mean(grades_pass_block, na.rm = T),
#' #               pass_block_grade_sd = sd(grades_pass_block, na.rm = T),
#' #               run_block_grade_mean_last = mean(grades_run_block, na.rm = T),
#' #               run_block_grade_sd = sd(grades_run_block, na.rm = T))
#' #   PFF_year_last <- suppressMessages(left_join(PFF_year_last, last_block_data))
#' #   PFF_year_last <- PFF_year_last |>
#' #     mutate(position = if_else(position=="HB", "RB", position))
#' #   cur_rush_varname <- paste0("PFF_Rushing_", year)
#' #   cur_rush_data <- get(cur_rush_varname, envir = parent.env(environment()))
#' #   PFF_year_cur <- cur_rush_data
#' #   if(nrow(PFF_year_cur) > 1){
#' #     PFF_year_cur <- PFF_year_cur |>
#' #       filter(position=="HB" & attempts >= 5) |>
#' #       group_by(player_id) |>
#' #       summarise(player = player[1],
#' #                 position = position[1],
#' #                 Week = Week[1],
#' #                 rush_grade_mean_cur = mean(grades_run, na.rm = T),
#' #                 rush_grade_sd_cur = sd(grades_run, na.rm = T),
#' #                 games_cur = n())
#' #     cur_rec_varname <- paste0("PFF_Receiving_", year)
#' #     cur_rec_data <- get(cur_rec_varname, envir = parent.env(environment())) |>
#' #       filter(position=="HB") |>
#' #       group_by(player_id) |>
#' #       summarise(player = player[1],
#' #                 position = position[1],
#' #                 Week = Week[1],
#' #                 rec_grade_mean_cur = mean(grades_pass_route, na.rm = T),
#' #                 rec_grade_sd_cur = sd(grades_pass_route, na.rm = T))
#' #     PFF_year_cur <- suppressMessages(left_join(PFF_year_cur, cur_rec_data))
#' #     cur_block_varname <- paste0("PFF_Blocking_", year)
#' #     cur_block_data <- get(cur_block_varname, envir = parent.env(environment())) |>
#' #       filter(position=="HB") |>
#' #       group_by(player_id) |>
#' #       summarise(player = player[1],
#' #                 position = position[1],
#' #                 Week = Week[1],
#' #                 pass_block_grade_mean_cur = mean(grades_pass_block, na.rm = T),
#' #                 run_block_grade_mean_cur = mean(grades_run_block, na.rm = T),
#' #                 pass_block_grade_sd_cur = sd(grades_pass_block, na.rm = T),
#' #                 run_block_grade_sd_cur = sd(grades_run_block, na.rm = T))
#' #     PFF_year_cur <- suppressMessages( left_join(PFF_year_cur, cur_block_data))
#' #     PFF_year_cur <- PFF_year_cur |>
#' #       mutate(position = if_else(position=="HB", "RB", position))
#' #     total_pff <- suppressMessages(full_join(PFF_year_last, PFF_year_cur))
#' #     cur_weight <- unique(PFF_year_cur$Week) * .2
#' #     cur_weight <- max(1, cur_weight)
#' #     total_pff <- total_pff |>
#' #       mutate(rush_grade_mean = case_when(
#' #         is.na(rush_grade_mean_last) ~ rush_grade_mean_cur,
#' #         is.na(rush_grade_mean_cur) ~ rush_grade_mean_last,
#' #         TRUE ~ (cur_weight * rush_grade_mean_cur) +
#' #           ((1 - cur_weight) * rush_grade_mean_last)
#' #       ),
#' #       rec_grade_mean = case_when(
#' #         is.na(rec_grade_mean_last) ~ rec_grade_mean_cur,
#' #         is.na(rec_grade_mean_cur) ~ rec_grade_mean_last,
#' #         TRUE ~ (cur_weight * rec_grade_mean_cur) +
#' #           ((1 - cur_weight) * rec_grade_mean_last)
#' #       ),
#' #       pass_block_grade_mean = case_when(
#' #         is.na(pass_block_grade_mean_last) ~ pass_block_grade_mean_cur,
#' #         is.na(pass_block_grade_mean_cur) ~ pass_block_grade_mean_last,
#' #         TRUE ~ (cur_weight * pass_block_grade_mean_cur) +
#' #           ((1 - cur_weight) * pass_block_grade_mean_last)
#' #       ),
#' #       run_block_grade_mean = case_when(
#' #         is.na(run_block_grade_mean_last) ~ run_block_grade_mean_cur,
#' #         is.na(run_block_grade_mean_cur) ~ run_block_grade_mean_last,
#' #         TRUE ~ (cur_weight * run_block_grade_mean_cur) +
#' #           ((1 - cur_weight) * run_block_grade_mean_last)
#' #       ),
#' #       games = case_when(
#' #         is.na(games_last) ~ games_cur,
#' #         is.na(games_cur) ~ games_last,
#' #         (games_cur + games_last) <= 17 ~ (games_cur + games_last),
#' #         TRUE ~ 17
#' #       ))
#' #   } else{
#' #     total_pff <- PFF_year_last |>
#' #       mutate(rush_grade_mean = rush_grade_mean_last,
#' #              rec_grade_mean = rec_grade_mean_last,
#' #              pass_block_grade_mean = pass_block_grade_mean_last,
#' #              run_block_grade_mean = run_block_grade_mean_last,
#' #              games = games_last)
#' #   }
#' #   colnames(total_pff)[colnames(total_pff) == "player_id"] <- "pff_id"
#' #   total_pff$pff_id <- as.character(total_pff$pff_id)
#' #
#' #   with_age <- total_pff
#' #   with_age <- with_age |>
#' #     mutate(Year = if_else(is.na(Year), year, Year),
#' #            position = if_else(is.na(position), "RB", position))
#' #   with_age <- age_joiner(with_age)
#' #   with_age <- with_age |>
#' #     mutate(Age_next = Age + 1)
#' #   rush_predictions <- predict(PFF_rushing_grade_mean_model, with_age)
#' #   rec_predictions <- predict(PFF_receiving_grade_mean_model, with_age)
#' #   passblockdf <- with_age |>
#' #     rename(block_grade_mean = pass_block_grade_mean)
#' #   passblock_predictions <- predict(PFF_pass_blocking_grade_mean_model, passblockdf)
#' #   runblockdf <- with_age |>
#' #     rename(block_grade_mean = run_block_grade_mean)
#' #   runblock_predictions <- predict(PFF_run_blocking_grade_mean_model, runblockdf)
#' #   finaldf <- tibble(pff_id = with_age$pff_id,
#' #                     rush_grade_mean = rush_predictions,
#' #                     rush_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                             with_age$rush_grade_sd_cur,
#' #                                             with_age$rush_grade_sd),
#' #                     rec_grade_mean = rec_predictions,
#' #                     rec_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                            with_age$rec_grade_sd_cur,
#' #                                            with_age$rec_grade_sd),
#' #                     pass_block_grade_mean = passblock_predictions,
#' #                     pass_block_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                                   with_age$pass_block_grade_sd_cur,
#' #                                                   with_age$pass_block_grade_sd),
#' #                     run_block_grade_mean = runblock_predictions,
#' #                     run_block_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                                  with_age$run_block_grade_sd_cur,
#' #                                                  with_age$run_block_grade_sd))
#' #
#' #   ### DEPTH CHART PART
#' #   runningbacks <- snaps_to_depth_chart(team, "coverage") |>
#' #     filter(position == "RB")
#' #
#' #   combined <- suppressMessages(left_join(runningbacks, finaldf))
#' #   combined <- combined |>
#' #     mutate(rush_grade_mean = if_else(is.na(rush_grade_mean), 62, rush_grade_mean),
#' #            rush_grade_sd = if_else(is.na(rush_grade_sd), 17.5, rush_grade_sd),
#' #            rec_grade_mean = if_else(is.na(rec_grade_mean), 59, rec_grade_mean),
#' #            rec_grade_sd = if_else(is.na(rec_grade_sd), 17.5, rec_grade_sd),
#' #            pass_block_grade_mean = if_else(is.na(pass_block_grade_mean), 57.5, pass_block_grade_mean),
#' #            pass_block_grade_sd = if_else(is.na(pass_block_grade_sd), 12.5, pass_block_grade_sd),
#' #            run_block_grade_mean = if_else(is.na(run_block_grade_mean), 57.5, run_block_grade_mean),
#' #            run_block_grade_sd = if_else(is.na(run_block_grade_sd), 12.5, run_block_grade_sd))
#' #   combined
#' # }
#' #
#' # WRorTE_PFF_grade <- function(team, year, pos){
#' #   last_rec_varname <- paste0("PFF_Receiving_", year - 1)
#' #   last_rec_data <- get(last_rec_varname, envir = parent.env(environment()))
#' #   PFF_year_last <- last_rec_data |>
#' #     filter(position==pos & routes >= 7) |>
#' #     group_by(player_id) |>
#' #     summarise(player = player[1],
#' #               Year = year,
#' #               position = position[1],
#' #               rec_grade_mean_last = mean(grades_pass_route, na.rm = T),
#' #               rec_grade_sd = sd(grades_pass_route, na.rm = T),
#' #               games_last = n())
#' #   last_block_varname <- paste0("PFF_Blocking_", year - 1)
#' #   last_block_data <- get(last_block_varname, envir = parent.env(environment())) |>
#' #     filter(position==pos) |>
#' #     group_by(player_id) |>
#' #     summarise(player = player[1],
#' #               Year = year,
#' #               position = position[1],
#' #               pass_block_grade_mean_last = mean(grades_pass_block, na.rm = T),
#' #               pass_block_grade_sd = sd(grades_pass_block, na.rm = T),
#' #               run_block_grade_mean_last = mean(grades_run_block, na.rm = T),
#' #               run_block_grade_sd = sd(grades_run_block, na.rm = T))
#' #   PFF_year_last <- suppressMessages(left_join(PFF_year_last, last_block_data))
#' #
#' #   cur_rec_varname <- paste0("PFF_Receiving_", year)
#' #   cur_rec_data <- get(cur_rec_varname, envir = parent.env(environment()))
#' #   PFF_year_cur <- cur_rec_data
#' #   if(nrow(PFF_year_cur) > 1){
#' #     PFF_year_cur <- PFF_year_cur |>
#' #       filter(position==pos & routes >= 7) |>
#' #       group_by(player_id) |>
#' #       summarise(player = player[1],
#' #                 position = position[1],
#' #                 Week = Week[1],
#' #                 rec_grade_mean_cur = mean(grades_pass_route, na.rm = T),
#' #                 rec_grade_sd_cur = sd(grades_pass_route, na.rm = T),
#' #                 games_cur = n())
#' #     cur_block_varname <- paste0("PFF_Blocking_", year)
#' #     cur_block_data <- get(cur_block_varname, envir = parent.env(environment())) |>
#' #       filter(position==pos) |>
#' #       group_by(player_id) |>
#' #       summarise(player = player[1],
#' #                 pass_block_grade_mean_cur = mean(grades_pass_block, na.rm = T),
#' #                 pass_block_grade_sd_cur = sd(grades_pass_block, na.rm = T),
#' #                 run_block_grade_mean_cur = mean(grades_run_block, na.rm = T),
#' #                 run_block_grade_sd_cur = sd(grades_run_block, na.rm = T))
#' #     PFF_year_cur <- suppressMessages(left_join(PFF_year_cur, cur_block_data))
#' #     total_pff <- suppressMessages(left_join(PFF_year_last, PFF_year_cur))
#' #     cur_weight <- unique(PFF_year_cur$Week) * .2
#' #     cur_weight <- max(1, cur_weight)
#' #     total_pff <- total_pff |>
#' #       mutate(rec_grade_mean = case_when(
#' #         is.na(rec_grade_mean_last) ~ rec_grade_mean_cur,
#' #         is.na(rec_grade_mean_cur) ~ rec_grade_mean_last,
#' #         TRUE ~ (cur_weight * rec_grade_mean_cur) +
#' #           ((1 - cur_weight) * rec_grade_mean_last)
#' #       ),
#' #       pass_block_grade_mean = case_when(
#' #         is.na(pass_block_grade_mean_last) ~ pass_block_grade_mean_cur,
#' #         is.na(pass_block_grade_mean_cur) ~ pass_block_grade_mean_last,
#' #         TRUE ~ (cur_weight * pass_block_grade_mean_cur) +
#' #           ((1 - cur_weight) * pass_block_grade_mean_last)
#' #       ),
#' #       run_block_grade_mean = case_when(
#' #         is.na(run_block_grade_mean_last) ~ run_block_grade_mean_cur,
#' #         is.na(run_block_grade_mean_cur) ~ run_block_grade_mean_last,
#' #         TRUE ~ (cur_weight * run_block_grade_mean_cur) +
#' #           ((1 - cur_weight) * run_block_grade_mean_last)
#' #       ),
#' #       games = case_when(
#' #         is.na(games_last) ~ games_cur,
#' #         is.na(games_cur) ~ games_last,
#' #         (games_cur + games_last) <= 17 ~ (games_cur + games_last),
#' #         TRUE ~ 17
#' #       ))
#' #   } else{
#' #     total_pff <- PFF_year_last |>
#' #       mutate(rec_grade_mean = rec_grade_mean_last,
#' #              pass_block_grade_mean = pass_block_grade_mean_last,
#' #              run_block_grade_mean = run_block_grade_mean_last,
#' #              games = games_last)
#' #   }
#' #   colnames(total_pff)[colnames(total_pff) == "player_id"] <- "pff_id"
#' #   total_pff$pff_id <- as.character(total_pff$pff_id)
#' #   with_age <- total_pff
#' #   with_age <- with_age |>
#' #     mutate(Year = if_else(is.na(Year), year, Year),
#' #            position = if_else(is.na(position), pos, position))
#' #   with_age <- age_joiner(with_age)
#' #   with_age <- with_age |>
#' #     mutate(Age_next = Age + 1)
#' #   rec_predictions <- predict(PFF_receiving_grade_mean_model, with_age)
#' #   passblockdf <- with_age |>
#' #     rename(block_grade_mean = pass_block_grade_mean)
#' #   passblock_predictions <- predict(PFF_pass_blocking_grade_mean_model, passblockdf)
#' #   runblockdf <- with_age |>
#' #     rename(block_grade_mean = run_block_grade_mean)
#' #   runblock_predictions <- predict(PFF_run_blocking_grade_mean_model, runblockdf)
#' #   finaldf <- tibble(pff_id = with_age$pff_id,
#' #                     rec_grade_mean = rec_predictions,
#' #                     rec_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                            with_age$rec_grade_sd_cur,
#' #                                            with_age$rec_grade_sd),
#' #                     pass_block_grade_mean = passblock_predictions,
#' #                     pass_block_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                                   with_age$pass_block_grade_sd_cur,
#' #                                                   with_age$pass_block_grade_sd),
#' #                     run_block_grade_mean = runblock_predictions,
#' #                     run_block_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                                  with_age$run_block_grade_sd_cur,
#' #                                                  with_age$run_block_grade_sd))
#' #
#' #   ### DEPTH CHART PART
#' #   players <- snaps_to_depth_chart(team, "coverage") |>
#' #     filter(position == pos)
#' #
#' #   combined <- suppressMessages(left_join(players, finaldf))
#' #   if(pos=="WR"){
#' #     combined <- combined |>
#' #       mutate(rec_grade_mean = if_else(is.na(rec_grade_mean), 62, rec_grade_mean),
#' #              rec_grade_sd = if_else(is.na(rec_grade_sd), 17.5, rec_grade_sd),
#' #              pass_block_grade_mean = if_else(is.na(pass_block_grade_mean), 59, pass_block_grade_mean),
#' #              pass_block_grade_sd = if_else(is.na(pass_block_grade_sd), 12.5, pass_block_grade_sd),
#' #              run_block_grade_mean = if_else(is.na(run_block_grade_mean), 59, run_block_grade_mean),
#' #              run_block_grade_sd = if_else(is.na(run_block_grade_sd), 12.5, run_block_grade_sd))
#' #   } else if(pos=="TE"){
#' #     combined <- combined |>
#' #       mutate(rec_grade_mean = if_else(is.na(rec_grade_mean), 60, rec_grade_mean),
#' #              rec_grade_sd = if_else(is.na(rec_grade_sd), 17.5, rec_grade_sd),
#' #              pass_block_grade_mean = if_else(is.na(pass_block_grade_mean), 62, pass_block_grade_mean),
#' #              pass_block_grade_sd = if_else(is.na(pass_block_grade_sd), 15, pass_block_grade_sd),
#' #              run_block_grade_mean = if_else(is.na(run_block_grade_mean), 62, run_block_grade_mean),
#' #              run_block_grade_sd = if_else(is.na(run_block_grade_sd), 15, run_block_grade_sd))
#' #   }
#' #   combined
#' # }
#' #
#' # OL_PFF_grade <- function(team, year){
#' #   last_varname <- paste0("PFF_Blocking_", year - 1)
#' #   last_data <- get(last_varname, envir = parent.env(environment()))
#' #   PFF_year_last <- last_data |>
#' #     mutate(position = if_else(position %in% c("T", "G", "C"), "OL", position)) |>
#' #     filter(position=="OL" & snap_counts_offense >= 15) |>
#' #     group_by(player_id) |>
#' #     summarise(player = player[1],
#' #               Year = year,
#' #               position = position[1],
#' #               pass_block_grade_mean_last = mean(grades_pass_block, na.rm = T),
#' #               pass_block_grade_sd = sd(grades_pass_block, na.rm = T),
#' #               run_block_grade_mean_last = mean(grades_run_block, na.rm = T),
#' #               run_block_grade_sd = sd(grades_run_block, na.rm = T),
#' #               games_last = n())
#' #   cur_varname <- paste0("PFF_Blocking_", year)
#' #   cur_data <- get(cur_varname, envir = parent.env(environment()))
#' #   PFF_year_cur <- cur_data
#' #   if(nrow(PFF_year_cur) > 1){
#' #     PFF_year_cur <- PFF_year_cur |>
#' #       mutate(position = if_else(position %in% c("T", "G", "C"), "OL", position)) |>
#' #       filter(position=="OL" & snap_counts_offense >= 15) |>
#' #       group_by(player_id) |>
#' #       summarise(player = player[1],
#' #                 position = position[1],
#' #                 Week = Week[1],
#' #                 pass_block_grade_mean_cur = mean(grades_pass_block, na.rm = T),
#' #                 pass_block_grade_sd_cur = sd(grades_pass_block, na.rm = T),
#' #                 run_block_grade_mean_cur = mean(grades_run_block, na.rm = T),
#' #                 run_block_grade_sd_cur = sd(grades_run_block, na.rm = T),
#' #                 games_cur = n())
#' #     total_pff <- suppressMessages(left_join(PFF_year_last, PFF_year_cur))
#' #     cur_weight <- unique(PFF_year_cur$Week) * .2
#' #     cur_weight <- max(1, cur_weight)
#' #     total_pff <- total_pff |>
#' #       mutate(pass_block_grade_mean = case_when(
#' #         is.na(pass_block_grade_mean_last) ~ pass_block_grade_mean_cur,
#' #         is.na(pass_block_grade_mean_cur) ~ pass_block_grade_mean_last,
#' #         TRUE ~ (cur_weight * pass_block_grade_mean_cur) +
#' #           ((1 - cur_weight) * pass_block_grade_mean_last)
#' #       ),
#' #       run_block_grade_mean = case_when(
#' #         is.na(run_block_grade_mean_last) ~ run_block_grade_mean_cur,
#' #         is.na(run_block_grade_mean_cur) ~ run_block_grade_mean_last,
#' #         TRUE ~ (cur_weight * run_block_grade_mean_cur) +
#' #           ((1 - cur_weight) * run_block_grade_mean_last)
#' #       ),
#' #       games = case_when(
#' #         is.na(games_last) ~ games_cur,
#' #         is.na(games_cur) ~ games_last,
#' #         (games_cur + games_last) <= 17 ~ (games_cur + games_last),
#' #         TRUE ~ 17
#' #       ))
#' #   } else{
#' #     total_pff <- PFF_year_last |>
#' #       mutate(pass_block_grade_mean = pass_block_grade_mean_last,
#' #              run_block_grade_mean = run_block_grade_mean_last,
#' #              games = games_last)
#' #   }
#' #   colnames(total_pff)[colnames(total_pff) == "player_id"] <- "pff_id"
#' #   total_pff$pff_id <- as.character(total_pff$pff_id)
#' #   with_age <- total_pff
#' #   with_age <- with_age |>
#' #     mutate(Year = if_else(is.na(Year), year, Year),
#' #            position = if_else(is.na(position), "OL", position))
#' #   with_age <- age_joiner(with_age)
#' #   with_age <- with_age |>
#' #     mutate(Age_next = Age + 1)
#' #   passblockdf <- with_age |>
#' #     rename(block_grade_mean = pass_block_grade_mean)
#' #   pass_block_predictions <- predict(PFF_pass_blocking_grade_mean_model, passblockdf)
#' #   runblockdf <- with_age |>
#' #     rename(block_grade_mean = run_block_grade_mean)
#' #   run_block_predictions <- predict(PFF_run_blocking_grade_mean_model, runblockdf)
#' #   finaldf <- tibble(pff_id = with_age$pff_id,
#' #                     pass_block_grade_mean = pass_block_predictions,
#' #                     run_block_grade_mean = run_block_predictions,
#' #                     pass_block_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                                   with_age$pass_block_grade_sd_cur,
#' #                                                   with_age$pass_block_grade_sd),
#' #                     run_block_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                                  with_age$run_block_grade_sd_cur,
#' #                                                  with_age$run_block_grade_sd))
#' #
#' #   ### DEPTH CHART PART
#' #   players <- snaps_to_depth_chart(team, "coverage") |>
#' #     filter(position == "OL")
#' #
#' #
#' #   combined <- suppressMessages(left_join(players, finaldf))
#' #   combined <- combined |>
#' #     mutate(run_block_grade_mean = if_else(is.na(run_block_grade_mean), 60, run_block_grade_mean),
#' #            run_block_grade_sd = if_else(is.na(run_block_grade_sd), 17.5, run_block_grade_sd),
#' #            pass_block_grade_mean = if_else(is.na(pass_block_grade_mean), 60, pass_block_grade_mean),
#' #            pass_block_grade_sd = if_else(is.na(pass_block_grade_sd), 17.5, pass_block_grade_sd))
#' #   combined
#' # }
#' #
#' # ## for testing
#' # team <- "DEN"
#' # year <- 2025
#' # pos <- "DB"
#' # ##
#' #
#' # Defense_PFF_grade <- function(team, year, pos){
#' #   last_def_varname <- paste0("PFF_Defense_", year - 1)
#' #   last_def_data <- get(last_def_varname, envir = parent.env(environment()))
#' #   PFF_year_last <- last_def_data |>
#' #     mutate(position = case_when(
#' #       position %in% c("DI", "ED", "LB") ~ "F7",
#' #       position %in% c("CB", "S") ~ "DB",
#' #       TRUE ~ position
#' #     ),
#' #     snap_counts_defense = snap_counts_coverage + snap_counts_pass_rush +
#' #       snap_counts_run_defense) |>
#' #     filter(position==pos & snap_counts_defense >= 15) |>
#' #     group_by(player_id) |>
#' #     summarise(player = player[1],
#' #               Year = year,
#' #               position = position[1],
#' #               def_cover_grade_mean_last = mean(grades_coverage_defense, na.rm = T),
#' #               def_cover_grade_sd = sd(grades_coverage_defense, na.rm = T),
#' #               def_pass_rush_grade_mean_last = mean(grades_pass_rush_defense, na.rm = T),
#' #               def_pass_rush_grade_sd = sd(grades_pass_rush_defense, na.rm = T),
#' #               def_run_defense_grade_mean_last = mean(grades_run_defense, na.rm = T),
#' #               def_run_defense_grade_sd = sd(grades_run_defense, na.rm = T),
#' #               games_last = n())
#' #   cur_def_varname <- paste0("PFF_Defense_", year)
#' #   cur_def_data <- get(cur_def_varname, envir = parent.env(environment()))
#' #   PFF_year_cur <- cur_def_data
#' #   if(nrow(PFF_year_cur) > 1){
#' #     PFF_year_cur <- PFF_year_cur |>
#' #       mutate(position = case_when(
#' #         position %in% c("DI", "ED", "LB") ~ "F7",
#' #         position %in% c("CB", "S") ~ "DB",
#' #         TRUE ~ position
#' #       ),
#' #       snap_counts_defense = snap_counts_coverage + snap_counts_pass_rush +
#' #         snap_counts_run_defense) |>
#' #       filter(position==pos & snap_counts_defense >= 15) |>
#' #       group_by(player_id) |>
#' #       summarise(player = player[1],
#' #                 position = position[1],
#' #                 Week = Week[1],
#' #                 def_cover_grade_mean_cur = mean(grades_coverage_defense, na.rm = T),
#' #                 def_pass_rush_grade_mean_cur = mean(grades_pass_rush_defense, na.rm = T),
#' #                 def_run_defense_grade_mean_cur = mean(grades_run_defense, na.rm = T),
#' #                 def_cover_grade_sd_cur = sd(grades_coverage_defense, na.rm = T),
#' #                 def_pass_rush_grade_sd_cur = sd(grades_pass_rush_defense, na.rm = T),
#' #                 def_run_defense_grade_sd_cur = sd(grades_run_defense, na.rm = T),
#' #                 games_cur = n())
#' #     total_pff <- suppressMessages(left_join(PFF_year_last, PFF_year_cur))
#' #     cur_weight <- unique(PFF_year_cur$Week) * .2
#' #     cur_weight <- max(1, cur_weight)
#' #     total_pff <- total_pff |>
#' #       mutate(def_coverage_grade_mean = case_when(
#' #         is.na(def_cover_grade_mean_last) ~ def_cover_grade_mean_cur,
#' #         is.na(def_cover_grade_mean_cur) ~ def_cover_grade_mean_last,
#' #         TRUE ~ (cur_weight * def_cover_grade_mean_cur) +
#' #           ((1 - cur_weight) * def_cover_grade_mean_last)
#' #       ),
#' #       def_pass_rush_grade_mean = case_when(
#' #         is.na(def_pass_rush_grade_mean_last) ~ def_pass_rush_grade_mean_cur,
#' #         is.na(def_pass_rush_grade_mean_cur) ~ def_pass_rush_grade_mean_last,
#' #         TRUE ~ (cur_weight * def_pass_rush_grade_mean_cur) +
#' #           ((1 - cur_weight) * def_pass_rush_grade_mean_last)
#' #       ),
#' #       def_run_grade_mean = case_when(
#' #         is.na(def_run_defense_grade_mean_last) ~ def_run_defense_grade_mean_cur,
#' #         is.na(def_run_defense_grade_mean_cur) ~ def_run_defense_grade_mean_last,
#' #         TRUE ~ (cur_weight * def_run_defense_grade_mean_cur) +
#' #           ((1 - cur_weight) * def_run_defense_grade_mean_last)
#' #       ),
#' #       games = case_when(
#' #         is.na(games_last) ~ games_cur,
#' #         is.na(games_cur) ~ games_last,
#' #         (games_cur + games_last) <= 17 ~ (games_cur + games_last),
#' #         TRUE ~ 17
#' #       ))
#' #   } else{
#' #     total_pff <- PFF_year_last |>
#' #       mutate(def_coverage_grade_mean = def_cover_grade_mean_last,
#' #              def_pass_rush_grade_mean = def_pass_rush_grade_mean_last,
#' #              def_run_grade_mean = def_run_defense_grade_mean_last,
#' #              games = games_last)
#' #   }
#' #   colnames(total_pff)[colnames(total_pff) == "player_id"] <- "pff_id"
#' #   total_pff$pff_id <- as.character(total_pff$pff_id)
#' #   with_age <- total_pff
#' #   with_age <- with_age |>
#' #     mutate(Year = if_else(is.na(Year), year, Year),
#' #            position = if_else(is.na(position), "OL", position))
#' #   with_age <- age_joiner(with_age)
#' #   with_age <- with_age |>
#' #     mutate(Age_next = Age + 1)
#' #   def_coverage_predictions <- predict(PFF_def_coverage_grade_mean_model, with_age)
#' #   def_pass_rush_predictions <- predict(PFF_def_pass_rush_grade_mean_model, with_age)
#' #   def_run_defense_predictions <- predict(PFF_def_run_grade_mean_model, with_age)
#' #   finaldf <- tibble(pff_id = with_age$pff_id,
#' #                     def_coverage_grade_mean = def_coverage_predictions,
#' #                     def_coverage_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                                     with_age$def_cover_grade_sd_cur,
#' #                                                     with_age$def_cover_grade_sd),
#' #                     def_pass_rush_grade_mean = def_pass_rush_predictions,
#' #                     def_pass_rush_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                                      with_age$def_pass_rush_grade_sd_cur,
#' #                                                      with_age$def_pass_rush_grade_sd),
#' #                     def_run_defense_grade_mean = def_run_defense_predictions,
#' #                     def_run_defense_grade_sd = if_else(with_age$games_cur >= 3,
#' #                                                        with_age$def_run_defense_grade_sd_cur,
#' #                                                        with_age$def_run_defense_grade_sd))
#' #
#' #   ### DEPTH CHART PART
#' #   cover_players <- snaps_to_depth_chart(team, "coverage") |>
#' #     filter(position == pos) |>
#' #     rename(predicted_coverage_snaps = final_predicted_snap_ct)
#' #   pass_rush_players <- snaps_to_depth_chart(team, "pass_rush") |>
#' #     filter(position == pos) |>
#' #     rename(predicted_pass_rush_snaps = final_predicted_snap_ct)
#' #   run_defense_players <- snaps_to_depth_chart(team, "run_defense") |>
#' #     filter(position == pos) |>
#' #     rename(predicted_run_defense_snaps = final_predicted_snap_ct)
#' #
#' #   players <- suppressMessages(left_join(cover_players, pass_rush_players))
#' #   players <- suppressMessages(left_join(players, run_defense_players))
#' #
#' #   combined <- suppressMessages(left_join(players, finaldf))
#' #   if(pos=="DB"){
#' #     combined <- combined |>
#' #       mutate(def_coverage_grade_mean = if_else(is.na(def_coverage_grade_mean), 62, def_coverage_grade_mean),
#' #              def_coverage_grade_sd = if_else(is.na(def_coverage_grade_sd), 17.5, def_coverage_grade_sd),
#' #              def_pass_rush_grade_mean = if_else(is.na(def_pass_rush_grade_mean), 59, def_pass_rush_grade_mean),
#' #              def_pass_rush_grade_sd = if_else(is.na(def_pass_rush_grade_sd), 12.5, def_pass_rush_grade_sd),
#' #              def_run_defense_grade_mean = if_else(is.na(def_run_defense_grade_mean), 60, def_run_defense_grade_mean),
#' #              def_run_defense_grade_sd = if_else(is.na(def_run_defense_grade_sd), 17.5, def_run_defense_grade_sd))
#' #   } else if(pos=="F7"){
#' #     combined <- combined |>
#' #       mutate(def_coverage_grade_mean = if_else(is.na(def_coverage_grade_mean), 59, def_coverage_grade_mean),
#' #              def_coverage_grade_sd = if_else(is.na(def_coverage_grade_sd), 17.5, def_coverage_grade_sd),
#' #              def_pass_rush_grade_mean = if_else(is.na(def_pass_rush_grade_mean), 62, def_pass_rush_grade_mean),
#' #              def_pass_rush_grade_sd = if_else(is.na(def_pass_rush_grade_sd), 17.5, def_pass_rush_grade_sd),
#' #              def_run_defense_grade_mean = if_else(is.na(def_run_defense_grade_mean), 62, def_run_defense_grade_mean),
#' #              def_run_defense_grade_sd = if_else(is.na(def_run_defense_grade_sd), 15, def_run_defense_grade_sd))
#' #   }
#' #   combined
#' # }
#'
#'
#'
#'
#'
#'
#'OLD ROUTE COMBINATION CODE
#'

# ### FIGURES OUT THE SPECIFIC ROUTES RAN
# anglect <- cornerct <- crossct <- flatct <- goct <- hitchct <- inct <-
#   outct <- postct <- screenct <- slantct <- wheelct <- 0
# chosenroutelist <- c()
# for(n in 1:routesrun){
#   modeldat <- modeldat |>
#     mutate(ANGLE_count = anglect,
#            CORNER_count = cornerct,
#            CROSS_count = crossct,
#            FLAT_count = flatct,
#            GO_count = goct,
#            HITCH_count = hitchct,
#            IN_count = inct,
#            OUT_count = outct,
#            POST_count = postct,
#            SCREEN_count = screenct,
#            SLANT_count = slantct,
#            WHEEL_count = wheelct)
#
#   angleprobs <- predict(data_env$angle_model, modeldat, type = "probs")
#   cornerprobs <- predict(data_env$corner_model, modeldat, type = "probs")
#   crossprobs <- predict(data_env$cross_model, modeldat, type = "probs")
#   flatprobs <- predict(data_env$flat_model, modeldat, type = "probs")
#   goprobs <- predict(data_env$go_model, modeldat, type = "probs")
#   hitchprobs <- predict(data_env$hitch_model, modeldat, type = "probs")
#   inprobs <- predict(data_env$in_model, modeldat, type = "probs")
#   outprobs <- predict(data_env$out_model, modeldat, type = "probs")
#   postprobs <- predict(data_env$post_model, modeldat, type = "probs")
#   screenprobs <- predict(data_env$screen_model, modeldat, type = "probs")
#   slantprobs <- predict(data_env$slant_model, modeldat, type = "probs")
#   wheelprobs <- predict(data_env$wheel_model, modeldat, type = "probs")
#   allprobs <- c(angleprobs, cornerprobs, crossprobs, flatprobs, goprobs, hitchprobs,
#                 inprobs, outprobs, postprobs, screenprobs, slantprobs, wheelprobs)
#   probsdf <- as.data.frame(t(allprobs))
#   colnames(probsdf) <- c("a0", "a1", "a2", "co0", "co1", "co2", "co3", "cr0",
#                          "cr1", "cr2", "cr3", "cr4", "f0", "f1", "f2", "f3",
#                          "f4", "g0", "g1", "g2", "g3", "g4", "h0", "h1", "h2",
#                          "h3", "h4", "h5", "i0", "i1", "i2", "i3", "i4",
#                          "o0", "o1", "o2", "o3", "o4", "p0", "p1", "p2", "p3",
#                          "sc0", "sc1", "sc2", "sc3", "sc4", "sl0", "sl1",
#                          "sl2", "sl3", "sl4", "w0", "w1", "w2")
#   aprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("a", anglect+1))])
#   coprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("co", cornerct+1))])
#   crprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("cr", crossct+1))])
#   fprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("f", flatct+1))])
#   gprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("g", goct+1))])
#   hprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("h", hitchct+1))])
#   iprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("i", inct+1))])
#   oprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("o", outct+1))])
#   pprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("p", postct+1))])
#   scprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("sc", screenct+1))])
#   slprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("sl", slantct+1))])
#   wprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("w", wheelct+1))])
#   probs <- c(aprob, coprob, crprob, fprob, gprob, hprob, iprob, oprob, pprob,
#              scprob, slprob, wprob)
#   route_options <- c("angle", "corner", "cross", "flat", "go", "hitch", "in",
#                      "out", "post", "screen", "slant", "wheel")
#   chosen_route <- sample(route_options, 1, prob = probs)
#   varname <- paste0(chosen_route, "ct")
#   value <- get(varname)
#   chosenroutelist <- append(chosenroutelist, chosen_route)
#   assign(varname, get(varname) + 1)
# }

# # ---- INITIALIZE COUNTS ----
# route_counts <- c(
#   angle = 0, corner = 0, cross = 0, flat = 0, go = 0,
#   hitch = 0, in_route = 0, out = 0, post = 0, screen = 0,
#   slant = 0, wheel = 0
# )
#
# route_options <- names(route_counts)
# chosenroutelist <- character(0)
#
# # ---- ADD COUNTS TO MODELDAT ONCE ----
# modeldat <- modeldat |>
#   mutate(
#     ANGLE_count = 0, CORNER_count = 0, CROSS_count = 0,
#     FLAT_count = 0, GO_count = 0, HITCH_count = 0,
#     IN_count = 0, OUT_count = 0, POST_count = 0,
#     SCREEN_count = 0, SLANT_count = 0, WHEEL_count = 0
#   )
#
# # ---- PRECOMPUTE ALL PREDICTIONS ONCE ----
# angleprobs  <- predict(data_env$angle_model, modeldat, type = "probs")
# cornerprobs <- predict(data_env$corner_model, modeldat, type = "probs")
# crossprobs  <- predict(data_env$cross_model, modeldat, type = "probs")
# flatprobs   <- predict(data_env$flat_model, modeldat, type = "probs")
# goprobs     <- predict(data_env$go_model, modeldat, type = "probs")
# hitchprobs  <- predict(data_env$hitch_model, modeldat, type = "probs")
# inprobs     <- predict(data_env$in_model, modeldat, type = "probs")
# outprobs    <- predict(data_env$out_model, modeldat, type = "probs")
# postprobs   <- predict(data_env$post_model, modeldat, type = "probs")
# screenprobs <- predict(data_env$screen_model, modeldat, type = "probs")
# slantprobs  <- predict(data_env$slant_model, modeldat, type = "probs")
# wheelprobs  <- predict(data_env$wheel_model, modeldat, type = "probs")
#
# # store in list for easy access
# prob_list <- list(
#   angle  = matrix(angleprobs,  nrow = 1),
#   corner = matrix(cornerprobs, nrow = 1),
#   cross  = matrix(crossprobs,  nrow = 1),
#   flat   = matrix(flatprobs,   nrow = 1),
#   go     = matrix(goprobs,     nrow = 1),
#   hitch  = matrix(hitchprobs,  nrow = 1),
#   in_route = matrix(inprobs,   nrow = 1),
#   out    = matrix(outprobs,    nrow = 1),
#   post   = matrix(postprobs,   nrow = 1),
#   screen = matrix(screenprobs, nrow = 1),
#   slant  = matrix(slantprobs,  nrow = 1),
#   wheel  = matrix(wheelprobs,  nrow = 1)
# )
#
# # ---- MAIN LOOP (NO PREDICT CALLS) ----
# for(n in seq_len(routesrun)){
#
#   probs <- numeric(length(route_options))
#
#   for(i in seq_along(route_options)){
#     route <- route_options[i]
#     count <- route_counts[route]
#
#     # grab correct column dynamically
#     probs[i] <- prob_list[[route]][, count + 1]
#   }
#
#   # normalize just in case
#   probs <- probs / sum(probs)
#
#   chosen_route <- sample(route_options, 1, prob = probs)
#
#   # update
#   route_counts[chosen_route] <- route_counts[chosen_route] + 1
#   chosenroutelist <- c(chosenroutelist, chosen_route)
# }

### FIGURES OUT THE SPECIFIC ROUTES RAN

# -----------------------------
# Initialize route state tracker
# -----------------------------
# route_counts <- c(
#   angle = 0, corner = 0, cross = 0, flat = 0, go = 0,
#   hitch = 0, in_route = 0, out = 0, post = 0,
#   screen = 0, slant = 0, wheel = 0
# )
#
# route_map <- c(
#   angle = "angle",
#   corner = "corner",
#   cross = "cross",
#   flat = "flat",
#   go = "go",
#   hitch = "hitch",
#   in_route = "in",
#   out = "out",
#   post = "post",
#   screen = "screen",
#   slant = "slant",
#   wheel = "wheel"
# )
#
# route_options <- names(route_counts)
# chosenroutelist <- character(0)
#
# # -----------------------------
# # Base model dat (NO mutating inside loop)
# # -----------------------------
# modeldat <- modeldat |>
#   mutate(
#     ANGLE_count = 0, CORNER_count = 0, CROSS_count = 0,
#     FLAT_count = 0, GO_count = 0, HITCH_count = 0,
#     IN_count = 0, OUT_count = 0, POST_count = 0,
#     SCREEN_count = 0, SLANT_count = 0, WHEEL_count = 0
#   )
#
# # -----------------------------
# # Precompute probabilities ONCE
# # -----------------------------
# prob_list <- list(
#   angle     = matrix(predict(data_env$angle_model, modeldat, type="probs"), nrow=1),
#   corner    = matrix(predict(data_env$corner_model, modeldat, type="probs"), nrow=1),
#   cross     = matrix(predict(data_env$cross_model, modeldat, type="probs"), nrow=1),
#   flat      = matrix(predict(data_env$flat_model, modeldat, type="probs"), nrow=1),
#   go        = matrix(predict(data_env$go_model, modeldat, type="probs"), nrow=1),
#   hitch     = matrix(predict(data_env$hitch_model, modeldat, type="probs"), nrow=1),
#   in_route  = matrix(predict(data_env$in_model, modeldat, type="probs"), nrow=1),
#   out       = matrix(predict(data_env$out_model, modeldat, type="probs"), nrow=1),
#   post      = matrix(predict(data_env$post_model, modeldat, type="probs"), nrow=1),
#   screen    = matrix(predict(data_env$screen_model, modeldat, type="probs"), nrow=1),
#   slant     = matrix(predict(data_env$slant_model, modeldat, type="probs"), nrow=1),
#   wheel     = matrix(predict(data_env$wheel_model, modeldat, type="probs"), nrow=1)
# )
#
# # -----------------------------
# # MAIN LOOP (FAST + SAFE)
# # -----------------------------
# for(n in seq_len(routesrun)){
#
#   probs <- numeric(length(route_options))
#
#   for(i in seq_along(route_options)){
#
#     route <- route_options[i]
#     count <- route_counts[[route]]
#
#     probs[i] <- prob_list[[route]][1, count + 1]
#   }
#
#   # safety normalization (prevents sample() errors)
#   probs[is.na(probs)] <- 0
#   if(sum(probs) == 0){
#     probs <- rep(1/length(probs), length(probs))
#   } else {
#     probs <- probs / sum(probs)
#   }
#
#   chosen_route <- sample(route_options, 1, prob = probs)
#
#   route_counts[[chosen_route]] <- route_counts[[chosen_route]] + 1
#
#   # convert internal name back to football name
#   chosenroutelist <- c(chosenroutelist, route_map[chosen_route])
# }
