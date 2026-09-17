#!/usr/bin/env Rscript
# Simulate synthetic ManyNumbers Dot Comparison (AMS) and Visual Memory (VM)
# task files, matching the columns documented in
# manynumbers_datadictionary_030326_V01.pdf ("Dot Comparison task" and
# "Visual Memory Task" sections). Companion to simulate_test_subjects.R,
# which covers the Give-N task; these two tasks aren't simulated anywhere
# else yet.
#
# The dictionary documents ~50-70 columns per task; most non-bold columns
# have no description ("-") because they're internal outputs of the real
# PsychoPy stimulus-generation/response-logging code, which isn't available
# here. For those, this script fills in internally-consistent derived values
# (e.g., surface area/contour computed from a simulated dot diameter) rather
# than leaving them blank, so the files "look real" -- but they are NOT an
# accurate re-implementation of the real stimulus generator. The columns the
# dictionary marks in bold (numRatio, accuracy, rt for AMS; clicks, objects,
# rt, accuracy for VM) come from real psychophysical models instead of
# placeholder noise:
#   - AMS: a Weber-fraction discrimination model. d' = |N1-N2| /
#     (w * sqrt(N1^2 + N2^2)), p(correct) = pnorm(d'). Smaller `weber_fraction`
#     = sharper numerical precision (typically improves with age).
#   - VM: a capacity-limited visual working-memory model. Each participant
#     has a fixed item capacity K; encoded ("occupied") screen positions are
#     correctly remembered with probability min(0.97, K / set_size), while
#     unoccupied positions are correctly rejected at a small constant
#     false-alarm rate. Assumes the probe phase re-displays the same
#     positions shown at encoding (a whole-display click/change-detection
#     task), so objects == clicks.
#
# Run from the repo root:
#   Rscript MN_code/simulate_test_subjects/simulate_ams_vm_subjects.R
# (writes its output csvs into MN_code/, alongside the Give-N test subjects
# produced by simulate_test_subjects.R)

out_dir <- "MN_code"

ams_ratios <- c(0.5, 0.6, 0.67, 0.75, 0.8) # standard AMS dot-comparison ratio set

compute_age <- function(dob, test_date) {
  age_days <- as.numeric(test_date - dob)
  age_years <- floor(age_days / 365.25)
  age_months <- floor(age_days / 30.44)
  age_group <- if (age_years <= 3) {
    "3-year-olds or younger"
  } else if (age_years == 4) {
    "4-year-olds"
  } else {
    "5-year-olds"
  }
  list(age_years = age_years, age_months = age_months, age_group = age_group)
}

# ---- Dot Comparison (AMS) task ---------------------------------------------
# One row per trial: sample (2) + practice (4) + three experimental blocks of
# 16 (48 total), matching "16 trials corresponds to one block. The total
# number of experimental trials are 48" in the dictionary.
simulate_ams_trials <- function(subject, seed) {
  set.seed(seed)

  block_defs <- data.frame(
    block = c("sample", "practice", "block1", "block2", "block3"),
    n_trials = c(2, 4, 16, 16, 16),
    stringsAsFactors = FALSE
  )
  trials <- data.frame(block = rep(block_defs$block, block_defs$n_trials), stringsAsFactors = FALSE)
  n <- nrow(trials)
  trials$test_trials <- seq_len(n)
  trials$block_displayed <- paste0(toupper(substr(trials$block, 1, 1)), substr(trials$block, 2, nchar(trials$block)))

  base_total <- sample(6:20, n, replace = TRUE)
  ratio <- sample(ams_ratios, n, replace = TRUE)
  smaller <- pmax(2, round(base_total * ratio / (1 + ratio)))
  larger <- pmax(smaller + 1, round(smaller / ratio))
  trials$numRatio <- round(smaller / larger, 3)

  trials$choice_right1 <- rbinom(n, 1, 0.5) # 1 = numerically larger array shown on the right
  trials$Num_1 <- ifelse(trials$choice_right1 == 1, smaller, larger) # left array
  trials$Num_2 <- ifelse(trials$choice_right1 == 1, larger, smaller) # right array

  # visual-control features, simulated (not measured): total surface area,
  # contour, and convex hull derived from number + an average dot diameter
  # that's manipulated per trial to be congruent, incongruent, or equated
  # with numerosity -- mirroring, not replicating, the real controlled
  # dot-array design.
  avg_diam <- runif(n, 20, 55)
  congruency <- sample(c("congruent", "incongruent", "equated"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))
  trials$threefeat <- congruency
  trials$threefeat_num <- as.numeric(factor(congruency, levels = c("congruent", "incongruent", "equated")))
  trials$fourfeat <- paste0(congruency, "_", ifelse(trials$choice_right1 == 1, "largeR", "largeL"))

  diam1 <- numeric(n)
  diam2 <- numeric(n)
  for (i in seq_len(n)) {
    if (congruency[i] == "congruent") {
      diam1[i] <- avg_diam[i]
      diam2[i] <- avg_diam[i]
    } else if (congruency[i] == "equated") {
      target_area <- 3000
      diam1[i] <- sqrt(target_area / trials$Num_1[i])
      diam2[i] <- sqrt(target_area / trials$Num_2[i])
    } else { # incongruent: the array with fewer dots gets bigger dots, so total surface flips vs. number
      if (trials$Num_1[i] < trials$Num_2[i]) {
        diam1[i] <- avg_diam[i] * 1.4
        diam2[i] <- avg_diam[i] * 0.8
      } else {
        diam2[i] <- avg_diam[i] * 1.4
        diam1[i] <- avg_diam[i] * 0.8
      }
    }
  }
  trials$AvDiameter_1 <- round(diam1, 2)
  trials$AvDiameter_2 <- round(diam2, 2)
  trials$TotSurf_1 <- round(trials$Num_1 * pi * (diam1 / 2)^2, 1)
  trials$TotSurf_2 <- round(trials$Num_2 * pi * (diam2 / 2)^2, 1)
  trials$TotContour_1 <- round(trials$Num_1 * pi * diam1, 1)
  trials$TotContour_2 <- round(trials$Num_2 * pi * diam2, 1)
  trials$ConvHull_1 <- round(trials$TotSurf_1 * runif(n, 3, 6), 1)
  trials$ConvHull_2 <- round(trials$TotSurf_2 * runif(n, 3, 6), 1)
  trials$ADBD_1 <- round(trials$ConvHull_1 / trials$Num_1, 2)
  trials$ADBD_2 <- round(trials$ConvHull_2 / trials$Num_2, 2)
  trials$ADfC_1 <- round(sqrt(trials$ConvHull_1 / pi), 2)
  trials$ADfC_2 <- round(sqrt(trials$ConvHull_2 / pi), 2)
  trials$TS_CH_1 <- round(trials$TotSurf_1 / trials$ConvHull_1, 3)
  trials$TS_CH_2 <- round(trials$TotSurf_2 / trials$ConvHull_2, 3)
  trials$isa_1 <- round(trials$TotSurf_1 / trials$Num_1, 2) # individual (average) dot surface area
  trials$isa_2 <- round(trials$TotSurf_2 / trials$Num_2, 2)
  trials$convex_hull <- round(trials$ConvHull_1 + trials$ConvHull_2, 1)
  trials$avg_ind_Dot_size <- round((trials$isa_1 + trials$isa_2) / 2, 2)
  trials$cum_Surf <- ave(trials$TotSurf_1 + trials$TotSurf_2, trials$block, FUN = cumsum)
  trials$cum_perim <- ave(trials$TotContour_1 + trials$TotContour_2, trials$block, FUN = cumsum)

  # response simulation (Weber-fraction psychometric model)
  d_prime <- abs(trials$Num_1 - trials$Num_2) / (subject$weber_fraction * sqrt(trials$Num_1^2 + trials$Num_2^2))
  p_correct <- pnorm(d_prime)
  correct <- rbinom(n, 1, p_correct)
  trials$corAns <- ifelse(trials$choice_right1 == 1, "m", "z") # m = right-hand key, z = left-hand key (placeholder mapping)
  trials$choice <- ifelse(correct == 1, trials$corAns, ifelse(trials$corAns == "m", "z", "m"))
  trials$accuracy <- correct
  trials$rt <- round(pmin(pmax(rlnorm(n, meanlog = log(1.0 + (1 - trials$numRatio) * 0.7), sdlog = 0.35), 0.25), 6), 3)
  trials$totaltime_exp <- round(cumsum(trials$rt + runif(n, 0.4, 0.9)), 3)

  age <- compute_age(subject$dob, subject$test_date)

  data.frame(
    participant = subject$participant,
    DayOfBirth = as.integer(format(subject$dob, "%d")),
    MonthOfBirth = as.integer(format(subject$dob, "%m")),
    YearOfBirth = as.integer(format(subject$dob, "%Y")),
    age_years = age$age_years,
    age_months = age$age_months,
    AgeGroup = age$age_group,
    SiteID = subject$site,
    Language = subject$language,
    RemoveDOB = "No",
    date = format(subject$test_date, "%Y-%m-%d"),
    expName = "amstask",
    psychopyVersion = "2023.2.3",
    visual_stimuli = "dot_array_v1",
    age_group = age$age_group,
    block = trials$block,
    block_displayed = trials$block_displayed,
    test_trials = trials$test_trials,
    numRatio = trials$numRatio,
    convex_hull = trials$convex_hull,
    avg_ind_Dot_size = trials$avg_ind_Dot_size,
    cum_Surf = trials$cum_Surf,
    cum_perim = trials$cum_perim,
    Num_1 = trials$Num_1,
    Num_2 = trials$Num_2,
    ConvHull_1 = trials$ConvHull_1,
    ConvHull_2 = trials$ConvHull_2,
    TotSurf_1 = trials$TotSurf_1,
    TotSurf_2 = trials$TotSurf_2,
    ADBD_1 = trials$ADBD_1,
    ADBD_2 = trials$ADBD_2,
    AvDiameter_1 = trials$AvDiameter_1,
    AvDiameter_2 = trials$AvDiameter_2,
    TotContour_1 = trials$TotContour_1,
    TotContour_2 = trials$TotContour_2,
    ADfC_1 = trials$ADfC_1,
    ADfC_2 = trials$ADfC_2,
    TS_CH_1 = trials$TS_CH_1,
    TS_CH_2 = trials$TS_CH_2,
    isa_1 = trials$isa_1,
    isa_2 = trials$isa_2,
    threefeat = trials$threefeat,
    threefeat_num = trials$threefeat_num,
    fourfeat = trials$fourfeat,
    choice_right1 = trials$choice_right1,
    totaltime_exp = trials$totaltime_exp,
    corAns = trials$corAns,
    choice = trials$choice,
    accuracy = trials$accuracy,
    rt = trials$rt,
    filename = paste0(subject$site, "_", subject$participant, "_", subject$timestamp),
    version = "local",
    stringsAsFactors = FALSE
  )
}

# ---- Visual Memory (VM) task -----------------------------------------------
# One row per trial (wide format matching the real organized file: 20 fixed
# screen positions per trial, tracked in test_p#/corr_p#/P#T_rectangle).
simulate_vm_trials <- function(subject, seed) {
  set.seed(seed + 1000) # offset so AMS/VM draws for the same subject don't reuse a seed
  n_positions <- 20
  set_sizes <- rep(2:6, each = 2) # 10 trials, two per set size (2-6 objects)
  n_trials <- length(set_sizes)
  fa_rate <- 0.08

  rows <- lapply(seq_len(n_trials), function(t) {
    clicks <- set_sizes[t]
    occupied <- sort(sample(seq_len(n_positions), clicks))
    target_pos <- sample(occupied, 1)
    probe_obj <- sample(c("circle", "square", "triangle", "star"), 1)

    probe_p <- rep(NA_character_, 7)
    probe_p[seq_len(clicks)] <- paste0("pos", occupied)

    hit_rate <- min(0.97, 0.5 + 0.45 * min(1, subject$capacity / clicks))
    corr <- integer(n_positions)
    rt_pos <- numeric(n_positions)
    for (p in seq_len(n_positions)) {
      is_occupied <- p %in% occupied
      corr[p] <- rbinom(1, 1, if (is_occupied) hit_rate else 1 - fa_rate)
      rt_pos[p] <- round(rlnorm(1, meanlog = log(0.8), sdlog = 0.4), 3)
    }
    test_p <- ifelse(seq_len(n_positions) %in% occupied, "occupied", "empty")

    c(
      list(
        item = paste0("VM_trial", t, "_set", clicks),
        clicks = clicks,
        objects = as.numeric(clicks),
        target_pos = paste0("pos", target_pos),
        probe_obj = probe_obj
      ),
      setNames(as.list(probe_p), paste0("probe_p", 1:7)),
      setNames(as.list(test_p), paste0("test_p", 1:20)),
      setNames(as.list(corr), paste0("corr_p", 1:20)),
      setNames(as.list(rt_pos), paste0("P", 1:20, "T_rectangle")),
      list(
        presented = clicks,
        rt = round(sum(rt_pos), 3),
        # accuracy is graded on the occupied ("hit") positions only -- with
        # up to 18 easy correct-rejections on empty positions in every
        # trial, folding those into the average would swamp the capacity
        # effect the hit rate is meant to show
        accuracy = as.numeric(mean(corr[occupied]) >= 0.8)
      )
    )
  })

  vm_trials <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  vm_trials$start <- round(cumsum(c(0, head(vm_trials$rt, -1) + 1)), 3)
  vm_trials$end <- round(vm_trials$start + vm_trials$rt, 3)
  vm_trials$totaltime_exp <- vm_trials$end

  wide_cols <- setdiff(names(vm_trials), c("presented", "rt", "accuracy", "start", "end", "totaltime_exp"))

  age <- compute_age(subject$dob, subject$test_date)

  data.frame(
    participant = subject$participant,
    DayOfBirth = as.integer(format(subject$dob, "%d")),
    MonthOfBirth = as.integer(format(subject$dob, "%m")),
    YearOfBirth = as.integer(format(subject$dob, "%Y")),
    age_years = age$age_years,
    age_months = age$age_months,
    AgeGroup = age$age_group,
    SiteID = subject$site,
    Language = subject$language,
    RemoveDOB = "No",
    date = format(subject$test_date, "%Y-%m-%d"),
    expName = "vmtask",
    psychopyVersion = "2023.2.3",
    vm_trials[, wide_cols],
    presented = vm_trials$presented,
    start = vm_trials$start,
    end = vm_trials$end,
    rt = vm_trials$rt,
    accuracy = vm_trials$accuracy,
    version = "local",
    monitor_size = "1920x1080",
    window_size = "1920x1080",
    totaltime_exp = vm_trials$totaltime_exp,
    filename = paste0(subject$site, "_", subject$participant, "_", subject$timestamp),
    stringsAsFactors = FALSE
  )
}

# subject config: (site, participant, version, timestamp) reproduce the
# exact filenames from the naming convention shown in the data dictionary
# (Dot Comparison/Visual Memory example, iucoglearn_mn085_...).
subjects <- list(
  list(
    site = "iucoglearn", participant = "mn085", version = "v2025.0301C", timestamp = "2025-10-0915h05.04.434",
    dob = as.Date("2021-03-12"), test_date = as.Date("2025-10-09"), language = "English",
    weber_fraction = 0.22, capacity = 4.0, seed = 8501
  ) # ~4.5yo, moderate numerical precision & memory capacity
)

for (s in subjects) {
  ams <- simulate_ams_trials(s, s$seed)
  vm <- simulate_vm_trials(s, s$seed)

  ams_path <- file.path(out_dir, paste0(s$site, "_", s$participant, "_", s$version, "_", s$timestamp, "_amstaskorganizedfile.csv"))
  vm_path <- file.path(out_dir, paste0(s$site, "_", s$participant, "_", s$version, "_", s$timestamp, "_vmtaskorganizedfile.csv"))

  write.csv(ams, ams_path, row.names = FALSE)
  write.csv(vm, vm_path, row.names = FALSE)

  cat(sprintf(
    "wrote %s / %s (AMS accuracy = %.2f, VM accuracy = %.2f)\n",
    basename(ams_path), basename(vm_path), mean(ams$accuracy), mean(vm$accuracy)
  ))
}

# ---- Highest Count task -----------------------------------------------------
# Single row per participant (wide-form, per the dictionary). hc_highestcount
# is set higher than each subject's Give-N "ability" level (see
# simulate_test_subjects.R) -- rote counting typically outruns Give-N
# cardinality understanding in real children, so equating the two would be
# unrealistic.
simulate_highestcount <- function(subject) {
  data.frame(
    hc_participant = subject$participant,
    hc_siteid = subject$site,
    hc_qualtrics_id = subject$qualtrics_id,
    hc_was.the.task.completed = "Yes (Please include date of testing)",
    hc_yes_date = "9-13-2026",
    hc_no_reason = "",
    hc_prompts = sprintf("What comes after %d?", subject$highestcount),
    hc_highestcount = subject$highestcount,
    hc_complete = 1,
    stringsAsFactors = FALSE
  )
}

# ---- Established R_TESTsim0001-0005 subjects -------------------------------
# Highest Count + AMS + VM files for the 5 synthetic subjects already used by
# simulate_test_subjects.R (Give-N long-form/summary) and
# ManyNumbers_forsharing_DataEntry_TESTsubjects.csv (demographics). site,
# participant, dob, and test_date are copied from those files so a
# participant's age is consistent across every one of their task files (the
# real qualitycontrolcheck file explicitly flags cross-task age mismatches --
# see calc_agediff/calc_yeardiff in the data dictionary's Site Summary
# section). weber_fraction/capacity/highestcount are new assumptions for
# this script, loosely scaled with age and with each subject's Give-N
# knower-level "ability" (pre-knower -> weakest, CP-knower -> strongest) so
# performance tells a consistent developmental story across all three tasks.
established_subjects <- list(
  list(
    site = "TESTSITE1", participant = "MNtest01", qualtrics_id = "R_TESTsim0001",
    dob = as.Date("2023-06-01"), test_date = as.Date("2026-09-13"), language = "English",
    weber_fraction = 0.35, capacity = 2.2, highestcount = 10, seed = 201
  ), # pre-knower, youngest (~3.3yo)
  list(
    site = "TESTSITE1", participant = "MNtest02", qualtrics_id = "R_TESTsim0002",
    dob = as.Date("2022-03-15"), test_date = as.Date("2026-09-13"), language = "English",
    weber_fraction = 0.28, capacity = 3.2, highestcount = 20, seed = 202
  ), # 2-knower (~4.5yo)
  list(
    site = "TESTSITE2", participant = "MNtest03", qualtrics_id = "R_TESTsim0003",
    dob = as.Date("2021-11-01"), test_date = as.Date("2026-09-13"), language = "Japanese",
    weber_fraction = 0.24, capacity = 4.0, highestcount = 31, seed = 203
  ), # 4-knower (~4.9yo)
  list(
    site = "TESTSITE2", participant = "MNtest04", qualtrics_id = "R_TESTsim0004",
    dob = as.Date("2020-02-20"), test_date = as.Date("2026-09-13"), language = "English",
    weber_fraction = 0.15, capacity = 5.5, highestcount = 100, seed = 204
  ), # CP-knower, oldest (~6.6yo)
  list(
    site = "TESTSITE1", participant = "MNtest05", qualtrics_id = "R_TESTsim0005",
    dob = as.Date("2022-08-09"), test_date = as.Date("2026-09-13"), language = "English",
    weber_fraction = 0.30, capacity = 3.5, highestcount = 15, seed = 205
  ) # 3-knower; Give-N task left unfinished in simulate_test_subjects.R
)

for (s in established_subjects) {
  s$timestamp <- s$qualtrics_id # reused as the cosmetic in-file "filename" value for AMS/VM

  hc <- simulate_highestcount(s)
  ams <- simulate_ams_trials(s, s$seed)
  vm <- simulate_vm_trials(s, s$seed)

  base <- paste0(s$site, "_", s$participant, "_", s$qualtrics_id)
  hc_path <- file.path(out_dir, paste0(base, "_highestcounttask.csv"))
  ams_path <- file.path(out_dir, paste0(base, "_amstaskorganizedfile.csv"))
  vm_path <- file.path(out_dir, paste0(base, "_vmtaskorganizedfile.csv"))

  write.csv(hc, hc_path, row.names = FALSE)
  write.csv(ams, ams_path, row.names = FALSE)
  write.csv(vm, vm_path, row.names = FALSE)

  cat(sprintf(
    "wrote %s / %s / %s (highest count = %d, AMS accuracy = %.2f, VM accuracy = %.2f)\n",
    basename(hc_path), basename(ams_path), basename(vm_path),
    hc$hc_highestcount, mean(ams$accuracy), mean(vm$accuracy)
  ))
}
