#!/usr/bin/env Rscript
# Simulate synthetic ManyNumbers Give-N test subjects (longform + summary
# csvs) to validate the numberbank import pipeline in data.qmd. Classification
# uses the real cumulative/specificity/knower-level criteria sourced from
# MN_code/mn_given_task/subfunctions/ -- the same code that produces summary
# files for real ManyNumbers submissions -- so the generated summary csvs are
# authentic, not hand-typed. Output files are prefixed "TESTSITE" so they're
# obviously synthetic alongside the real BCDLabIL_MN1p032 sample.
#
# Run from the repo root: Rscript MN_code/simulate_test_subjects/simulate_test_subjects.R
# (writes its output csvs into MN_code/ directly, alongside the real sample
# files, since that's where data.qmd looks for ManyNumbers imports)

library(dplyr)

source("MN_code/mn_given_task/subfunctions/mn_gn_cummulative.R")
source("MN_code/mn_given_task/subfunctions/mn_gn_specificity.R")
source("MN_code/mn_given_task/subfunctions/mn_gn_knowerlevel.R")

query_nums <- c(1, 2, 3, 4, 5, 6, 8, 10)
n_blocks <- 3
thr <- .66
thr_specificity <- 1

# simulate one participant's raw trial-level Give-N responses given a knower
# "ability" (the highest number they reliably give correctly). ability = 0 is
# a pre-knower (never correct); ability >= max(query_nums) is a CP-knower.
# complete = FALSE drops the last block's responses, mimicking an
# unfinished task.
simulate_longform <- function(site, participant, qualtrics_id, ability, complete, seed) {
  set.seed(seed)
  trials <- expand.grid(query = query_nums, block = seq_len(n_blocks)) |> arrange(block, query)
  # wrong answers are always overcounts (a number above the child's mastered
  # range) rather than a uniform random guess -- guessing with an already-
  # mastered number would (correctly) trip the classifier's specificity
  # check for that number, which isn't the point of this simulation
  trials$response <- vapply(trials$query, function(q) {
    if (q <= ability) q else sample(setdiff(query_nums[query_nums > ability], q), 1)
  }, numeric(1))
  trials$trial_num <- seq_len(nrow(trials))

  if (!complete) {
    trials$response[trials$block == max(trials$block)] <- NA
  }

  data.frame(
    gn_participant = participant,
    gn_siteid = site,
    gn_qualtrics_id = qualtrics_id,
    gn_was.the.task.completed = "Yes (Please include date of testing)",
    gn_yes_date = "9-13-2026",
    gn_no_reason = "",
    gn_trial = sprintf("GN%02d", trials$trial_num),
    gn_Query = trials$query,
    gn_Response_text = ifelse(is.na(trials$response), "NA (Not Administered)", as.character(trials$response)),
    gn_Response = trials$response,
    gn_accuracy = ifelse(is.na(trials$response), NA, as.numeric(trials$response == trials$query)),
    gn_complete = as.numeric(!any(is.na(trials$response))),
    gn_block = paste0("Block", trials$block),
    gn_blockcomplete = as.numeric(ave(!is.na(trials$response), trials$block, FUN = all)),
    stringsAsFactors = FALSE
  )
}

# mirrors gn_classifier() in mn_gn_classifier_082125.R, using the sourced
# cumulative/specificity/knower-level criteria above
classify_longform <- function(longform) {
  dummy <- longform
  names(dummy) <- gsub("gn_", "", names(dummy))

  dummy2_mean <- aggregate(accuracy ~ Query, dummy, mean, na.action = na.omit)
  dummy2_mean$Query <- as.factor(as.character(dummy2_mean$Query))
  dummy2_count <- as.data.frame(table(dummy$Query[!is.na(dummy$Response)]))
  names(dummy2_count) <- c("Query", "NumTrials")

  dummy_summary <- dummy2_mean |> left_join(dummy2_count, by = "Query")
  dummy_summary2 <- cumulative_criterion(dummy_summary, query_nums, thr)
  dummy_summary3 <- specificity_criterion(dummy, query_nums, dummy_summary2, thr_specificity)

  if (dummy$complete[1] == 1) {
    dummy_summary4 <- knower_level(dummy_summary3)
  } else {
    dummy_summary4 <- dummy_summary3
    dummy_summary4$KnowerLevel <- "not_calculated"
  }

  dummy_summary4$participant <- paste(dummy$siteid[1], dummy$participant[1], sep = "_")
  dummy_summary5 <- dummy_summary4 |> dplyr::select(participant, dplyr::everything())
  dummy_summary5$AccPass <- as.numeric(dummy_summary5$accuracy > thr)
  dummy_summary5$SpecificityCumul <- NA
  for (xx in seq_len(nrow(dummy_summary5))) {
    if (dummy_summary5[xx, "Query"] == 1 && dummy_summary5[xx, "SpecificityPass"] == 1) {
      dummy_summary5$SpecificityCumul[xx] <- 1
    } else if (dummy_summary5[xx, "Query"] == 1 && dummy_summary5[xx, "SpecificityPass"] == 0) {
      dummy_summary5$SpecificityCumul[xx] <- 0
    } else {
      dummy_summary6 <- subset(dummy_summary5, Query <= query_nums[xx])
      dummy_summary5[xx, "SpecificityCumul"] <- as.numeric(mean(dummy_summary6$SpecificityPass) == 1)
    }
  }

  dummy_summary5[c("participant", "Query", "NumTrials", "accuracy", "AccPass",
                    "SpecificityPass", "AccCumul", "SpecificityCumul", "KnowerLevel")]
}

test_subjects <- list(
  list(site = "TESTSITE1", participant = "MNtest01", qualtrics_id = "R_TESTsim0001", ability = 0,  complete = TRUE,  seed = 101), # pre-knower
  list(site = "TESTSITE1", participant = "MNtest02", qualtrics_id = "R_TESTsim0002", ability = 2,  complete = TRUE,  seed = 102), # 2-knower
  list(site = "TESTSITE2", participant = "MNtest03", qualtrics_id = "R_TESTsim0003", ability = 4,  complete = TRUE,  seed = 103), # 4-knower
  list(site = "TESTSITE2", participant = "MNtest04", qualtrics_id = "R_TESTsim0004", ability = 10, complete = TRUE,  seed = 104), # tests all numbers correctly -> CP-knower
  list(site = "TESTSITE1", participant = "MNtest05", qualtrics_id = "R_TESTsim0005", ability = 3,  complete = FALSE, seed = 105)  # unfinished task -> KnowerLevel = "not_calculated"
)

out_dir <- "MN_code"

for (s in test_subjects) {
  longform <- simulate_longform(s$site, s$participant, s$qualtrics_id, s$ability, s$complete, s$seed)
  summary_df <- classify_longform(longform)

  longform_path <- file.path(out_dir, paste0(s$site, "_", s$participant, "_", s$qualtrics_id, "_longformgiventaskV01.csv"))
  summary_path  <- file.path(out_dir, paste0(s$site, "_", s$participant, "_", s$qualtrics_id, "_summary_giventaskV01.csv"))

  write.csv(longform, longform_path, row.names = FALSE)
  write.csv(summary_df, summary_path, row.names = FALSE)

  cat(sprintf("wrote %s / %s (KnowerLevel = %s)\n",
              basename(longform_path), basename(summary_path),
              paste(unique(summary_df$KnowerLevel), collapse = ", ")))
}

# ---- demographics companion -----------------------------------------------
# A separate test-only export (doesn't touch the real
# ManyNumbers_forsharing_DataEntry_*.csv sample) with a demographics row for
# each test subject above, so the age/language/country/gender merge in
# data.qmd can be validated end-to-end. Mirrors the real Qualtrics export
# shape: row 1 = field names, rows 2-3 are junk (question text / ImportId)
# that data.qmd's reader always drops, real data from row 4 on. Only includes
# the columns data.qmd actually reads -- read.csv doesn't care that the real
# export has 361 columns and this one has 8.
#
# data.qmd matches demographics to longform rows on gn_qualtrics_id (longform)
# <-> ResponseId (demographics), never on Q1/Q3 (site/participant as typed by
# the RA) -- those can disagree with gn_siteid/gn_participant, as they
# genuinely do in the real BCDLabIL_MN1p032 sample. Q1/Q3 are still included
# here for realism, but ResponseId is what actually drives the join.
demographics_profiles <- list(
  list(site = "TESTSITE1", participant = "MNtest01", dob = "6/1/23",  dot = "9/13/26", gender = "Male",   language = "English", multilingual = "No",  country = "United States"),
  list(site = "TESTSITE1", participant = "MNtest02", dob = "3/15/22", dot = "9/13/26", gender = "Female", language = "English", multilingual = "Yes", country = "United States"),
  list(site = "TESTSITE2", participant = "MNtest03", dob = "11/1/21", dot = "9/13/26", gender = "Female", language = "Japanese",  multilingual = "No",  country = "Japan"),
  list(site = "TESTSITE2", participant = "MNtest04", dob = "2/20/20", dot = "9/13/26", gender = "Male",   language = "English",  multilingual = "Yes", country = "Singapore"),
  list(site = "TESTSITE1", participant = "MNtest05", dob = "8/9/22",  dot = "9/13/26", gender = "Female", language = "English", multilingual = "No",  country = "United States")
)

# look up each profile's qualtrics_id from test_subjects above, keyed by
# site_participant, so the two lists can't silently drift out of sync
qualtrics_id_by_subject <- setNames(
  sapply(test_subjects, function(s) s$qualtrics_id),
  sapply(test_subjects, function(s) paste(s$site, s$participant, sep = "_"))
)

demographics_cols <- c("ResponseId", "Q1", "Q3", "Q10", "Dem_DOB", "Dem_DOT", "Dem_Gender", "Dem_Language", "Dem_Multilingual")

demographics_rows <- bind_rows(lapply(demographics_profiles, function(p) {
  subject_key <- paste(p$site, p$participant, sep = "_")
  data.frame(ResponseId = qualtrics_id_by_subject[[subject_key]], Q1 = p$site, Q3 = p$participant,
             Q10 = p$country, Dem_DOB = p$dob, Dem_DOT = p$dot, Dem_Gender = p$gender,
             Dem_Language = p$language, Dem_Multilingual = p$multilingual, stringsAsFactors = FALSE)
}))

junk_rows <- rbind(
  setNames(as.data.frame(as.list(paste("Question text for", demographics_cols)), stringsAsFactors = FALSE), demographics_cols),
  setNames(as.data.frame(as.list(paste0('{"ImportId":"', demographics_cols, '"}')), stringsAsFactors = FALSE), demographics_cols)
)

demographics_out <- rbind(junk_rows, demographics_rows)
demographics_path <- file.path(out_dir, "ManyNumbers_forsharing_DataEntry_TESTsubjects.csv")
write.csv(demographics_out, demographics_path, row.names = FALSE)
cat(sprintf("wrote %s (%d test demographics rows)\n", basename(demographics_path), nrow(demographics_rows)))
