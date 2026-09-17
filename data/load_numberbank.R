#!/usr/bin/env Rscript
# Shared Numberbank data pipeline -------------------------------------------
# This mirrors the R chunks that live inline in data_v2.qmd (Redivis load ->
# ManyNumbers Give-N import -> kl recalculation -> gap-dataset backfill ->
# kl_final/kl_data/trial_data -> ManyNumbers AMS+VM import). It's kept here so
# other things (e.g. the "Export this view" R snippets data_v2.qmd generates)
# can rebuild the same data without hand-copying the pipeline. data.qmd and
# data_v2.qmd still carry their own inline copies for now -- if this file and
# those chunks ever diverge, whichever .qmd generated the export you're
# looking at is the one that was actually live at the time.
#
# source("data/load_numberbank.R") from the repo root leaves the following in
# scope: hc, kld, td, mn_td, mn_kld, td_all, kl_recalc, kl_recalc_details,
# mn_kl_qc, kld_gap_rows, languages_countries, kl_source_eligibility,
# kl_final, kl_data, trial_data, ams_data, vm_data.

library(dplyr)
library(stringr)
library(forcats)
library(glue)

# ---- load Numberbank's own tables from Redivis -----------------------------
dataset <- redivis::user("datapages")$dataset("Numberbank")
hc <- dataset$table("highest count")$to_tibble()
kld <- dataset$table("knower level")$to_tibble()
td <- dataset$table("trials")$to_tibble()

# ---- ManyNumbers Give-N import ----------------------------------------------
# Raw per-participant site exports arrive in pairs, named like:
#   {site}_{participant}_{qualtrics_id}_longformgiventaskV##.csv  (one row per trial)
#   {site}_{participant}_{qualtrics_id}_summary_giventaskV##.csv  (pre-computed per-query summary + KnowerLevel)
# Point mn_giventask_dir at the folder holding all such pairs (currently just
# the two example files in MN_code/) as more sites' data comes in. This folds
# ManyNumbers into td's schema below as dataset_id "ManyNumbers".
mn_giventask_dir <- "MN_code"

mn_longform_files <- Sys.glob(file.path(mn_giventask_dir, "*_longformgiventaskV*.csv"))
mn_summary_files  <- Sys.glob(file.path(mn_giventask_dir, "*_summary_giventaskV*.csv"))
mn_demographics_files <- Sys.glob(file.path(mn_giventask_dir, "ManyNumbers_forsharing_DataEntry_*.csv"))

# Read demographic file.
# Only age (from Dem_DOB/Dem_DOT), language, country, and gender are pulled
# in for now.
read_mn_demographics <- function(path) {
  raw <- read.csv(path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  raw <- raw[-c(1, 2), ] # drop the question-text + ImportId rows
  raw |>
    filter(trimws(ResponseId) != "") |>
    transmute(
      qualtrics_id = trimws(ResponseId),
      dob = suppressWarnings(as.Date(Dem_DOB, format = "%m/%d/%y")),
      dot = suppressWarnings(as.Date(Dem_DOT, format = "%m/%d/%y")),
      age = as.integer(round(as.numeric(difftime(dot, dob, units = "days")) / 30.4368)), # months
      gender = Dem_Gender,
      language = Dem_Language,
      country = Q10,
      export_mtime = file.info(path)$mtime
    )
}

# multiple exports may exist over time (new Qualtrics downloads); keep the
# most recently exported row per response
mn_demographics <- if (length(mn_demographics_files)) {
  bind_rows(lapply(mn_demographics_files, read_mn_demographics)) |>
    arrange(qualtrics_id, desc(export_mtime)) |>
    distinct(qualtrics_id, .keep_all = TRUE) |>
    select(-export_mtime)
} else {
  tibble(qualtrics_id = character(), age = integer(), gender = character(),
         language = character(), country = character())
}

read_mn_longform <- function(path) {
  read.csv(path, stringsAsFactors = FALSE) |>
    mutate(query = suppressWarnings(as.numeric(gn_Query)),
           response = suppressWarnings(as.numeric(gn_Response))) |>
    filter(!is.na(query), !is.na(response)) |>
    transmute(
      dataset_id = "ManyNumbers",
      subject_id = paste(gn_siteid, gn_participant, sep = "_"),
      qualtrics_id = trimws(gn_qualtrics_id),
      method = "non-titrated", # fixed give-n protocol
      query = query,
      response = pmin(response, 10) # cap at 10, matching trial_data convention
    )
}

# Reshape KL labels into numberbank convention
label_to_kl <- function(label, cp_threshold = 6) {
  kl_numeric <- suppressWarnings(as.numeric(gsub("-knower", "", label, ignore.case = TRUE)))
  kl_numeric[tolower(label) == "pre-knower"] <- 0
  if_else(is.na(kl_numeric), NA_character_,
          if_else(kl_numeric >= cp_threshold, "CP-knower", paste0(kl_numeric, "-knower")))
}

label_to_kl_subset <- function(label, cp_threshold = 6) {
  kl <- label_to_kl(label, cp_threshold)
  case_when(
    is.na(kl) ~ NA_character_,
    kl == "CP-knower" ~ "CP-knower",
    kl == "0-knower" ~ "Pre-knower",
    .default = "Subset-knower"
  )
}

read_mn_summary <- function(path) {
  read.csv(path, stringsAsFactors = FALSE) |>
    transmute(subject_id = participant, KnowerLevel)
}

mn_longform_raw <- if (length(mn_longform_files)) bind_rows(lapply(mn_longform_files, read_mn_longform)) else
  tibble(dataset_id = character(), subject_id = character(), qualtrics_id = character(),
         method = character(), query = integer(), response = integer())

# Highest Count task, named like {site}_{participant}_{qualtrics_id}_highestcounttask.csv
mn_hc_files <- Sys.glob(file.path(mn_giventask_dir, "*_highestcounttask.csv"))

read_mn_hc <- function(path) {
  read.csv(path, stringsAsFactors = FALSE) |>
    transmute(
      subject_id = paste(hc_siteid, hc_participant, sep = "_"),
      qualtrics_id = trimws(hc_qualtrics_id),
      hc = suppressWarnings(as.numeric(hc_highestcount))
    ) |>
    filter(!is.na(hc))
}

mn_hc <- if (length(mn_hc_files)) bind_rows(lapply(mn_hc_files, read_mn_hc)) else
  tibble(subject_id = character(), qualtrics_id = character(), hc = numeric())

# combines both sources so a participant who only did the Highest Count task
# (no Give-N submission) still resolves a qualtrics_id for the demographics
# lookup below
mn_subject_qualtrics <- bind_rows(
  mn_longform_raw |> distinct(subject_id, qualtrics_id),
  mn_hc |> distinct(subject_id, qualtrics_id)
) |> distinct(subject_id, qualtrics_id)

# one KnowerLevel per participant (the summary file repeats it once per
# query row, so take the first)
mn_summary_kl <- if (length(mn_summary_files)) {
  bind_rows(lapply(mn_summary_files, read_mn_summary)) |>
    group_by(subject_id) |>
    summarise(KnowerLevel = first(KnowerLevel), .groups = "drop")
} else {
  tibble(subject_id = character(), KnowerLevel = character())
}

# kld-shaped table for ManyNumbers: every subject who has either a Give-N
# summary or a Highest Count submission (or both), same as how Numberbank's
# own kld can have kl-only, hc-only, or both -- with age/language/country
# merged in from demographics via qualtrics_id
mn_kld <- full_join(mn_summary_kl, mn_hc |> select(subject_id, hc), by = "subject_id") |>
  left_join(mn_subject_qualtrics, by = "subject_id") |>
  left_join(mn_demographics |> select(qualtrics_id, age, language, country), by = "qualtrics_id") |>
  transmute(
    dataset_id = "ManyNumbers",
    subject_id,
    age,
    language,
    country,
    method = "non-titrated",
    kl = label_to_kl(KnowerLevel),
    kl_subset = label_to_kl_subset(KnowerLevel),
    hc
  )

# trial-level ManyNumbers rows also need kl/kl_subset attached (from mn_kld
# above) -- td's own kl/kl_subset columns are what the Plots -- Items tab
# filters trials by, so without this every ManyNumbers trial silently fails
# that filter and never renders there, no matter how much data exists
mn_td <- mn_longform_raw |>
  left_join(mn_demographics |> select(qualtrics_id, age, language, country), by = "qualtrics_id") |>
  left_join(mn_kld |> select(subject_id, kl, kl_subset), by = "subject_id") |>
  select(dataset_id, subject_id, method, age, language, country, query, response, kl, kl_subset)

# combined trial-level data: Numberbank's own td plus the newly imported
# ManyNumbers trials, in the same shape
td_all <- bind_rows(td, mn_td)

# datasets not yet in kld at all: currently just ManyNumbers, whose kl comes
# from mn_kld above (built from the site's own summary file) rather than the
# recalculation below.
new_datasets <- c("ManyNumbers")

# ---- recalculate kl ---------------------------------------------------------
source("data/kl_calc_mn.R")
source("data/helpers/mn_gn_knowerlevel_details.R")

# recalculate_kl_mn(_details) match trials into sessions by exact age, and
# NA != NA, so give ManyNumbers rows a fake age for this call only
# where their real (demographics-derived) age is still missing. Note
# ManyNumbers' kl in kl_final comes from mn_kld (the summary file), not from
# this recalculation -- this is only run so we have a from-raw-trials
# cross-check (mn_kl_qc below) and so existing kld-tracked datasets keep
# working exactly as before.
td_for_recalc <- td_all |> mutate(age = if_else(dataset_id %in% new_datasets & is.na(age), -1L, age))
kl_recalc <- recalculate_kl_mn(td_for_recalc, hc)
kl_recalc_details <- recalculate_kl_mn_details(td_for_recalc)

# QC check: does recomputing kl from raw trial responses agree with the
# KnowerLevel the site's own summary file already gives us (the value we
# actually use, via mn_kld)?
mn_kl_qc <- kl_recalc |>
  filter(dataset_id == "ManyNumbers") |>
  select(-age) |>
  left_join(mn_kld |> select(subject_id, kl_from_summary = kl), by = "subject_id") |>
  mutate(kl_match = kl == kl_from_summary)

# ---- languages/countries, eligibility, gap datasets, kl_final, kl_data, trial_data
# replacement language values
language_recode <- list(
  "Arabic" = "Saudi",
  "French" = "Français",
  "English" = c("Anglais", "English (India)", "English (US)", "English/Portugese", "English/Spanish"),
  "Slovenian (dual)" = "Slovenian_dual",
  "Slovenian (non-dual)" = c("Slovenian", "Slovenian_nonDual", "Serbian/Slovenian")
)

languages_countries <- bind_rows(kld |> select(language, country), td_all |> select(language, country)) |>
  distinct() |>
  mutate(# recode + sort languages
         lang = language |> fct_collapse(!!!language_recode) |> fct_relevel(sort),
         # combination of language and country
         language_country = paste(lang, country, sep = "\n")) |>
  distinct(language, lang, country, language_country) |>
  group_by(lang) |>
  mutate(language_countries = paste(unique(country), collapse = ", "),
         language_countries = glue("{lang} – {language_countries}")) |>
  group_by(country) |>
  mutate(country_languages = paste(unique(lang), collapse = ", "),
         country_languages = glue("{country} – {country_languages}")) |>
  ungroup()

# a study/method is eligible for the recalculated knower level only if it
# uses a non-titrated method and tests all of the numbers 1 through 6
kl_source_eligibility <- td_all |>
  group_by(dataset_id, method) |>
  summarise(tests_1_to_6 = all(1:6 %in% query), .groups = "drop") |>
  mutate(use_recalc = method == "non-titrated" & tests_1_to_6) |>
  select(dataset_id, method, use_recalc)

# some older studies were uploaded to the "trials" table (td) but were never
# given an official row in the "knower level" table (kld) at all -- so they
# don't show up anywhere in the knower-level plots, even though we have their
# raw trial data. Add them to kld here using the same recalculation already
# computed above (kl_recalc), restricted to the same eligibility rule used
# everywhere else (kl_source_eligibility): non-titrated, and testing all of
# 1 through 6.
kld_gap_datasets <- setdiff(unique(td$dataset_id), unique(kld$dataset_id))

kld_gap_rows <- kl_recalc |>
  filter(dataset_id %in% kld_gap_datasets) |>
  semi_join(kl_source_eligibility |> filter(use_recalc), by = c("dataset_id", "method")) |>
  select(dataset_id, subject_id, age, language, country, method, kl, kl_subset, hc)

kld <- bind_rows(kld, kld_gap_rows)

# td's own kl/kl_subset should also reflect the recalculated classification
# when eligible, so Plots -- Items (which reads straight from
# trial_data/td_all) stays consistent with Plots -- Knower levels (which
# reads kl_data/kl_final) for the same trials. ManyNumbers is excluded here
# since it already got its authoritative kl merged in from its own summary
# files above (see mn_td) -- recomputing it from trial responses would
# override that on-purpose choice.
td_all <- td_all |>
  left_join(kl_source_eligibility, by = c("dataset_id", "method")) |>
  mutate(use_recalc = coalesce(use_recalc, FALSE) & !(dataset_id %in% new_datasets)) |>
  left_join(
    kl_recalc |> select(dataset_id, subject_id, age, method, kl_recalc = kl, kl_subset_recalc = kl_subset),
    by = c("dataset_id", "subject_id", "age", "method")
  ) |>
  mutate(
    kl = if_else(use_recalc & !is.na(kl_recalc), kl_recalc, kl),
    kl_subset = if_else(use_recalc & !is.na(kl_recalc), kl_subset_recalc, kl_subset)
  ) |>
  select(-use_recalc, -kl_recalc, -kl_subset_recalc)

# kld left untouched; kl_final swaps in the recalculated kl/kl_subset only for
# eligible study/methods, falling back to kld's values everywhere else
# NOTE: this only affects Piantadosi & Gibson's data, because they only ask each kid for 1 trial per number.
kl_final <- bind_rows(
  kld |>
    left_join(kl_source_eligibility, by = c("dataset_id", "method")) |>
    mutate(use_recalc = coalesce(use_recalc, FALSE)) |>
    left_join(
      kl_recalc |> select(dataset_id, subject_id, age, method, kl_recalc = kl, kl_subset_recalc = kl_subset),
      by = c("dataset_id", "subject_id", "age", "method")
    ) |>
    mutate(
      kl = if_else(use_recalc & !is.na(kl_recalc), kl_recalc, kl),
      kl_subset = if_else(use_recalc & !is.na(kl_recalc), kl_subset_recalc, kl_subset)
    ) |>
    select(-use_recalc, -kl_recalc, -kl_subset_recalc),
  # MN gets KL from own summary files
  mn_kld
)

kl_data <- kl_final |>
  filter(!is.na(kl) | !is.na(hc)) |> # remove rows with both kl and hc NA
  left_join(languages_countries) |> mutate(language = lang) |> select(-lang) |>
  arrange(language, kl, age)

trial_data <- td_all |>
  mutate(response = if_else(response > 10, 10, response)) |> # cap responses at 10
  left_join(languages_countries) |> mutate(language = lang) |> select(-lang)

# ---- ManyNumbers Dot Comparison (AMS) + Visual Memory (VM) import ----------
# Raw per-participant site exports are named like:
#   {site}_{participant}_{id}_amstaskorganizedfile.csv  (one row per AMS trial)
#   {site}_{participant}_{id}_vmtaskorganizedfile.csv   (one row per VM trial)
# Unlike the Give-N/demographics files, these carry their own age/language
# columns directly (age_years/age_months/Language) and have no qualtrics id
# of their own -- see manynumbers_datadictionary_030326_V01.pdf. Country
# isn't part of either file, so it's backfilled from the Give-N demographics
# import above where the same subject_id also has a Give-N submission; a
# subject with AMS/VM data but no Give-N/demographics (e.g. a lone AMS-only
# submission) will end up with country = NA and so won't match any
# country/language sidebar filter.
mn_ams_files <- Sys.glob(file.path(mn_giventask_dir, "*_amstaskorganizedfile.csv"))
mn_vm_files  <- Sys.glob(file.path(mn_giventask_dir, "*_vmtaskorganizedfile.csv"))

read_mn_ams <- function(path) {
  read.csv(path, stringsAsFactors = FALSE) |>
    filter(block %in% c("block1", "block2", "block3")) |> # experimental trials only, drop sample/practice
    transmute(
      dataset_id = "ManyNumbers",
      subject_id = paste(SiteID, participant, sep = "_"),
      age = suppressWarnings(as.numeric(age_months)),
      language = Language,
      numRatio = suppressWarnings(as.numeric(numRatio)),
      accuracy = suppressWarnings(as.numeric(accuracy)),
      rt = suppressWarnings(as.numeric(rt))
    ) |>
    filter(!is.na(accuracy))
}

read_mn_vm <- function(path) {
  read.csv(path, stringsAsFactors = FALSE) |>
    transmute(
      dataset_id = "ManyNumbers",
      subject_id = paste(SiteID, participant, sep = "_"),
      age = suppressWarnings(as.numeric(age_months)),
      language = Language,
      clicks = suppressWarnings(as.numeric(clicks)),
      accuracy = suppressWarnings(as.numeric(accuracy)),
      rt = suppressWarnings(as.numeric(rt))
    ) |>
    filter(!is.na(accuracy))
}

mn_ams_raw <- if (length(mn_ams_files)) bind_rows(lapply(mn_ams_files, read_mn_ams)) else
  tibble(dataset_id = character(), subject_id = character(), age = numeric(),
         language = character(), numRatio = numeric(), accuracy = numeric(), rt = numeric())

mn_vm_raw <- if (length(mn_vm_files)) bind_rows(lapply(mn_vm_files, read_mn_vm)) else
  tibble(dataset_id = character(), subject_id = character(), age = numeric(),
         language = character(), clicks = numeric(), accuracy = numeric(), rt = numeric())

# country + knower level backfilled by subject_id from the Give-N import
# above (mn_subject_qualtrics/mn_demographics/mn_kld); a subject with no
# Give-N submission gets NA for both, same caveat as above for country
mn_subject_country <- mn_subject_qualtrics |>
  left_join(mn_demographics |> select(qualtrics_id, country), by = "qualtrics_id") |>
  distinct(subject_id, country)

mn_subject_kl <- mn_kld |> distinct(subject_id, kl, kl_subset)

# split the (language, country) lookup in two so a subject with no country
# (no matching Give-N submission to backfill it from) still resolves a
# recoded language/language_countries -- language_countries is aggregated
# per language across all its countries, so it doesn't actually need country
# to be known. Only language_country/country_languages genuinely require it
# and stay NA without one.
lang_lookup <- languages_countries |> distinct(language, lang, language_countries)
country_lookup <- languages_countries |> distinct(language, country, language_country, country_languages)

attach_mn_lookups <- function(df) {
  df |>
    left_join(mn_subject_country, by = "subject_id") |>
    left_join(mn_subject_kl, by = "subject_id") |>
    left_join(lang_lookup, by = "language") |>
    left_join(country_lookup, by = c("language", "country")) |>
    mutate(language = lang) |>
    select(-lang)
}

ams_data <- mn_ams_raw |> attach_mn_lookups()
vm_data <- mn_vm_raw |> attach_mn_lookups()
