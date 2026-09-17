library(dplyr)

source("data/helpers/mn_gn_cummulative.R")
source("data/helpers/mn_gn_specificity.R")
source("data/helpers/mn_gn_knowerlevel.R")

recalculate_kl_mn <- function(td, hc, thr = .66, thr_specificity = 1, cp_threshold = 6,
                               required_queries = 1:4) {
  sessions <- td |> distinct(dataset_id, subject_id, age, method, language, country)

  session_results <- lapply(seq_len(nrow(sessions)), function(i) {
    s <- sessions[i, ]

    dummy <- td |>
      filter(dataset_id == s$dataset_id, subject_id == s$subject_id,
             age == s$age, method == s$method) |>
      transmute(Query = query, Response = response, accuracy = as.numeric(response == query))

    query_nums <- sort(unique(dummy$Query))

    # only score kl if the study asked for all of required_queries (e.g. 1-4)
    if (!all(required_queries %in% query_nums)) {
      return(data.frame(dataset_id = s$dataset_id, subject_id = s$subject_id, age = s$age,
                         language = s$language, country = s$country, method = s$method,
                         kl = NA_character_, kl_subset = NA_character_))
    }

    trial_counts <- as.data.frame(table(dummy$Query))
    names(trial_counts) <- c("Query", "NumTrials")

    dummy_summary <- aggregate(accuracy ~ Query, dummy, mean)
    dummy_summary$Query <- as.factor(as.character(dummy_summary$Query))
    dummy_summary <- dummy_summary |> left_join(trial_counts, by = "Query")
    
    dummy_summary2 <- cumulative_criterion(dummy_summary, query_nums, thr)
    dummy_summary3 <- specificity_criterion(dummy, query_nums, dummy_summary2, thr_specificity)
    dummy_summary4 <- knower_level(dummy_summary3)

    kl_label <- dummy_summary4$KnowerLevel[1]
    kl_numeric <- if (kl_label == "pre-knower") 0 else as.numeric(gsub("-knower", "", kl_label))

    kl <- if (kl_numeric >= cp_threshold) "CP-knower" else paste0(kl_numeric, "-knower")
    kl_subset <- case_when(
      kl == "CP-knower" ~ "CP-knower",
      kl_numeric == 0 ~ "Pre-knower",
      .default = "Subset-knower"
    )

    data.frame(dataset_id = s$dataset_id, subject_id = s$subject_id, age = s$age,
               language = s$language, country = s$country, method = s$method,
               kl = kl, kl_subset = kl_subset)
  })

  bind_rows(session_results) |>
    left_join(hc, by = c("dataset_id", "subject_id"))
}