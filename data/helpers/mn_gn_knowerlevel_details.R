library(dplyr)

source("data/helpers/mn_gn_cummulative.R")
source("data/helpers/mn_gn_specificity.R")
source("data/helpers/mn_gn_knowerlevel.R")

recalculate_kl_mn_details <- function(td, thr = .66, thr_specificity = 1) {
  sessions <- td |> distinct(dataset_id, subject_id, age, method, language, country)

  session_details <- lapply(seq_len(nrow(sessions)), function(i) {
    s <- sessions[i, ]

    dummy <- td |>
      filter(dataset_id == s$dataset_id, subject_id == s$subject_id,
             age == s$age, method == s$method) |>
      transmute(Query = query, Response = response, accuracy = as.numeric(response == query))

    query_nums <- sort(unique(dummy$Query))

    trial_counts <- as.data.frame(table(dummy$Query))
    names(trial_counts) <- c("Query", "NumTrials")

    dummy_summary <- aggregate(accuracy ~ Query, dummy, mean)
    dummy_summary$Query <- as.factor(as.character(dummy_summary$Query))
    dummy_summary <- dummy_summary |> left_join(trial_counts, by = "Query")
    dummy_summary$Query <- as.numeric(as.character(dummy_summary$Query))
    dummy_summary$AccPass <- as.numeric(dummy_summary$accuracy > thr)

    dummy_summary2 <- cumulative_criterion(dummy_summary, query_nums, thr)
    dummy_summary3 <- specificity_criterion(dummy, query_nums, dummy_summary2, thr_specificity)
    dummy_summary4 <- knower_level(dummy_summary3)

    # cumulative version of SpecificityPass, mirroring mn_gn_classifier_082125.R:36-51
    dummy_summary4$SpecificityCumul <- NA
    for (xx in seq_len(nrow(dummy_summary4))) {
      if (dummy_summary4$Query[xx] == query_nums[1]) {
        dummy_summary4$SpecificityCumul[xx] <- dummy_summary4$SpecificityPass[xx]
      } else {
        up_to <- subset(dummy_summary4, Query <= dummy_summary4$Query[xx])
        dummy_summary4$SpecificityCumul[xx] <- as.numeric(mean(up_to$SpecificityPass) == 1)
      }
    }

    dummy_summary4$dataset_id <- s$dataset_id
    dummy_summary4$subject_id <- s$subject_id
    dummy_summary4$age <- s$age
    dummy_summary4$method <- s$method
    dummy_summary4$language <- s$language
    dummy_summary4$country <- s$country

    dummy_summary4 |>
      select(dataset_id, subject_id, age, method, language, country,
             Query, NumTrials, accuracy, AccPass, SpecificityPass, AccCumul, SpecificityCumul, KnowerLevel)
  })

  bind_rows(session_details)
}
