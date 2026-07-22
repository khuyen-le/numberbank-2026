specificity_criterion <-function (dummy,query_nums, dummy_summary, thr_specificity){
  dummy_summary$SpecificityPass = NA
  i = 2
  for (i in 1:length(query_nums)) {
    dummy3 = subset(dummy, Response == query_nums[i] & accuracy !=1)
    if (length(dummy3$Query)==0) {
      dummy_summary[which(dummy_summary$Query == query_nums[i] ),"SpecificityPass"] = 1
    }else {
      errors = as.data.frame(table(dummy3$Query, dummy3$Response)) # This could be only by Query, as there should always be one response
      names(errors) = c("Query","Response","Frequency")
      if (max(errors$Freq) <= thr_specificity) {
        dummy_summary[which(dummy_summary$Query == query_nums[i] ),"SpecificityPass"] = 1
      }else{
        dummy_summary[which(dummy_summary$Query == query_nums[i] ),"SpecificityPass"] = 0
      }
    }
  }
  dummy_summary = dummy_summary
}
