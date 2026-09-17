cumulative_criterion <-function (dummy_summary, query_nums, thr){
  dummy_summary$Query = as.numeric(as.character(dummy_summary$Query))
  dummy_summary$AccCumul = NA
  i =1
  for (i in 1:length(query_nums)) {
    if(query_nums[i] == 1){
      if(dummy_summary[which(dummy_summary$Query == query_nums[i]),"accuracy"] > thr){
        dummy_summary[which(dummy_summary$Query == query_nums[i]),"AccCumul"] = 1
      }else{
        dummy_summary[which(dummy_summary$Query == query_nums[i]),"AccCumul"] = 0 
      }
    } else{
      dummy2_acc = subset(dummy_summary, Query<= query_nums[i])
      dummy2_acc = subset(dummy2_acc, accuracy  > thr)
      if (mean(query_nums[1:i] %in% dummy2_acc$Query)==1) {
        dummy_summary[which(dummy_summary$Query == query_nums[i]),"AccCumul"] = 1
      } else{
        dummy_summary[which(dummy_summary$Query == query_nums[i] ),"AccCumul"] = 0
      }
      
    }
  }
  dummy_summary =dummy_summary
}
