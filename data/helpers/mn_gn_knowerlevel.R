knower_level <-function (dummy_summary3){
  dummy_knowerlevel = subset(dummy_summary3, AccCumul == 1)
  if(length(dummy_knowerlevel$AccCumul)==0){
    dummy_summary3$KnowerLevel = "pre-knower"
  } else if (mean(dummy_knowerlevel$SpecificityPass) ==1) {
    dummy_summary3$KnowerLevel = paste(max(dummy_knowerlevel$Query),"-knower", sep = "")
  }else{
    element_to_find <- 0
    first_occurrence <- which(dummy_knowerlevel$SpecificityPass == element_to_find)[1]
    if(first_occurrence == 1){
      dummy_summary3$KnowerLevel = "pre-knower"  
    }else {
      dummy_summary3$KnowerLevel = paste(dummy_knowerlevel$Query[first_occurrence-1],"-knower", sep = "")
    }
    
  }
  dummy_summary3 = dummy_summary3
}