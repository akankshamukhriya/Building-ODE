
Roc_Auc <- function(A_RA, A_Labels)
{
  library(ROCR);
  
  t1_pred  = A_RA[,1]
  PRED <- prediction(t1_pred, A_Labels);
  auc.perf = performance(PRED, measure = "auc")
  ra = as.numeric(auc.perf@y.values)
  
  return(ra);
}
