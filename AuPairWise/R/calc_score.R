## Calculates the ROC for a given list of scores(or ranks) and the known labels
roc_score <- function(scores,labels)
{
  negatives = which(labels == 0, arr.ind=T)
  scores[negatives] <- 0
  
  p  = sum(scores)
  nL = length(labels)
  np = sum(labels)
  nn = nL - np
  
  roc  <-  (p/np - (np+1)/2)/nn
  
  return(roc)
}

##########################################
calc_rates <- function(scores, labels, thresh){
  tp = sum((scores >= thresh) * labels)
  fp = sum ((scores >= thresh) * !labels)
  p  = sum(labels)
  n = sum(!labels)
  #fn = p - tp
  #tn = n - fp
  fpr = (fp/n)
  tpr = (tp/p)
  
  #return(cbind(tp,fp, tn, fn, p, n, fpr,tpr))
  return(cbind(fpr,tpr))
}