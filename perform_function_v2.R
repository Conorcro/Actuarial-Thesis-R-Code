perform = function(p.pred, response, tau){
  
  if(missing(tau))
    stop('tau is missing')
  
  if(tau<0 | (tau>1&tau!='random'))
    stop('tau must be either numeric between 0 and 1 or tau="random"')
  
  pout = list()
  
  if(tau>=0&tau<=1){
    
    y.pred = ifelse(p.pred >= tau, 1, 0)
    
    MisClassError = mean(y.pred != response)
    MisClassTable = table(Actual = response, Predicted = y.pred)
    
  } else if(tau=='random') {
    
    y.pred = rbinom(20*length(p.pred), size=rep(1,20*length(p.pred)), prob=rep(p.pred,20))
    
    MisClassError = mean(y.pred != rep(response,20))
    MisClassTable = table(Actual = rep(response,20), Predicted = y.pred)/20
    
  }
  
  Accuracy = 1- MisClassError
  
  rates = matrix(rep(0,8), 8, 1)
  rownames(rates) = c('tpr', 'tnr', 'ppv', 'npv', 'fpr', 'fnr', 'fdr', 'for')
  
  rates['tpr', 1] = MisClassTable[2,2]/rowSums(MisClassTable)[[2]] 
  rates['tnr', 1] = MisClassTable[1,1]/rowSums(MisClassTable)[[1]]
  rates['ppv', 1] = MisClassTable[2,2]/colSums(MisClassTable)[[2]]
  rates['npv', 1] = MisClassTable[1,1]/colSums(MisClassTable)[[1]]
  rates['fpr', 1] = MisClassTable[1,2]/rowSums(MisClassTable)[[1]]
  rates['fnr', 1] = MisClassTable[2,1]/rowSums(MisClassTable)[[2]]
  rates['fdr', 1] = MisClassTable[1,2]/colSums(MisClassTable)[[2]]
  rates['for', 1] = MisClassTable[2,1]/colSums(MisClassTable)[[1]]
  
  pr = prediction(p.pred, response)
  
  auc = performance(pr, measure = "auc")@y.values[[1]]
  
  prf = performance(pr, measure = "tpr", x.measure = "fpr")
  par(mfrow=c(1,1))
  plot(prf,main='ROC Curve')
  plt=prf
  
  pout$Error = MisClassError
  pout$Accuracy = Accuracy
  pout$Table = MisClassTable
  pout$Rates = rates
  pout$AUC = auc
  pout$plot = plt
  
  cat('\nMisclassification Error: ',MisClassError,'\n\n')
  cat('Accuracy: ',Accuracy,'\n\n')
  print(rates)
  cat('\nAUC: ',auc)
  return(pout)
  
}
