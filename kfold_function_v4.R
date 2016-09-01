kfold = function(formula, family='binomial', data, coords, response, covars, k, tau, weights, n.samples, starting, tuning, priors, cov.model, amcmc, knots, start=1, end, thin=1, verbose=FALSE, n.report=100, type, control.family, control.fixed, name.response){
  
  # type='sp' will run kfold using spGLM
  # type='INLA' will run kfold using inla
  # type='glm' will run kfold using glm (no spatial dependence)
  
  
  if(missing(tau))
    stop('tau is missing')
  
  if(tau<0 | (tau>1&tau!='random'))
    stop('tau must be either numeric between 0 and 1 or tau="random"')

  if(missing(type))
    stop('kfold type not specified: Options for type are "sp", "spknot" or "glm"')
  
  if(type!='sp'&type!='glm'&type!='INLA')
    stop('Incorrect kfold type: Options for type are "sp", "glm" or "INLA"')
  
  if(missing(k))
    stop('k (= number of folds) not specified')
  
  
  par(mfrow=c(ceiling(sqrt(k)),ceiling(sqrt(k))))
  
  n = nrow(data)
  fold.length = ceiling(n/k)
  
  MisClassError = matrix(rep(0,k), 1, k)
  MisClassTable = vector(mode='list', length=k)
  rates = matrix(rep(0,k*8), 8, k)
  auc = matrix(rep(0,k), 1, k)
  plt = vector(mode='list', length=k)
  kout = list()
  
  class(kout) = 'kfold'
  
  rownames(rates) = c('tpr', 'tnr', 'ppv', 'npv', 'fpr', 'fnr', 'fdr', 'for')
  
  
  
  if(type=='sp'){
    
   
    if(missing(covars))
      stop('covars is missing')
    
    if(class(covars)!='matrix')
      stop('covars must be a matrix')
    
    if(!is.numeric(covars))
      stop('covars must be numeric (Hint: Try creating indicator variables to replace categorical variables)')
    
    if(any(is.na(covars)))
      stop('NA values in covars')
    
    if(nrow(covars)!=length(response)|nrow(covars)!=nrow(data))
      stop('Problem with dimensions (Check dimensions of covars, response or data)')
    
    if(missing(data))
      stop('data is missing')
    
    if(missing(coords))
      stop('coords is missing')
    
    if(!is.numeric(coords))
      stop('coords must be of type numeric')
    
    if(any(is.na(coords)))
      stop('NA values in coords')
    
    if(missing(response))
      stop('response is missing')
    
    if(missing(n.samples))
      stop('n.samples is missing')
    
    
    
    for(i in 1:k) {
      
      if(i!=k){
        fold.test = (1 + (i - 1)*fold.length):(i*fold.length)
      } else if(i==k) {
        fold.test = (1 + (k - 1)*fold.length):n
      }
      
      #fit = glm(formula, family=family, data=data[-fold.test, ])
      
      #start.new=NULL
      #start.new=starting
      #start.new$beta.starting = coefficients(fit)
      
      #tune.new=NULL
      #tune.new=tuning
      #tune.new$beta = t(chol(vcov(fit)))
      
      spfit = spGLM(formula, family=family, weights=weights, data=data[-fold.test, ], coords=coords[-fold.test, ], starting=starting, tuning=tuning, priors=priors, amcmc=amcmc, knots=knots, n.samples=n.samples, cov.model=cov.model, verbose=verbose, n.report=n.report)
      
      spPred = spPredict(spfit, pred.coords=coords[fold.test, ], pred.covars=as.matrix(covars[fold.test, ]), start=start, end=end, thin=thin, verbose=verbose, n.report=n.report)
      p.pred = apply(spPred$p.y.predictive.samples,1,mean)
      
      if(tau>=0&tau<=1){
      
        y.pred = ifelse(p.pred >= tau, 1, 0)
        
        MisClassError[, i] = mean(y.pred != response[fold.test])
        MisClassTable[[i]] = table(Actual = response[fold.test], Predicted = y.pred)

      } else if(tau=='random') {
        
        y.pred = rbinom(20*length(p.pred), size=rep(1,20*length(p.pred)), prob=rep(p.pred,20))
        
        MisClassError[, i] = mean(y.pred != rep(response[fold.test],20))
        MisClassTable[[i]] = table(Actual = rep(response[fold.test],20), Predicted = y.pred)/20
        
      }
      
      rates['tpr', i] = MisClassTable[[i]][2,2]/rowSums(MisClassTable[[i]])[[2]] 
      rates['tnr', i] = MisClassTable[[i]][1,1]/rowSums(MisClassTable[[i]])[[1]]
      rates['ppv', i] = MisClassTable[[i]][2,2]/colSums(MisClassTable[[i]])[[2]]
      rates['npv', i] = MisClassTable[[i]][1,1]/colSums(MisClassTable[[i]])[[1]]
      rates['fpr', i] = MisClassTable[[i]][1,2]/rowSums(MisClassTable[[i]])[[1]]
      rates['fnr', i] = MisClassTable[[i]][2,1]/rowSums(MisClassTable[[i]])[[2]]
      rates['fdr', i] = MisClassTable[[i]][1,2]/colSums(MisClassTable[[i]])[[2]]
      rates['for', i] = MisClassTable[[i]][2,1]/colSums(MisClassTable[[i]])[[1]]
      
      pr = prediction(p.pred, response[fold.test])
      
      auc[i] = performance(pr, measure = "auc")@y.values[[1]]
      
      prf = performance(pr, measure = "tpr", x.measure = "fpr")
      plot(prf,main=paste('ROC Fold ',i,'',sep=''))
      plt[[i]] = prf
      
    }
    
    
    
  } else if(type=='glm') {
    
    if(length(response)!=nrow(data))
      stop('Problem with dimensions (Check dimensions of covars, response or data)')
    
    if(missing(data))
      stop('data is missing')
    
    if(missing(response))
      stop('response is missing')
    
    
    
    for(i in 1:k) {
      
      if(i!=k){
        fold.test = (1 + (i - 1)*fold.length):(i*fold.length)
      } else if(i==k) {
        fold.test = (1 + (k - 1)*fold.length):n
      }
      
      fit = glm(formula, family=family, data=data[-fold.test, ])
      
      p.pred = predict(fit, data[fold.test,], type='response')

      if(tau>=0&tau<=1){
      
        y.pred = ifelse(p.pred >= tau, 1, 0)
        
        MisClassError[, i] = mean(y.pred != response[fold.test])
        MisClassTable[[i]] = table(Actual = response[fold.test], Predicted = y.pred)
      
      } else if(tau=='random') {
        
        y.pred = rbinom(20*length(p.pred), size=rep(1,20*length(p.pred)), prob=rep(p.pred,20))
        
        MisClassError[, i] = mean(y.pred != rep(response[fold.test],20))
        MisClassTable[[i]] = table(Actual = rep(response[fold.test],20), Predicted = y.pred)/20

      }
      
      rates['tpr', i] = MisClassTable[[i]][2,2]/rowSums(MisClassTable[[i]])[[2]] 
      rates['tnr', i] = MisClassTable[[i]][1,1]/rowSums(MisClassTable[[i]])[[1]]
      rates['ppv', i] = MisClassTable[[i]][2,2]/colSums(MisClassTable[[i]])[[2]]
      rates['npv', i] = MisClassTable[[i]][1,1]/colSums(MisClassTable[[i]])[[1]]
      rates['fpr', i] = MisClassTable[[i]][1,2]/rowSums(MisClassTable[[i]])[[1]]
      rates['fnr', i] = MisClassTable[[i]][2,1]/rowSums(MisClassTable[[i]])[[2]]
      rates['fdr', i] = MisClassTable[[i]][1,2]/colSums(MisClassTable[[i]])[[2]]
      rates['for', i] = MisClassTable[[i]][2,1]/colSums(MisClassTable[[i]])[[1]]
      
      pr = prediction(p.pred, response[fold.test])
      
      auc[i] = performance(pr, measure = "auc")@y.values[[1]]
      
      prf = performance(pr, measure = "tpr", x.measure = "fpr")
      plot(prf,main=paste('ROC Fold ',i,'',sep=''))
      plt[[i]] = prf
      
    }
    


  } else if(type=='INLA') {
    
    if(any(is.null(data$node)))
      stop('node is missing from data. Add node to data')
    
    if(anyNA(data$node))
      stop('NA value in data$node')
    
    if(length(response)!=nrow(data))
      stop('Problem with dimensions (Check dimensions of response or data)')
    
    if(missing(data))
      stop('data is missing')
    
    if(missing(response))
      stop('response is missing')
    
    if(missing(name.response))
      stop('name.response is missing')
    
    if(class(name.response)!='character')
      stop('name.response must be a character string')
    
    if(missing(control.family))
      stop('control.family is missing')
    
    if(missing(control.fixed))
      stop('control.fixed is missing')
    
    
    for(i in 1:k) {
      
      if(i!=k){
        fold.test = (1 + (i - 1)*fold.length):(i*fold.length)
      } 
      else if(i==k){
        fold.test = (1 + (k - 1)*fold.length):n
      }
      
      data.INLA = data
      data.INLA[fold.test,name.response] = rep(NA,length(fold.test))
      
      
      INLAfit = inla(formula, family = family, data = data.INLA, verbose = verbose,
                     control.family = control.family,
                     control.fixed = control.fixed,
                     control.predictor = list(compute=TRUE,link=1))
      
      p.pred = INLAfit$summary.fitted.values$mean[fold.test]

      if(tau>=0&tau<=1){
      
        y.pred = ifelse(p.pred >= tau, 1, 0)
        
        MisClassError[, i] = mean(y.pred != response[fold.test])
        MisClassTable[[i]] = table(Actual = response[fold.test], Predicted = y.pred)
        
      } else if(tau=='random') {
        
        y.pred = rbinom(20*length(p.pred), size=rep(1,20*length(p.pred)), prob=rep(p.pred,20))
        
        MisClassError[, i] = mean(y.pred != rep(response[fold.test],20))
        MisClassTable[[i]] = table(Actual = rep(response[fold.test],20), Predicted = y.pred)/20
        
      }
      
      rates['tpr', i] = MisClassTable[[i]][2,2]/rowSums(MisClassTable[[i]])[[2]] 
      rates['tnr', i] = MisClassTable[[i]][1,1]/rowSums(MisClassTable[[i]])[[1]]
      rates['ppv', i] = MisClassTable[[i]][2,2]/colSums(MisClassTable[[i]])[[2]]
      rates['npv', i] = MisClassTable[[i]][1,1]/colSums(MisClassTable[[i]])[[1]]
      rates['fpr', i] = MisClassTable[[i]][1,2]/rowSums(MisClassTable[[i]])[[1]]
      rates['fnr', i] = MisClassTable[[i]][2,1]/rowSums(MisClassTable[[i]])[[2]]
      rates['fdr', i] = MisClassTable[[i]][1,2]/colSums(MisClassTable[[i]])[[2]]
      rates['for', i] = MisClassTable[[i]][2,1]/colSums(MisClassTable[[i]])[[1]]
      
      pr = prediction(p.pred, response[fold.test])
      
      auc[i] = performance(pr, measure = "auc")@y.values[[1]]
      
      prf = performance(pr, measure = "tpr", x.measure = "fpr")
      plot(prf,main=paste('ROC Fold ',i,'',sep=''))
      plt[[i]] = prf
      
    }
    
  }
  
  
  
  colnames(MisClassError) = paste('Fold.',1:k,'', sep='')
  rownames(MisClassError) = 'Misclassification Error:'
  
  accuracy = 1 - MisClassError
  rownames(accuracy) = 'Misclassification Accuracy:'
  
  names(MisClassTable) = paste('Fold.',1:k,'', sep='')
  
  colnames(rates) = paste('Fold.',1:k,'', sep='')
  
  mean.rates = matrix(apply(rates,1,mean), 1, nrow(rates))
  colnames(mean.rates) = rownames(rates)
  rownames(mean.rates) = 'Average'
  
  colnames(auc) = paste('Fold.',1:k,'', sep='')
  rownames(auc) = 'AUC:'
  
  names(plt) = paste('Fold.',1:k,'', sep='')
  
  kout$Error = MisClassError
  kout$Mean_Error = mean(MisClassError)
  kout$Accuracy = accuracy
  kout$Mean_Accuracy = mean(accuracy)
  kout$Table = MisClassTable
  kout$Rates = rates
  kout$Mean_Rates = mean.rates
  kout$AUC = auc
  kout$Mean_AUC = mean(auc)
  kout$ROC = plt
  
  
  
  print(MisClassError)
  cat('\nMean Misclassification Error: ',mean(MisClassError),'\n\n')
  print(rates[c('tpr','tnr'),])
  cat('\nMean Sensitivity: ', mean.rates[,'tpr'],'\n')
  cat('\nMean Specificity: ', mean.rates[,'tnr'],'\n\n')
  print(auc)
  cat('\nMean AUC: ', mean(auc),'\n')
  return(kout)
  
}
