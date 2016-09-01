plot.kfold = function(kfold.obj, mfrow, type='ROC') {
  
  if(type!='ROC'&type!='tau'&type!='ROC single')
    stop('Incorrect plot type: Options for type are "ROC", "tau" or "ROC single"')
  
  k = length(kfold.obj$ROC)
  
  if(type=='ROC'){
    
    if(missing(mfrow)) {
      
      par(mfrow=c(ceiling(sqrt(k)),ceiling(sqrt(k))))
      
      for(i in 1:k){
        plot(kfold.obj$ROC[[i]], main=paste('ROC Fold ',i,'',sep=''))
        points(kfold.obj$Rates['fpr',i], kfold.obj$Rates['tpr',i], col='red', pch=4, cex=2)
        lines(c(0,0,1), c(0,1,1), col='red', lty=2)
      }
      
    } 
    
    else {
      
      if(prod(mfrow)<k)
        stop('Number of plotting windows too small: Increase values in mfrow')
      
      par(mfrow=mfrow)
      
      for(i in 1:k){
        plot(kfold.obj$ROC[[i]], main=paste('ROC Fold ',i,'',sep=''))
        points(kfold.obj$Rates['fpr',i], kfold.obj$Rates['tpr',i], col='red', pch=4, cex=2)
        lines(c(0,0,1), c(0,1,1), col='red', lty=2)
      }
      
    }
    
  } 
  
  
  else if(type=='ROC single') {
    
    par(mfrow=c(1,1))
    
    if(k==1) {
      
      plot(kfold.obj$ROC[[1]], main=paste('ROC Fold ',1,'',sep=''))
      
    }
    
    else {
      
      plot(kfold.obj$ROC[[1]], main='ROC for all Folds')
      
      for(i in 2:k){
        lines(kfold.obj$ROC[[i]]@x.values[[1]],kfold.obj$ROC[[i]]@y.values[[1]])
      }
      
    }
    
    lines(c(0,0,1), c(0,1,1), col='red', lty=2)
    legend('bottomright', legend=c(paste('min(AUC) = ',min(kfold.obj$AUC),'',sep=''),paste('max(AUC) = ',max(kfold.obj$AUC),'',sep='')),cex=1.25)
    
  } 
  
}

