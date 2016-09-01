summary.kfold = function(kfold.obj) {
  
  rownames(kfold.obj$Rates)[1:2] = c('True Positive Rate/Sensitivity:','True Negative Rate/Specificity:')
  
  print(rbind(kfold.obj$Error, kfold.obj$Accuracy, kfold.obj$Rates[1:2,],kfold.obj$AUC))
  cat('\nMean Misclassification Error:\t',kfold.obj$Mean_Error,'\nMean Misclassification Accuracy:', kfold.obj$Mean_Accuracy )
  cat('\nMean Sensitivity:\t\t', kfold.obj$Mean_Rates[,'tpr'])
  cat('\nMean Specificity:\t\t', kfold.obj$Mean_Rates[,'tnr'])
  cat('\nMean AUC:\t\t\t', kfold.obj$Mean_AUC,'\n\n')

}

