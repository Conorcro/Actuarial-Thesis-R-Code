maxtau = function(pred, response) {
  
  SensPlusSpec = function(x, p.pred, act.response) {
    
    y = ifelse(p.pred > x, 1, 0)
    t = table(Actual = act.response, Predicted = y)
    
    if(sum(y)==length(y)|sum(y)==0) {
      sens.plus.spec = 1
    } else {
      sens.plus.spec = (t[2,2]/rowSums(t)[[2]]) + (t[1,1]/rowSums(t)[[1]])
    }
    
    return(sens.plus.spec)
  }
  
  tau.out = list()
  
  tau = as.matrix(seq(0, 1, 0.001))
  
  tau.out$SensPlusSpec = apply(tau,1,SensPlusSpec,p.pred=pred,act.response=response)
  
  tau.choice = tau[which(tau.out$SensPlusSpec == max(tau.out$SensPlusSpec))]
  
  tau.out$max = tau.choice[which.min(abs(tau.choice-0.5))]
  
  return(tau.out)
}
