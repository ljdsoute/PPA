tuneMember <- function(call,tuning,xtest,ytest, predicttype=NULL,probability=TRUE){
  grid <- expand.grid(tuning)
  perf <- numeric()
  for (i in 1:nrow(grid)){
    Call <- c(as.list(call), grid[i,])
    model <- eval(as.call(Call))
    predictions <- predict(model,xtest,type=predicttype, probability=probability)
    predictions <- attr(predictions,"probabilities")[,"1"]
    perf[i] <- AUC::auc(AUC::roc(predictions,factor(ytest)))}
  perf <- data.frame(grid, auc=perf)
  return(perf[which.max(perf$auc),]) 
}