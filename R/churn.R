#' Shifted Beta Geometric Model
#'
#' This function allows you to implement as shifted beta geometric model
#' @param alpha Alpha (starts as 1)
#' @param beta Beta (starts as 1)
#' @keywords shifted beta geometric
#' @export
#' @examples
#' sBG.log.likelihood()
#'

sBG.log.likelihood<-function(params) {
  a<-params[1]
  b<-params[2]
  alive = df$Subscribers[length(df$Subscribers)]
  lost_subscribers = df$Subscribers[-length(df$Subscribers)] -df$Subscribers[-1]
  time_periods = length(lost_subscribers)
  time = seq(from=1,to=time_periods,by=1)
  ll<-0
  ll<-ll+lost_subscribers*log(beta(a+1,b+time-1)/beta(a,b))
  ll.alive = alive*log(beta(a,b+time_periods)/beta(a,b))
  return(-sum(ll,ll.alive))
}

#' Helper function 1
#'
#' This function allows you to implement as shifted beta geometric model
#' @param alpha Alpha (starts as 1)
#' @param beta Beta (starts as 1)
#' @keywords shifted beta geometric
#' @export
#' @examples
#' sBG.table()
#'

sBG.table<-function(params,data){
  a<-params[1]
  b<-params[2]
  alive = df$Subscribers[length(df$Subscribers)]
  lost_subscribers = df$Subscribers[-length(df$Subscribers)] -df$Subscribers[-1]
  time_periods = length(lost_subscribers)
  time = seq(from=1,to=time_periods,by=1)
  prob_churn = beta(a+1,b+time-1)/beta(a,b)
  survival_function = 1-cumsum(prob_churn)
  ll = lost_subscribers*log(prob_churn)
  ll = c(ll,alive*log(1-sum(prob_churn)))
  ll = c(NA,ll)
  survival_function = c(1,survival_function,NA)
  time = c(0,time,time_periods+1)
  lost_subscribers = c(NA,lost_subscribers,NA)
  prob_churn = c(NA,prob_churn,1-sum(prob_churn))
  subcribers = c(df$Subscribers,NA)
  df = data.frame(time, subcribers, lost_subscribers, prob_churn, ll, survival_function)
}

#' Helper function 2
#'
#' This function allows you to implement as shifted beta geometric model
#' @param alpha Alpha (starts as 1)
#' @param beta Beta (starts as 1)
#' @keywords shifted beta geometric
#' @export
#' @examples
#' sbg_function()
#'

sbg_function<-function(params){
  model.sBG = optim(par=c(1,1),sBG.log.likelihood,lower=c(0.00001,0.000001), method="L-BFGS-B")
  return(model.sBG)
}
