### Function: Get Returns for Weighted Portfolios --------------------------
get_model_returns <- function (
  weights = models_weights,
  returns_act) {
  
  returns_act <- gather(returns_act,key="ID",value="Returns",starts_with("portfolio"))
  returns_act$ID = as.factor(returns_act$ID)
  
  model_results <- weights %>% group_by(model,date) %>%
    left_join(returns_act,by=c("date"="period_formatted","ID"="ID")) %>%
    mutate(weighted_return=weights*Returns) %>%
    summarise(return=sum(weighted_return))
  
  model_results <- model_results%>%group_by(model) %>% mutate(return_plus1=(1+return/100)) %>%
    mutate(cumulative_return=cumprod(return_plus1)) %>% mutate(cumulative_return=(cumulative_return-1)*100) %>%
    select(model,date,return,cumulative_return)
  
  return(model_results)
}
