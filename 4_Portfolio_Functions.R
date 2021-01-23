### Function: Create Single Generic Portfolio ----------------------------------------------------------
get_portfolio <- function(ID,aggregation,factor,decile,return,return_type,metric="mean") {
  
  if (aggregation=="Static") { 
    portfolio <- static_portfolio(factor=factor,decile=decile,return=return,return_type=return_type)
  } else if (aggregation=="Dynamic") {
    portfolio <- dynamic_portfolio(factor=factor,decile=decile,return=return)
    if (return_type=="Excess") {
      portfolio <- dynamic_portfolio(factor="universe",decile="not used",return=return) %>%
        inner_join(y=portfolio,by="period_formatted") %>%
        mutate(excess=metric.y-metric.x) %>%
        select(period_formatted,metric=excess)
    }
  }
  portfolio<-rename(portfolio,!!ID:=metric)
  return(portfolio)
}


### Function: Create Single Static Portfolio -----------------------------------------------------------
static_portfolio <- function(factor,decile,return,return_type="Absolute") {
  
  if (return_type == "Absolute") {
    input.file <- combined
  } else if (return_type == "Excess") {
    input.file <- combined_excess
  }
  
  if (factor!="universe") {  
    decile_high = as.numeric(sub("D","",decile))/10
    decile_low = (as.numeric(sub("D","",decile))-1)/10
    portfolio <- input.file %>% 
      select(identifier, name.x, period_formatted, one_of(factor,return)) %>%
      filter(!is.na(!!sym(factor))) %>% # Remove N/A's
      group_by(period_formatted) %>%
      mutate(count=n(), Rank = rank(!!sym(factor), ties.method="first",na.last = FALSE)) %>%
      filter(Rank > floor(decile_low*count) & Rank <= floor(decile_high*count)) %>%
      summarise(metric=mean(!!sym(return),na.rm=TRUE)) 
  } else {
    portfolio <- input.file %>% 
      select(identifier, name.x, period_formatted, one_of(return)) %>%
      group_by(period_formatted) %>%
      summarise(metric=mean(!!sym(return),na.rm=T)) 
  }
  
  return(portfolio)
}


### Function: Create Single Dynamic Portfolio --------------------------------------------------------------
dynamic_portfolio <- function(factor,decile,return) {
  
  horizon <- as.numeric(sub("w_ret","",sub("fut_","",return)))
  
  portfolio_dynamic <- static_portfolio(factor=factor,decile=decile,return="fut_1w_ret",return_type="Absolute") %>%
    mutate(metric_plus1=(1+metric/100)) %>%
#
# CODE REMOVED WHEN SWITCHED FROM HORIZON IN MONTHS    
#    mutate(period_start = period_formatted - 6,
#           period_end = period_start %m+% months(horizon)) %>% 
#    mutate(period_width = apply(.[4:5],1,function(x) sum(.$period_formatted>min(x) & .$period_formatted<=max(x))),
#           metric_compound=rollapply(data=metric_plus1,width=period_width,FUN=prod,fill=NA,align="left"),
#           metric_final=(metric_compound-1)*100) %>%
#
# CODE ADDED WHEN SWITCHED TO HORIZON IN WEEKS
        mutate(metric_compound=rollapply(data=metric_plus1,width=horizon,FUN=prod,fill=NA,align="left"),
           metric_final=(metric_compound-1)*100) %>%
    select(period_formatted,metric = metric_final)
  
  return(portfolio_dynamic)
}