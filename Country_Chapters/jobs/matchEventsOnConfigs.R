matchEventsOnConfigs <- function(
  events,  # vector with event dates or date frame, where observations are identified by events-within-countries
  config_ts,  # configuration time-series, is data frame with attributes start_date and (optionally) group_id 
  start_date_var,  # variable that has configuration start dates
  event_dates_col = NULL,  #  variable has event dates
  group_var = NULL # variable that identifies groups in configuration time series 
) {

# config_ts: configuration time-series, is data frame with attributes group_id and start_date
  options(warn=-1)
  if (!require('dplyr')) install.packages("dplyr") 
  
  if ( !( class(events) == "data.frame" || is.null(dim(events)) ) ) stop("Input to 'events' argument must be a vector or data frame!","\n")
  if ( !( class(config_ts) == "data.frame" ) ) stop("Input to 'config_ts' argument must be a data frame!","\n")
  if ( !( class(config_ts[, start_date_var]) == "Date" ) ) stop("start_date_var must have format date!","\n")
  
  # compute configuration end dates
  if ( !is.null(group_var) ) {
    names(config_ts)[ names(config_ts) %in% start_date_var ] <- "start_date"
    names(config_ts)[ names(config_ts) %in% group_var ] <- "group_id"
    
  config_ts <- config_ts %>% 
    arrange(group_id, start_date) %>% 
    group_by(group_id) %>% 
    mutate(end_date = lead(start_date-1) ) %>%   
    ungroup %>%
    as.data.frame()
  } else {
    names(config_ts)[ names(config_ts) %in% start_date_var ] <- "start_date"
    config_ts <- config_ts %>% 
      arrange(start_date) %>% 
      mutate(end_date = lead(start_date-1) ) %>%  
      ungroup %>%
      as.data.frame()
  }

  # match event dates to configurations 
  if ( is.null(dim(events)) && typeof(events)!="list" ) {  # if input to events argument is a vector
    occured <- lapply(seq_along(events), function(e) {
      subsetter <- ifelse( is.na(config_ts$end_date), 
                           (config_ts$start_date<=events[e]), 
                           (config_ts$start_date<=events[e] & config_ts$end_date>=events[e])
                          )
      if ( any(subsetter) ) {
        df <- subset(config_ts, subset = subsetter)
        df <- cbind(event_date=events[e],df)
      }
    })
    # names(occured) <- paste("event",seq_along(events),sep="")
  } else if ( length(dim(events)) == 2L ) { # if input to events argument is a data frame
#    config_ts <- as.data.frame(config_ts)
    
    names(events)[ names(events) %in% event_dates_col ] <- "event_date"
    names(events)[ names(events) %in% group_var ] <- "group_id"
    
    joiner <- merge(events,config_ts,by="group_id",all=T)
    
    group_ids <- unique(joiner[, "group_id"])
    occured <- lapply(seq_along(group_ids), function(g) {
      subset(subset(joiner,group_id==group_ids[g]), 
             ifelse( is.na(end_date), 
                     (start_date<=event_date), 
                     (start_date<=event_date & end_date>=event_date)
                   )
      )
    })
  }
  
  if ( !missing(group_var) ) {
    occured <- occured %>%
      do.call(rbind,.) %>%
      arrange(group_id, event_date) %>% 
      as.data.frame()
  } else {
    occured <- occured %>%
      do.call(rbind,.) %>%
      arrange(event_date) %>% 
      as.data.frame()
  }
  
  names(occured)[ names(occured) %in% "group_id" ] <- group_var
  names(occured)[ names(occured) %in% "start_date" ] <- start_date_var
  names(occured)[ names(occured) %in% "event_date" ] <- event_dates_col
  
  options(warn=0)
  occured
  
}
  