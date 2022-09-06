####################### Summary Tables #################################
# Summary table by quintile/tertile/decile variables
CT_level_characteristics <- function(data, byvar_raw, rank_type){
  if(rank_type == "tertile"){
    byvar = paste0(byvar_raw, "_rank_tert")
  }
  if(rank_type == "quintile"){
    byvar = paste0(byvar_raw, "_quintile")
  }
  if(rank_type == "decile"){
    byvar = paste0(byvar_raw, "_rank_deci")
  }
  
  byvar <- as.name(byvar)                        
  byvar_raw <- as.name(byvar_raw)                 
  
  CT_level =  data %>% 
    group_by(!!byvar) %>% 
    summarise(total_pop = sum(CT_pop),
              N = length(unique(CTid)),
              mean = mean(!!byvar_raw, na.rm = T),
              median = median(!!byvar_raw, na.rm = T),
              min = min(!!byvar_raw, na.rm = T),
              max = max(!!byvar_raw, na.rm = T))
  
  return(round(CT_level, 2))}