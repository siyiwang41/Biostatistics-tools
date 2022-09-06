# Many analyses about social determinants of health (SDOH) require to divide census tracts (CT) into quanitle or tertile. 
# Some need to be adjusted by CT population, or ranking within public health units (PHU)

################################# Ranking Functions ##################################
# Function to make population weighted quintile of SDOH
PopWeightedQuintile <- function(data, quintileCol, weightedCol){
  
  data[,quintileCol] <- as.numeric(data[,quintileCol])
  data[,quintileCol] <- ifelse(data[,quintileCol]<0, NA, data[,quintileCol])
  data <- data[!is.na(data[,quintileCol]),]
  data <- arrange(data, !!sym(quintileCol))
  cum_pop <- cumsum(data[,weightedCol])
  cum_pop_percent <- cum_pop/sum(data[,weightedCol])
  data$quintile <- NULL
  
  
  for (i in 1:dim(data)[1]){
    
    if (cum_pop_percent[i] <= 0.2) {
      data$quintile[i] <- 1
    } else if (cum_pop_percent[i] > 0.2 & cum_pop_percent[i] <= 0.4) {
      data$quintile[i] <- 2
    } else if (cum_pop_percent[i] > 0.4 & cum_pop_percent[i] <= 0.6) {
      data$quintile[i] <- 3
    } else if (cum_pop_percent[i] > 0.6 & cum_pop_percent[i] <= 0.8) {
      data$quintile[i] <- 4
    } else {data$quintile[i] <- 5}
    
  }
  
  names(data)[names(data)=="quintile"] <- paste0(quintileCol, "_quintile")
  return(data)
  
}



# Function to make population weighted quintile of SDOH ranking within PHU
PopWeightedQuintile_withinPHU <- function(data, quintileCol, weightedCol, PHU){
  columnnames <- c(quintileCol,weightedCol)
  
  data[,PHU] <- as.factor(data[,PHU])
  split_data <- split(data, data[,PHU])
  
  phu_subset <- NULL
  split_data_i <- NULL
  ranked_phu_subset <- NULL
  
  for (i in 1:nlevels(data[,PHU])) {
    split_data_i <- split_data[[i]]
    ranked_phu_subset <- PopWeightedQuintile(data=split_data_i, quintileCol = columnnames[1], weightedCol = columnnames[2])
    phu_subset <- rbind(phu_subset, ranked_phu_subset)
    
  }
  
  names(phu_subset)[names(phu_subset)==paste0(quintileCol, "_quintile")] <- paste0(quintileCol, "_quintile_withinPHU")
  return(phu_subset)
}



# Function to make population weighted tertile of SDOH
PopWeightedTertile <- function(data, tertileCol, weightedCol){
  
  data[,tertileCol] <- as.numeric(data[,tertileCol])
  data[,tertileCol] <- ifelse(data[,tertileCol]<0, NA, data[,tertileCol])
  data <- data[!is.na(data[,tertileCol]),]
  data <- arrange(data, !!sym(tertileCol))
  cum_pop <- cumsum(data[,weightedCol])
  cum_pop_percent <- cum_pop/sum(data[,weightedCol])
  data$tertile <- NULL
  
  
  for (i in 1:dim(data)[1]){
    
    if (cum_pop_percent[i] <= 0.33) {
      data$tertile[i] <- 1
    } else if (cum_pop_percent[i] > 0.33 & cum_pop_percent[i] <= 0.67) {
      data$tertile[i] <- 2
    } else {data$tertile[i] <- 3}
    
  }
  
  names(data)[names(data)=="tertile"] <- paste0(tertileCol, "_tertile")
  return(data)
  
}




# Function to make population weighted tertile of SDOH ranking within PHU
PopWeightedtertile_withinPHU <- function(data, tertileCol, weightedCol, PHU){
  columnnames <- c(tertileCol,weightedCol)
  
  data[,PHU] <- as.factor(data[,PHU])
  split_data <- split(data, data[,PHU])
  
  phu_subset <- NULL
  split_data_i <- NULL
  ranked_phu_subset <- NULL
  
  for (i in 1:nlevels(data[,PHU])) {
    split_data_i <- split_data[[i]]
    ranked_phu_subset <- PopWeightedTertileQuintile(data=split_data_i, tertileCol = columnnames[1], weightedCol = columnnames[2])
    phu_subset <- rbind(phu_subset, ranked_phu_subset)
    
  }
  
  names(phu_subset)[names(phu_subset)==paste0(tertileCol, "_tertile")] <- paste0(tertileCol, "_tertile_withinPHU")
  return(phu_subset)
}


