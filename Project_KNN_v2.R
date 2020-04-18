library(class)
library(dplyr)
library(combinat)
data <- data_set
variables_total <- c('id', 'amount_tsh', 'date_recorded', 'funder', 'gps_height', 'installer', 'longitude', 'latitude', 'wpt_name', 'num_private', 'subvillage', 'region', 'region_code', 'lga', 'ward', 'population', 'public_meeting', 'recorded_by', 'scheme_management', 'scheme_name', 'permit', 'construction_year', 'extraction_type', 'extraction_type_group','extraction_type_class', 'management', 'payment', 'payment_type', 'water_quality', 'quantity_group', 'quantity_group', 'source','source_type', 'source_class', 'waterpoint_type', 'waterpoint_type_group')
minValue <- 1
minVariables <- c('')
minK <- 0
i <- 1
sum <- 0
leftovers = ''
startTime <- Sys.time()
minValue = array(0, dim = length(region_codes) + 1)
minValueK = array(0, dim = length(region_codes) + 1)
data$region_code[is.na(data$region_code)] <- 1
while(i <= length(region_codes))
{

  #variables_combn <- combn(variables_total, i)
  c <- 1
  while(c < 2)#length(variables_combn)/i)
  {
      print(c)
      value <- data
      value <- subset(value, region_code == region_codes[i])
      # variables <- variables_combn[,c]
    variables <- c('longitude',
                   'quantity_group',
                   'waterpoint_type',
                   'latitude')
    c <- c+ 1
    value <- value[c(variables, 'status_group')]
    
    for(k in 1:as.integer(length(variables)))
    {
      value[variables[[k]]] <- as.factor(value[[variables[[k]]]])
      value[is.na(value)] <- 0
      value[variables[[k]]] <- as.integer(value[[variables[[k]]]])
      
      value[variables[[k]]] <- (value[[variables[k]]] - min(value[variables[[k]]]))/(min(value[variables[[k]]]) + max(value[variables[[k]]]))*100
    }
    
    n.points <- nrow(value)
    sum <- sum + n.points
  
      samplingrate <- .8
      num.test.set.labels <- n.points * (1- samplingrate)
      training <- sample(1:n.points, samplingrate * n.points, replace=FALSE)
      train<- subset(value[training,],select = variables)
      
      testing <- setdiff(1:n.points, training)
      
      test <- subset(value[testing,], select=variables)
      cl <- value$status_group[training]
      true.labels <- value$status_group[testing]
      tryCatch({
      for(k in 1:20)
      {
        
        
        predicted.labels <- knn(train, test, cl, k)
        num.incorrect.labels <- sum(predicted.labels != true.labels)
        misclassification.rate <- num.incorrect.labels /num.test.set.labels
       
        if(k == 1)
        {
          minValue[i] = misclassification.rate 
          minValueK[i] = 1
        }
        if(misclassification.rate < minValue[i])
        {
          minVariables <- variables
          minK <- k
          minValueK[i] = k
          minValue[i] <- misclassification.rate
        }
        
      }
      }, error=function(e){})
      warnings()
    print(paste("Number Completed: ", i, " Current Min ", minValue[i]))
    i <- i + 1
    print(Sys.time() - startTime)
    startTime <- Sys.time()
  }
}

i <- 1
last_regions = -1
while(i <= length(region_codes))
{
  if(minValue[i] > .2)
  {
      last_regions <- c(last_regions, region_codes[i])
  }
  i <- i + 1
}

i <- length(region_codes) + 1
c <- 1
while(c < 2)#length(variables_combn)/i)
{
  print(c)
  value <- data[!is.na(data$status_group),]
  value <- subset(value, region_code == last_regions)
  # variables <- variables_combn[,c]
  variables <- c('longitude',
                 'quantity_group',
                 'waterpoint_type',
                 'latitude')
  c <- c+ 1
  value <- value[c(variables, 'status_group')]
  
  for(k in 1:as.integer(length(variables)))
  {
    value[variables[[k]]] <- as.factor(value[[variables[[k]]]])
    value[is.na(value)] <- 0
    value[variables[[k]]] <- as.integer(value[[variables[[k]]]])
    
    value[variables[[k]]] <- (value[[variables[k]]] - min(value[variables[[k]]]))/(min(value[variables[[k]]]) + max(value[variables[[k]]]))*100
  }
  
  n.points <- length(value)

    samplingrate <- .8
    num.test.set.labels <- n.points * (1- samplingrate)
    training <- sample(1:n.points, samplingrate * n.points, replace=FALSE)
    train<- subset(value[training,],select = variables)
    
    testing <- setdiff(1:n.points, training)
    
    test <- subset(value[testing,], select=variables)
    cl <- value$status_group[training]
    true.labels <- value$status_group[testing]
    tryCatch({
      for(k in 1:20)
      {
        
        
        predicted.labels <- knn(train, test, cl, k)
        num.incorrect.labels <- sum(predicted.labels != true.labels)
        misclassification.rate <- num.incorrect.labels /num.test.set.labels
        
        if(k == 1)
        {
          minValue[i] = misclassification.rate 
          minValueK[i] = 1
        }
        if(misclassification.rate < minValue[i])
        {
          minVariables <- variables
          minK <- k
          minValueK[i] = k
          minValue[i] <- misclassification.rate
        }
        
      }
    }, error=function(e){})
    warnings()
  print(paste("Number Completed: ", i, " Current Min ", minValue[i]))
  print(Sys.time() - startTime)
  startTime <- Sys.time()
}
print(last_regions)
print(minValueK)
print("DONE")
print(minValue)