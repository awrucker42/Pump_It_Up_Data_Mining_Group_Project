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
data$region_code[is.na(data$region_code)] <- 1
Test_Set_Data$status_group = ''
Test_Set_Data$region_code[is.na(Test_Set_Data$region_code)] <- 1
id <- 0

  
  #variables_combn <- combn(variables_total, i)
  c <- 1
  value <- data
  # variables <- variables_combn[,c]
  variables <- c('longitude',
                 'quantity_group',
                 'waterpoint_type',
                 'latitude')

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
  while(c < nrow(Test_Set_Data))#length(variables_combn)/i)
  {


    testing <- Test_Set_Data[c,]

    if(FALSE)
    {
    } 
    else
    {
      id <- testing$id
      testing <- testing[c(variables)]
      for(k in 1:as.integer(length(variables)))
      {
        testing[variables[[k]]] <- as.factor(testing[[variables[[k]]]])
        testing[is.na(testing)] <- 0
        testing[variables[[k]]] <- as.integer(testing[[variables[[k]]]])
        
        testing[variables[[k]]] <- (testing[[variables[k]]] - min(testing[variables[[k]]]))/(min(testing[variables[[k]]]) + max(value[variables[[k]]]))*100
      }
      samplingrate <- 1
      num.test.set.labels <- n.points * (1- samplingrate)
      training <- sample(1:n.points, samplingrate * n.points, replace=FALSE)
      train<- subset(value[training,],select = variables)
      
      
      test <- testing
      cl <- value$status_group[training]
      tryCatch({
          k = 1
          if(k == 0){
            k = 1
          }
          predicted.labels <- knn(train, test, cl, k)
          
          Test_Set_Data[c,]$status_group <- predicted.labels
      }, error=function(e){print(e)})
      warnings()
      
      
    }
    if(c%%100 ==0)
    {
      print(c)
    }
    c <- c+ 1
  }
  print(Sys.time() - startTime)
  startTime <- Sys.time()
  i <- i + 1

print(Sys.time() - startTime)
startTime <- Sys.time()
i <- 1
while(i <= 14850)
{
  SubmissionFormat[i, ]$status_group <- Test_Set_Data[i,]$status_group
  if(SubmissionFormat[i,]$status_group == 1){
    SubmissionFormat[i,]$status_group = 'non functional'
  }
  else if(SubmissionFormat[i, ]$status_group == 2)
  {
    SubmissionFormat[i,]$status_group = 'function needs repair'
  }
  else
  {
      SubmissionFormat[i,]$status_group = 'functional'
    }
  i <- i + 1
}