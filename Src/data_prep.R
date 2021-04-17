house <- read.csv('../Data/housing_price.csv', header = T)

makeBuildingType <- function(x){
  if(!is.na(x)){
    if(x==1){
      return('Tower')
    }
    else if (x==2){
      return('Bungalow')
    }
    else if (x==3){
      return('Mix_plate_tower')
    }
    else if (x==4){
      return('plate')
    }
    else return('wrong_coded')
  }
  else{return('missing')}
}
house$buildingType <- sapply(house$buildingType, makeBuildingType)

makeRenovationCondition <- function(x){
  if(x==1){
    return('Other')
  }
  else if (x==2){
    return('Rough')
  }
  else if (x==3){
    return('Simplicity')
  }
  else if (x==4){
    return('Hardcover')
  }
}
house$renovationCondition <- sapply(house$renovationCondition, makeRenovationCondition)

makeBuildingStructure <- function(x){
  if(x==1){
    return('Unknown')
  }
  else if (x==2){
    return('Mix')
  }
  else if (x==3){
    return('Brick_Wood')
  }
  else if (x==4){
    return('Brick_Concrete')
  }
  else if (x==5){
    return('Steel')
  }
  else if (x==6){
    return('Steel_Concrete')
  }
}
house$buildingStructure <- sapply(house$buildingStructure, makeBuildingStructure)

house$elevator <- ifelse(house$elevator==1,'has_elevator','no_elevator')
house$subway <- ifelse(house$subway==1,'has_subway','no_subway')
house$fiveYearsProperty <- ifelse(house$fiveYearsProperty==1,'owner_less_5y','owner_more_5y')

house$constructionTime <-as.numeric(house$constructionTime)

house$district <-as.factor(house$district)
house$buildingType <- as.factor(house$buildingType)
house$renovationCondition <- as.factor(house$renovationCondition)
house$buildingStructure <- as.factor(house$buildingStructure)
house$elevator <- as.factor(house$elevator)
house$fiveYearsProperty <- as.factor(house$fiveYearsProperty)
house$subway <- as.factor(house$subway)
house$district <- as.factor(house$district)
# house$tradeTime <- as.character.Date(house$tradeTime)

#calculate house age
max.age = max(house$constructionTime)
relative.age = max.age - house$constructionTime
house = cbind(house, relative.age)

#calculate distance
library(geosphere)
beijing.lat = 39.916668
beijing.log = 116.383331
distance = distHaversine(cbind(beijing.log, beijing.lat), cbind(house$Lng, house$Lat))
house = cbind(house, distance)

#saving in rds (if u needed)
# saveRDS(house, "housing_data.rds")
