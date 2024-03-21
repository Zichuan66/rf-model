install.packages(c("VIM", "mice", "missForest"))
#####

library(data.table)
library(dplyr)
library(ggplot2)
library(missForest)

# 1. Read the data
claims <- read.csv("/Users/apple/Desktop/Qualification_Package/Claims_Years_1_to_3.csv")
new <- read.csv("/Users/apple/Desktop/Qualification_Package/Submission_Data.csv")

# converting catogorical variable into 0 and 1
claims$vh_type <- ifelse(claims$vh_type == "Commercial", 0, claims$vh_type)
claims$vh_type <- ifelse(claims$vh_type == "Tourism", 1, claims$vh_type)

#2.data preparation for for claims and new

data_fix <- function(data) {
  # Step 1: Type 1 NA
  # Remove rows with NA values for drv_sex2, drv_age2, drv_age_lic2
  data_ma <- data[complete.cases(data$drv_sex2, data$drv_age2, data$drv_age_lic2), ]
  
  # Setting an indicator variable of 0 and 1 to indicate existence of second driver
  data$second_driver_missing <- ifelse(is.na(data$drv_sex2) | is.na(data$drv_age2) | is.na(data$drv_age_lic2), 1, 0)
  
  # Assigning values and "Unknown" to NA for drv_sex2, 0 for drv_age2 and drv_age_lic2
  data$drv_sex2[is.na(data$drv_sex2)] <- "Unknown" 
  data$drv_age2[is.na(data$drv_age2)] <- 0  # Using 0 as a placeholder for unknown age
  data$drv_age_lic2[is.na(data$drv_age_lic2)] <- 0  # Using 0 as a placeholder for unknown age of license
  
  # Step 2: Type 2 NA, apply missForest to impute missing values for vh_speed, vh_value, vh_weight
  vars_have_NA <- c("vh_speed", "vh_value", "vh_weight")
  data_NA <- data[vars_have_NA]
  data_imputed <- missForest(data_NA)
  
  # Assign imputed values back to the original dataframe
  data[vars_have_NA] <- data_imputed$ximp
  
  return(data)
}

claims=data_fix(claims)
sum(is.na(claims))  #gives 0
new=data_fix(new)
sum(is.na(new))  #gives 0

########################################


#3.Find out the useful predict variable
mean(claims$claim_amount)

get_avg_sev = function(var,data){
  
  agg = data %>% group_by(!!sym(var)) %>% summarise(Severity = mean(claim_amount),Claim_Count = n())
  #pipline, agg returns the mean severity and count by each group.
  scale_factor <- round(max(agg$Claim_Count ) / max(agg$Severity), digit = 2)#round to 2 decimal
  ggplot(agg) +  aes_string(var,"Severity",group = 1) + geom_point() + geom_line()+
    geom_bar(aes(y = Claim_Count  / scale_factor,color = "Claim_Count "), stat = "identity", fill = "black", alpha = 0.1)
  
}

claims$claim_amount[claims$drv_age_lic1 == 76] <- 1187

get_avg_sev("vh_age",claims)                  #useful prediction variable due to distribution
get_avg_sev("vh_fuel",claims)                  #
get_avg_sev("vh_type",claims)
get_avg_sev("pol_usage",claims) 
get_avg_sev("drv_sex2",claims)
get_avg_sev("drv_age1",claims)
get_avg_sev("drv_age_lic1",claims)
get_avg_sev("pol_pay_freq",claims)
get_avg_sev("pol_duration",claims)
get_avg_sev("pol_no_claims_discount",claims)
get_avg_sev("year",claims)
get_avg_sev("pol_payd",claims)
get_avg_sev("population",claims)


claims$claims_amount[claims$drv_age_lic1 == 76] <- 1187
##result:

########################
#4. use random forest to creat model

library(randomForest)
predictors <- c('pol_no_claims_discount','drv_age1','drv_age_lic1', 'drv_age_lic2', 'vh_age', 'vh_type', 'vh_speed', 'vh_value', 'vh_weight', 'population', 'town_surface_area','second_driver_missing')
target_variable <- 'claim_amount'

set.seed(1234) 
# creating 80% of the observations for training
train_indices <- sample(1:nrow(claims), 0.8*nrow(claims)) 
train_data <- claims[train_indices, ]
test_data <- claims[-train_indices, ]

rand_forest <- randomForest(x = train_data[predictors], y = train_data[[target_variable]], importance = TRUE)
claims$est_1_to_3 = predict(rand_forest, newdata = claims)


RMSE = function(x,y){
  MSE = sum((y - x)^2)/length(x)
  return(MSE^.5)
}

RMSE(claims$claim_amount,claims$est_1_to_3)
#gives 1028.203, which looks a better estimate for severity amont 

#apply this model to the new dataset
new$Severity_Est <- predict(rand_forest, newdata = new)
mean(new$Severity_Est)#1236.369
summary(new)


############################
# test whether the model gives RMSE small enough

library(caret)

set.seed(1234)
#try 5-fold cross-validation since 10-fold takes too long to generate

ctrl <- trainControl(method = "cv", number = 5)  
grid_rf <- expand.grid(mtry = seq(1, sqrt(length(predictors)), by = 1))

# Train Random Forest model
rs_result <- train(x = train_data[predictors], 
                   y = train_data[[target_variable]], 
                   method = "svmRadial",
                   metric = "RMSE",
                   trControl = ctrl,
                   tuneGrid = grid_rf)

rs_result

#see best reult in trained result
best_model <- rs_result$finalModel
claims$est_new = predict(best_model, newdata = claims)

RMSE = function(x,y){
  MSE = sum((y - x)^2)/length(x)
  return(MSE^.5)
}

RMSE(claims$claim_amount,claims$est_new)


###########################
output <- claims$claim_amount

# Convert the output to a data frame
output_df <- data.frame(output)

# Export the data frame to a CSV file
write.csv(output_df, file = "output.csv1", row.names = FALSE)
