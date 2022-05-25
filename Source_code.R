require(rpart)
library(glmnet)
library(MASS)
library(e1071)
library(randomForest)
library(RobustLinearReg)
library(gbm)

# datasets, urls, and frmla_strings used in i loop (first/outermost loop)
datasets <- c(
  # Charlie (n = 7)
  "ceo_pay", "AIG_daily", "diamonds", "energy", "beijin_pm2.5", "ami_charges", "ny_bridges",
  # Zoe (n = 10)
  "wine_quality", "named_uci_data", "Parkinsons", "sleep", "garments", "weight", "boston", "cholestrol", "graduate", "iceCream",
  # Manraj (n = 8)
  "Saratoga_house_prices", "cigarettes", "forest_fires", "QSAR_fish", "NBA_Betting_Odds", "Predictive_maintenance", "QSAR_aquatic", "Fuel_economy",
  # Harshali (n = 6)
  "Seoul_Bike_Sharing_Demand", "Amazon_book_dataset", "Movie_profit_dataset", "PVC", "Cost_of_living_2018", "Real_Estate_Cost_Evaluation",
  # Sanaz (n = 1)
  "Commercial_properties"
)

frmla_strings <- c(
  "compensation_M ~ Ratio + median_salary", # begin charlie
  "close_open_diff ~ Open + High + Low + Volume + year_day + Month + Day + Year",
  "Price ~ Carat_size + Color + Clarity + Depth + Table + Cut + Report",
  "total_energy ~ lights + T1 + RH_1 + T2 + RH_2 + T3 + RH_3 + T4 + RH_4 + T5 + RH_5 + T6 + RH_6 + T7 + RH_7 + T8 + RH_8 + T9 + RH_9 + T_out + Press_mm_hg + RH_out + Windspeed + Visibility + Tdewpoint + rv1 + rv2",
  "pm2.5 ~ year + month + day + hour + DEWP + TEMP + PRES + cbwd + Iws + Is + Ir",
  "CHARGES ~ LOS + AGE + SEX + DRG + DIED",
  "Condition ~ Built + AgeAtInspection + Manhattan", # end charlie
  "alcohol ~ sulphates + pH + density", # begin zoe
  "tau1 ~ tau2 + tau3 + tau4 + p1 + p2 + p3",
  "age ~ sex + test_time + motor_UPDRS + total_UPDRS ",
  "heart_rate ~ sleep_state + temperature",
  "targeted_productivity ~ team + smv + over_time + incentive + actual_productivity",
  "Weight ~ Density + Age + Height",
  "CRIM ~  DIS + INDUS + NOX + RM + AGE + DIS",
  "chol ~ age + trestbps",
  "public ~ gpa + pared + apply",
  "ice_cream ~ female + video + puzzle", # end zoe
  "Price ~ Size + Baths + Bedrooms + Acres + Age", # begin manraj
  "Nicotine ~ TAR + CO",
  "DC ~ FFMC + DMC + ISI + temp + RH + wind",
  "LC50 ~ CIC0 + SM1_Dz + GATS1i + NdsCH + NdssC + MLOGP",
  "TeamDiff ~ TeamPts + OppPts + OT + OvrUndr + TeamSprd + Team_id + OppId",
  "Torque_nm ~  Air_temp_K + Rotational_speed_rpm + Tool_wear_min",
  "LC50_aquatic ~ TPSA_Tot + SAacc + H050 + MLOGP + RDCHI + GATS1p + nN + C040",
  "CombinedMPG ~ Displacement + Cylinders + Gears + CombCO2 + Sample", # end manraj
  "Rented_Bike_Count ~ Hour + Humidity + Solar_Radiation", # begin harshali
  "List_Price ~  Amazon_Price + height + Width + thick + Weight",
  "US_Gross_ ~ Budget + Run_Time + Critic_Score",
  "Sqrt.Average.Gift ~ Average.Gift + Current.Gift + Num.Children",
  "Cost_of_Living ~ Groceries + Rent + Restaurant",
  "Price ~ Living_area + bedrooms + bathrooms + year + garage", # end harshali
  "Price ~ Stories + Size	+ Fitness_Center + Food_Service +	Atrium + Land + Yr_Ren + Yr_Blt	+ Steel	+ Masonry" # sanaz
)

# used in k loop (third/innermost loop)
methods <- c(
  "lm",
  "rpart",
  "lasso",
  "ridge",
  "elastic",
  "rlm",
  "svm",
  "rf",
  "lqs",
  "boost"
)

# determine sequence length of each for loop; used to determine length of
# column vectors for final table and their index within innermost loop 
len_i <- length(datasets) 
len_j <- 20 # iterations of each data set
len_k <- length(methods) 
vctr_len <- len_i * len_j * len_k 

# output containers to "grow" vectors for final table 
dataset <- vector("character", vctr_len)
ss <- vector("integer", vctr_len)
predictors <- vector("integer", vctr_len)
resp_var <- vector("character", vctr_len)
train_ss <- vector("integer", vctr_len)
test_ss <- vector("integer", vctr_len)
method <- vector("character", vctr_len)
iteration <- vector("integer", vctr_len)
cpu_train <- vector("numeric", vctr_len)
rse_train <- vector("numeric", vctr_len)
adj_rsq_train <- vector("numeric", vctr_len)
rse_test <- vector("numeric", vctr_len)
adj_rsq_test <- vector("numeric", vctr_len)

#=========================================================================
for(i in 1:len_i) {
  
  # ith data set
  df <- get(datasets[i])
  df <- df[complete.cases(df), ]
  
  # clean ith data set (change non-syntactic names; recast, restructure if necessary)
  if(datasets[i] == "ceo_pay") {
    colnames(df)[5:6] <- c("compensation_M", "median_salary")

  } else if(datasets[i] == "AIG_daily") {
    colnames(df)[7:9] <- c("close_open_diff", "adj_close", "year_day")

  } else if(datasets[i] == "diamonds") {
    colnames(df)[1] <- "Carat_size"

  } else if(datasets[i] == "energy") {
    df[, c(2:3, 14:15, 22:29)] <- lapply(df[, c(2:3, 14:15, 22:29)], as.numeric)
    df[, 2:29] <- lapply(df[, 2:29], jitter) # need noise for rlm() to avoid singular fit error
    df$total_energy <- df$Appliances + df$lights # total energy consumption has two components

  } else if(datasets[i] == "wine_quality") {
    colnames(df) <- c("fixed_acidity", "vol_acidity", "citric_acid", "res_sugar", "chlorides", "free_so2", "total_so2", "density", 
                      "pH", "sulphates", "alcohol", "quality")

  } else if(datasets[i] == "iceCream") { 
    colnames(df) <- c("ice_cream", "female", "video" , "puzzle")
    
  } else if(datasets[i] == "forest_fires") {
    colnames(df) <- c("X", "Y", "month", "day", "FFMC", "DMC", "DC", "ISI", 
                      "temp", "RH", "wind", "rain", "area")
   
  } else if(datasets[i] == "QSAR_fish") {
    colnames(df) <- c("CIC0", "SM1_Dz", "GATS1i", "NdsCH", "NdssC", "MLOGP", 
                      "LC50")
    
  } else if(datasets[i] == "NBA_Betting_Odds") {
    colnames(df) <- c("Datenum", "Team", "Dateslash", "OppTeam", "Home", "TeamPts",
                      "OppPts", "OT", "TeamWin", "TeamCov", "TeamSprd", "OvrUndr",
                      "OUCov", "Team_id", "OppId", "TeamDiff", "TotalPts")
    df[, c("OvrUndr", "TeamSprd")] <- lapply(df[, c("OvrUndr", "TeamSprd")], jitter, factor = 3)
    
  } else if(datasets[i] == "Predictive_maintenance") {
    colnames(df) <- c("UDI", "Product_id", "Type", "Air_temp_K", "Process_temp_K",
                      "Rotational_speed_rpm", "Torque_nm", "Tool_wear_min", 
                      "Machine_failure", "TWF", "HDF", "PWF", "OSF", "RNF")

  } else if(datasets[i] == "QSAR_aquatic") {
    colnames(df) <- c("TPSA_Tot", "SAacc", "H050", "MLOGP", "RDCHI", "GATS1p", "nN", "C040", "LC50_aquatic")
    
  } else if(datasets[i] == "Fuel_economy") {
    colnames(df) <- c("Model_year", "Mfr", "Division", "Car_line", "Class", "CityMPG",
                      "HighwayMPG", "CombinedMPG", "Displacement", "Cylinders", "Transmission",
                      "Gears", "CityCO2", "HwyCO2", "CombCO2", "Sample")
   
  } else if(datasets[i] == "Seoul_Bike_Sharing_Demand") {
    colnames(df) <- c("Date", "Rented_Bike_Count", "Hour", "Temperature", "Humidity", 
                      "Wind_speed", "Visibility", "Dew_point_temperature","Solar_Radiation", 
                      "Rainfall", "Snowfall", "Seasons","Holiday", "Functioning_Day")
   
  } else if(datasets[i] == "Amazon_book_dataset") {
    colnames(df) <- c("Title", "Author", "List_Price", "Amazon_Price", "Hard/Paper", "NumPages", 
                      "Publisher", "Pub_year", "ISBN-10", "height", "Width", "thick", "Weight")

  } else if(datasets[i] == "Movie_profit_dataset") {
    colnames(df) <- c("Year", "Movie", "US_Gross_", "Budget", "Run_Time", "Critic_Score")

  } else if(datasets[i] == "PVC") {
    colnames(df) <- c("Age", "Own.Home.", "Num.Children", "Income", "Sex", "Total.Wealth", 
                      "Other.Gifts", "Number.of.Gifts", "Smallest.Gift", "Largest.Gift", 
                      "Previous.Gift", "Time.Between.Gifts", "Average.Gift", "Current.Gift", 
                      "Sqrt.Smallest.Gift", "Sqrt.Largest.Gift", "Sqrt.Previous.Gift",
                      "Sqrt.Average.Gift", "Sqrt.Current.Gift")

  } else if(datasets[i] == "Cost_of_living_2018") {
    colnames(df) <- c("Rank", "City", "Cost_of_Living", "Rent", "COL+Rent", 
                      "Groceries", "Restaurant", "Purchasing_Power")

  } else if(datasets[i] == "Real_Estate_Cost_Evaluation") {
    colnames(df) <- c("Price", "Living_area" , "bedrooms", "bathrooms", "year", "garage", "date_collected", "location_type", "Urban", "Suburb")

  } else if(datasets[i] == "Commercial_properties") {
    colnames(df) <- c("Price", "Stories", "Size", "Fitness_Center", "Food_Service", 
                      "Atrium", "Land", "Yr_Ren", "Yr_Blt", "Steel", "Masonry")
    
  } else { 
    df
  }

  # details for ith data set, used for objects in nested for loops below
  frmla <- as.formula(frmla_strings[i])
  n <- nrow(df)
  p <- all.vars(frmla) |> length() - 1
  dv <- all.vars(frmla)[1]
  
  #=========================================================================
  for(j in 1:len_j) {
    
    # jth random 70/30 split of ith data set
    train_n <- floor(0.7 * n) # sample() also rounds down size argument
    test_n <- n - train_n 
    train_indx <- sample(n, size = train_n)
    train_df <- df[train_indx, ]
    test_df <- df[-train_indx, ]
    
    #=========================================================================
    for(k in 1:len_k) {
      
      # track index for populating each column vector for final table 
      counter <- (i - 1) * len_j * len_k + (j - 1) * len_k + k 
      
      # populate column vectors for final table from objects in outer loops
      dataset[[counter]] <- datasets[i]
      ss[[counter]] <- n
      predictors[[counter]] <- p
      resp_var[[counter]] <- dv
      train_ss[[counter]] <- train_n
      test_ss[[counter]] <- test_n
      method[[counter]] <- methods[k]
      iteration[[counter]] <- j
      
      # apply k different methods to jth instance of ith data set;
      # batch predictions after model training or during for less standardized APIs 
      if(methods[k] == "lm") {
        train_mod <- lm(frmla, train_df) 
        cpu_train[[counter]] <- system.time(lm(frmla, train_df))[3]

      } else if(methods[k] == "rpart") {
        train_mod <- rpart::rpart(frmla, train_df)
        cpu_train[[counter]] <- system.time(rpart::rpart(frmla, train_df))[3]

      } else if(methods[k] == "lasso")  {
        train_mod <- glmnet::glmnet(x = model.matrix(frmla, train_df)[, -1], 
                                    y = train_df[, dv], family = "gaussian")
        cpu_train[[counter]] <- 
          system.time(
            glmnet::glmnet(x = model.matrix(frmla, train_df)[, -1], 
                           y = train_df[, dv], family = "gaussian")
          )[3]
        train_preds <- predict(train_mod, 
                               newx = model.matrix(frmla, train_df)[, -1],
                               s = min(train_mod$lambda)) # picked lower lambda value (closer to OLR coefficients)
        test_preds <- predict(train_mod, 
                              newx = model.matrix(frmla, test_df)[, -1],
                              s = min(train_mod$lambda)) # picked lower lambda value (closer to OLR coefficients)

      } else if(methods[k] == "ridge")  {
        train_mod <- glmnet::glmnet(x = model.matrix(frmla, train_df)[, -1], 
                                    y = train_df[, dv], alpha = 0, family = "gaussian")
        cpu_train[[counter]] <- 
          system.time(
            glmnet::glmnet(x = model.matrix(frmla, train_df)[, -1], 
                           y = train_df[, dv], alpha = 0, family = "gaussian")
          )[3]
        train_preds <- predict(train_mod, 
                               newx = model.matrix(frmla, train_df)[, -1],
                               s = min(train_mod$lambda)) # picked lower lambda value (closer to OLR coefficients)
        test_preds <- predict(train_mod, 
                              newx = model.matrix(frmla, test_df)[, -1],
                              s = min(train_mod$lambda)) # picked lower lambda value (closer to OLR coefficients)
      
      } else if(methods[k] == "elastic")  {
        train_mod <- glmnet::glmnet(x = model.matrix(frmla, train_df)[, -1], 
                                    y = train_df[, dv], alpha = 0.5, family = "gaussian")
        cpu_train[[counter]] <- 
          system.time(
            glmnet::glmnet(x = model.matrix(frmla, train_df)[, -1], 
                           y = train_df[, dv], alpha = 0.5, family = "gaussian")
          )[3]
        train_preds <- predict(train_mod, 
                               newx = model.matrix(frmla, train_df)[, -1],
                               s = min(train_mod$lambda)) # picked lower lambda value (closer to OLR coefficients)
        test_preds <- predict(train_mod, 
                              newx = model.matrix(frmla, test_df)[, -1],
                              s = min(train_mod$lambda)) # picked lower lambda value (closer to OLR coefficients)
        
      } else if(methods[k] == "rlm") {
        train_mod <- MASS::rlm(frmla, train_df)
        cpu_train[[counter]] <- system.time(MASS::rlm(frmla, train_df))[3]

      } else if(methods[k] == "svm") {
        train_mod <- e1071::svm(frmla, train_df)
        cpu_train[[counter]] <- system.time(e1071::svm(frmla, train_df))[3]

      } else if(methods[k] == "rf") {
        train_mod <- randomForest::randomForest(frmla, train_df)
        cpu_train[[counter]] <- system.time(randomForest::randomForest(frmla, train_df))[3]

      } else if(methods[k] == "lqs") {
        train_mod <- MASS::lqs(frmla, train_df)
        cpu_train[[counter]] <- system.time(lqs(frmla, train_df))[3]

      } else if(methods[k] == "boost") {
        train_df[, ] <- lapply(train_df,
                               function(x) if(is.character(x)) as.factor(x) else x)
        test_df[, ] <- lapply(test_df,
                              function(x) if(is.character(x)) as.factor(x) else x)
        train_mod <- gbm::gbm(frmla, distribution = "gaussian", train_df)
        cpu_train[[counter]] <- system.time(gbm::gbm(frmla, distribution = "gaussian", train_df))[3]

      } else {
        stop("Unregistered method in 'methods' object")
      }
      
      
      if(methods[k] %in% c("lm", "rpart", "rlm", "svm", "rf", "lqs", "boost")) {
        train_preds <- predict(train_mod, newdata = train_df)
        test_preds <- predict(train_mod, newdata = test_df)
      } 
      
      # calculate training accuracy of jth instance of ith data set (using kth method)
      tss_train <- sum((train_df[, dv] - mean(train_df[, dv])) ^ 2)
      rss_train <- sum((train_df[, dv] - train_preds) ^ 2)
      rsq_train <- (tss_train - rss_train) / tss_train
      
      rse_train[[counter]] <- sqrt(rss_train / (train_n - p - 1))
      adj_rsq_train[[counter]] <- 
        1 - ( ((train_n - 1) / (train_n - p - 1)) * (1 - rsq_train) )

      # calculate testing accuracy of jth instance of ith data set (using kth method)
      tss_test <- sum((test_df[, dv] - mean(test_df[, dv])) ^ 2)
      rss_test <- sum((test_df[, dv] - test_preds) ^ 2)
      rsq_test <- (tss_test - rss_test) / tss_test
      
      rse_test[[counter]] <- sqrt(rss_test / (test_n - p - 1))
      adj_rsq_test[[counter]] <- 
        1 - ( ((test_n - 1) / (test_n - p - 1)) * (1 - rsq_test) )
    }
  }
}

# construct final dataframe from established vectors
cols <- c("dataset", "ss", "predictors", "resp_var", "train_ss", "test_ss", 
          "method", "iteration", "cpu_train", "rse_train", "adj_rsq_train",
          "rse_test", "adj_rsq_test")
results <- lapply(cols, get)
names(results) <- cols
results <- as.data.frame(results)
print(results)

