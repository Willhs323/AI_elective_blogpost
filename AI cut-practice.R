rm(list=ls(all=T))
options(stringAsFactors = F)
options("scipen" = 100, "digits" = 4)
library(ggpubr)
library(descr)
library(dplyr)
library(survival)
library(survminer)
library(C50)
library(gmodels) # for cross table
setwd("/Users/wills/Documents - PC/R projects/AI/AI elective project/")
ansvt <- read.csv("Copy of NSVT_WS_no_identifiers.csv", na.strings = c("", "NA"))
View(ansvt)
str(ansvt)


newvars <- ansvt %>% 
  select(c("Age", "Sex", "EF", "IVST", 
           "PWT", "LAVI", "BB", "CCB", "AA", "Hs.cTnT",
           "NYHA", "NT.proBNP","Mayo.stage",
           "Syncope","NSVT.1","Runs.per.monitoring.period",
           "Longest","VE..","Atrial.fibrillation",
           "VT_VF_WS", "Mortality")
         ) %>% 
  mutate(across(c(Sex, BB, CCB, AA, 
                  Syncope, NSVT.1, 
                  Atrial.fibrillation,
                  VT_VF_WS, Mortality), 
                as.factor)
  )
dim(newvars)
colSums(is.na(newvars))

colSums(is.na(train_data))

## Need to impute ##
# data missing at random - some didn't have it

newvars <- newvars %>% 
  mutate(
    IVST = if_else(is.na(IVST), median(IVST, na.rm = TRUE), IVST),
    PWT = if_else(is.na(PWT), median(PWT, na.rm = TRUE), PWT),
    LAVI = if_else(is.na(LAVI), median(LAVI, na.rm = TRUE), LAVI),
    Hs.cTnT = if_else(is.na(Hs.cTnT), median(Hs.cTnT, na.rm = TRUE), Hs.cTnT),
    NYHA = if_else(is.na(NYHA), median(NYHA, na.rm = TRUE), NYHA),
    NT.proBNP = if_else(is.na(NT.proBNP), median(NT.proBNP, na.rm = TRUE), NT.proBNP),
    Mayo.stage = if_else(is.na(Mayo.stage), median(Mayo.stage, na.rm = TRUE), Mayo.stage)
  )





# Forest treee plot, and gradient boost - DONE
  # random numbers
  set.seed(900)
  train_sample <- sample(217,163)
  train_sample
  length(train_sample)
  
# test and train datsets and make model
  train_data <- newvars[train_sample,] # 163 items long
  test_data <- newvars[-train_sample,] # 54 items long
  freq(train_data$Mortality)
  freq(test_data$Mortality)
            amyloidmod <- C5.0(as.factor(Mortality) ~ Age + EF + NSVT.1 + IVST + PWT + LAVI + Tafamidis.baseline + LGE.extensive +
                       VE.. + NYHA + Runs.per.monitoring.period + Mayo.stage + Syncope + Longest + VT_VF_WS,
                     data = train_data)
  set.seed(1234)
  amyloidmod <- C5.0(as.factor(Mortality) ~., data = train_data)
  amyloidmod
  summary(amyloidmod)
  x11()
  plot(amyloidmod)
  set.seed(1234)
  amyloidpredict <- predict(object = amyloidmod, newdata = test_data)  # makes a vector with probabilities
  amyloidpredict
# to cross tabulate predicted vs actual
  CrossTable(test_data$Mortality, amyloidpredict, 
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual mortality', 'predicted mortality'))  
  # 18 incorrect
  
  
  
  # boost 10x - DONE
  amyloidmod_10x <- C5.0(as.factor(Mortality) ~., data = train_data,trials = 10)
  amyloidmod_10x
  summary(amyloidmod_10x)
  x11()
  plot(amyloidmod_10x)
  
  amyloidpredict_10x <- predict(object = amyloidmod_10x, newdata = test_data)  # makes a vector with probabilities
  amyloidpredict_10x
  # to cross tabulate predicted vs actual
  CrossTable(test_data$Mortality, amyloidpredict_10x, 
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual mortality', 'predicted mortality'))  
  # now 14 incorrect however all are not predicted to have mortality. only 2/14 correct for actual mortality
  
  

  # 10-fold CV - DONE
  ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
  grid <- expand.grid(model = "tree",
                      trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                      winnow = FALSE)
  grid
  set.seed(300)
  m <- train(Mortality ~., data = newvars, method = "C5.0",
             metric = "Kappa",
             trControl = ctrl,
             tuneGrid = grid)
  m
  length(m)
  CrossTable(m, newvars$Mortality, 
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual mortality', 'predicted mortality')) 
  
  model <- train(Mortality ~., data = train_data, method = "C5.0",
                 metric = "Kappa",
                 trControl = ctrl,
                 tuneGrid = grid)
  model
  predictions <- predict(model, newdata = test_data)
  table(Predicted = predictions, actual = test_data$Mortality)
  
  
  
  
  
  
  # bagging
  library(ipred)
  set.seed(123)
  mybag <- bagging(Mortality ~., data = newvars, nbagg =25)
  mortality_predict <- predict(mybag, newvars)
  table(mortality_predict, newvars$Mortality)
  
  
  # boosting
  library(adabag)
  set.seed(300)
  m_adaboost <- boosting(Mortality ~., data = newvars)
  p_adaboost <- predict(m_adaboost, newvars)
  head(p_adaboost)
  
  adaboost.cv <- boosting.cv(Mortality ~., data = newvars)
  adaboost.cv$confusion
  library(vcd)
  Kappa(adaboost.cv$confusion)
  
  adaboost_cv <- boosting.cv(Mortality~., data = train_data)
  p_adaboost <- predict(adaboost_cv, test_data)
  adaboost_cv$confusion
  
  
  # random forests
  library(randomForest)
  set.seed(300)
  rf <- randomForest(Mortality ~., data = newvars)
  rf
  library(vcd)
  rf$confusion
  dim(rf$confusion)
  Kappa(rf$confusion[1:2, 1:2])
  
  
  # gradient boosting - gbm
  set.seed(123)
  library(gbm)
  set.seed(300)
  str(newvars)
  newvars$Mortality <- ifelse(newvars$Mortality == "1", 1, 0)
  set.seed(900)
  train_sample <- sample(217,163)
  train_sample
  length(train_sample)
  # test and train datsets and make model
  train_data <- newvars[train_sample,] # 163 items long
  test_data <- newvars[-train_sample,] # 54 items long
  m_gbm <- gbm(Mortality~., data = train_data)
  m_gbm
  p_gbm <- predict(m_gbm, test_data, type = "response")
  p_gbm_c <- ifelse(p_gbm >0.50, 1, 0)
  p_gbm_c
  table(test_data$Mortality, p_gbm_c)
  
  # GbM and caret
  grid_gbm <- expand.grid(
    n.trees = c(100, 150, 200),
    interaction.depth = c(1,2,3),
    shrinkage = c(0.01, 0.1, 0.3),
    n.minobsinnode =10
  )
  
  ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "best")
  
  m_gbm_c <- train(as.factor(Mortality) ~., data = newvars, method = "gbm",
                   trControl = ctrl, tuneGrid = grid_gbm, 
                   metric = "Kappa",
                   verbose = FALSE)
  m_gbm_c
  
  
  
  
  # XG BOOST
  library(xgboost)
  library(Matrix)
  newvarssparse <- sparse.model.matrix(~. -Mortality, data = newvars)
  dim(newvarssparse)
  print(newvarssparse[1:5, 1:15])
  
  newvarssparse <- newvarssparse[,-1] # remove intercepts
  set.seed(12345)
  train_sample <- sample(217,163)
  train_data <- newvarssparse[train_sample,] # 163 items long
  test_data <- newvarssparse[-train_sample,] # 54 items long
  
  test_data_labels <- ifelse(newvars[-train_sample, c("Mortality")] == 1, 1, 0)
  train_data_labels <- ifelse(newvars[train_sample, c("Mortality")] == 1, 1, 0)
  
  str(test_data_labels)
  str(train_data_labels)
  
  params.xgb <- list(objective = "binary:logistic",
                     max_depth = 6,
                     eta = 0.3, 
                     gamma = 0,
                     colsample_bytree = 1,
                     min_child_Weight =1,
                     subsample = 1)
  
  xgbm <- xgboost(params.xgb, data = train_data,
                  label = train_data_labels,
                  nrounds = 100,
                  verbose = 1,
                  print_every_n = 10,
                  eval_metric = "logloss")
  
  prob_mortality <- predict(xgbm, test_data)
  prob_mortality
  
  predict_mortality <- ifelse(prob_mortality >= 0.5, 1, 0)
  
  table(predict_mortality, test_data_labels)
  library(vcd)
  kappa(table(predict_mortality, test_data_labels))
  
  library(pROC)
  roc_curve_xgb <- roc(response = test_data_labels, predictor = prob_mortality)
  plot(roc_curve_xgb)
  auc(roc_curve_xgb)
  
  
  # make a tuning grid
  grid_xgb <- expand.grid(
    eta = c(0.3, 0.4),
    max_depth = c(1,2,3),
    colsample_bytree = c(0.6, 0.8),
    subsample = c(0.5, 0.75, 1.00),
    nrounds = c(50, 100, 150),
    gamma = c(0,1),
    min_child_weight = 1
  )
  
  
  library(caret)
  ctrl <- trainControl(method = "cv",
                       number = 10,
                       selectionFunction = "best")
  set.seed(300)
  m_xgb <- train(Mortality ~., data = newvars, method = "xgbTree",
                 trControl = ctrl, tuneGrid = grid_xgb,
                 metric = "Kappa", verbosity = 0)
  m_xgb$bestTune
  max(m_xgb$results["Kappa"])

  
  
  
  
  
  
  # tuning hyperparameters - ch 14, page 601
  modelLookup("C5.0")
  library(caret)
  set.seed(300)
  str(train_data)
  # by default, caret will search through at most 3 values for p parameters.
  # model and winnow have 2 vales so it makes 12 trees. 3x2x2
  # 25 bootstrapped parameters per = 300 models
  m <- train(Mortality ~., data = train_data, method = "C5.0")
  str(m)
  m
  # final model is 10, model = rules, winnow = false
  p <- predict(m, newvars$Mortality)
  p
  length(p)
  table(p, newvars$Mortality)
  str(newvars)
  
  
  # Cross validation
  library(caret)
  set.seed(123)
  train_control <- trainControl(method = "cv", number = 25)
  train_data$Mortality
  model <- train(Mortality ~., data = train_data, method = "C5.0", trControl = train_control)
  warnings()
  print(model)
  
  p <- predict(object = model, newdata = test_data)
  p
  
  CrossTable(test_data$Mortality, p, 
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual mortality', 'predicted mortality')) 
  
  library(caret)
  library(irr)

  folds <- createFolds(newvars$Mortality, k = 10)
  str(folds)
  
  train_1 <- newvars[folds$Fold01,]
  test_1 <- newvars[-folds$Fold01,]
  
  CV_fold_results <- lapply(folds, function(x){
    amyloid_train <- newvars[-x,]
    amyloid_test <- newvars[x,]
    amyloid_model <- C5.0(Mortality ~., data = amyloid_train)
    amyloid_predict <- predict(amyloid_model, amyloid_test)
    amyloid_actual <- amyloid_test$Mortality
    kappa <- kappa2(data.frame(amyloid_actual, amyloid_predict))$value
  })
  str(CV_fold_results)
  mean(unlist(CV_fold_results))
  
  
  
  credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
  
  set.seed(123)
  folds <- createFolds(credit$default, k = 10)
  
  cv_results <- lapply(folds, function(x) {
    credit_train <- credit[-x, ]
    credit_test <- credit[x, ]
    credit_model <- C5.0(default ~ ., data = credit_train)
    credit_pred <- predict(credit_model, credit_test)
    credit_actual <- credit_test$default
    kappa <- kappa2(data.frame(credit_actual, credit_pred))$value # compare values with kappa2
    return(kappa)
  })
  
  # examine the results of the 10 trials
  str(cv_results) # cvresults make the list of kappa values - returned by the function
  
  # compute the average kappa across the 10 trials
  mean(unlist(cv_results)) # its stored in a list, need to unlist it to a numeric vector
  
  ###
  # Cost matrix - not working
  matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
  names(matrix_dimensions) <- c("predicted", "actual")
  matrix_dimensions
  # build the matrix
  error_cost <- matrix(c(0, 1, 20, 0), nrow = 2, dimnames = matrix_dimensions) # r fills by columns
  error_cost # say a loan default costs the bank 4x as much as a missed opportunity
  amyloidmod_cf <- C5.0(as.factor(Mortality) ~., data = train_data, costs = error_cost)
  amyloidmod_cf
  amyloidpredict_cf <- predict(object = amyloidmod_cf, newdata = test_data)  # makes a vector with probabilities
  amyloidpredict_cf
  # to cross tabulate predicted vs actual
  CrossTable(test_data$Mortality, amyloidpredict_10x, 
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual mortality', 'predicted mortality')) 

  
  
  
  amyloidmod <- C5.0(as.factor(Mortality) ~., data = train_data)
  amyloidmod
  
  
  cost_matrix <- matrix(c(0,10,1,0), 
                        nrow =2, byrow = TRUE, 
                        dimnames = list(c("predict no", "predict yes"),
                                        c("actual no", "actual yes")))
  cost_matrix
  
  amyloidmod_cf <- C5.0(as.factor(Mortality) ~., data = train_data, costs = cost_matrix)
  amyloidmod_cf
  amyloidpredict_cf <- predict(object = amyloidmod_cf, newdata = test_data)  # makes a vector with probabilities
  amyloidpredict_cf
  CrossTable(test_data$Mortality, amyloidpredict_cf, 
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual mortality', 'predicted mortality')) 
  
  ###
  

  
# using rules to show what is going on - cannot get this to work
  library(OneR)
  amyloid_1R <- OneR(Mortality ~., data = na.omit(newvarstidy))
  amyloid_1R
  
  sapply(test_data, class)
  
# visualing relationships among features: scatterplot matrix
  pairs(insurance[c("age", "est_value", "miles_driven",
                    "expenses")], pch = ".")
  
  
  
  # regression
  
  
  # regression tree
    library(rpart) # rpart - recursive partitioning - from CART team
    amyloid.part <- rpart(Mortality ~., data = train_data)
    amyloid.part
    summary(amyloid.part)
    library(rpart.plot)
    rpart.plot(amyloid.part, digits = 3)
    rpart.plot(amyloid.part, digits =4, fallen.leaves = TRUE, type = 3, extra = 101)
    p.amyloid.part <- predict(amyloid.part, test_data)
    p.amyloid.part
    
    
    
    predicted_classes <- ifelse(p.amyloid.part[,2] >= 0.5, 1, 0)
    table(predicted = predicted_classes, Actual = test_data$Mortality)
    
    
    
    
    
    # cubist model
    library(Cubist) # enhancement of M5 model tree algorithm
      # cubist makes a decision tree, makes decision rules based on branches of the tree, and makes a regression at each leaf node
      # prooning and boosting are also used
    
      # m <- cubist(train, class) - train is matrix with training data, class is a factor vector with class for each row in training data
      # p <- predict (m, test)
    
      # cubist doesn't accept r syntax, must specify the data frame columns for dependent and independent variables
      m.cubist <- cubist(x = wine_train[-12], y = wine_train$quality)
    
      # display basic information about the model tree
      m.cubist
      # made 25 rules to model wine quality
    
      # display the tree itself
      summary(m.cubist)
      # similar to regression tree, but nodes terminate in linear model not a numeric prediction
      # regression models only apply to examples reaching that node on the tree
    
      # generate predictions for the model
      p.cubist <- predict(m.cubist, wine_test)
    
      # summary statistics about the predictions
      summary(p.cubist)
    
      # correlation between the predicted and true values
      cor(p.cubist, wine_test$quality)
    
      # mean absolute error of predicted and true values
      # (uses a custom function defined above)
      MAE(wine_test$quality, p.cubist)
  
      
      View(train_data)
      
      amyloid.cubist <- cubist(x = train_data[,-21], y = as.numeric(train_data$Mortality))
      amyloid.cubist
      summary(amyloid.cubist)
      p.cubist <- predict(amyloid.cubist, train_data)
      summary(p.cubist)
      cor(p.cubist, as.numeric(train_data$Mortality))
  
  # data partition to split equally
      in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
      credit_train <- credit[in_train, ]
      credit_test <- credit[-in_train, ]
      # 10-fold CV
      # repeated holdout - uses average of several holdout samples
      # k fold cross validation - randomly divides to folds. standard is to use 10 - train on 90%, hold out 10. x10, then average them
      set.seed(123) # to ensure results match
      folds <- createFolds(credit$default, k = 10)
      str(folds) # result has the row numbers
      credit01_test <- credit[folds$Fold01, ]
      credit01_train <- credit[-folds$Fold01, ]
  
  
  
      library(caret) # to make folds
      library(C50) # to make decision tree
      library(irr) # to calculate kappa
      
      credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
      
      set.seed(123)
      folds <- createFolds(credit$default, k = 10)
      
      cv_results <- lapply(folds, function(x) {
        credit_train <- credit[-x, ]
        credit_test <- credit[x, ]
        credit_model <- C5.0(default ~ ., data = credit_train)
        credit_pred <- predict(credit_model, credit_test)
        credit_actual <- credit_test$default
        kappa <- kappa2(data.frame(credit_actual, credit_pred))$value # compare values with kappa2
        return(kappa)
      })
  
      
      # logistic regression
      simple_model <- glm(Mortality ~., family = binomial(link = "logit"), data = train_data)
      simple_model
      
      sw_forward <- stats::step(simple_model, scope = formula(simple_model),
                                direction = "forward")
      sw_forward
      
      
      p <- predict(object = simple_model, newdata = test_data, type = "response")
      p
      str(p)
      
      test_data$Mortality
      
      
      library(pROC)
      roc_curve_logistic_regression <- roc(response = test_data$Mortality, predictor = p)
      plot(roc_curve_logistic_regression)
      auc(roc_curve_logistic_regression)
      
      
  
      
      
      # specify the simplest logistic regression model
      simple_model <- glm(Survived ~ 1, family = binomial, data = titanic_train)
      # forward stepwise regression
      # use stats::step() to get the right step function. 
      sw_forward <- stats::step(simple_model, scope = formula(full_model),
                                direction = "forward")
      # backward stepwise
      sw_backward <- stats::step(full_model, direction = "backward")
      
      
     
      
      # Boruda
      library(Boruta)
      amyloid_boruta <- Boruta(Mortality ~., data = train_data, doTrace =1)
      amyloid_boruta
      View(amyloid_boruta)
      x11()
      plot(amyloid_boruta)
      
      
      
 
      
      
      # look up the tuning parameters for C5.0
      modelLookup("C5.0")
      # there are 12 models that it can use - 3 variables and 2/3 are binary
      # caret will select the best model out of those
      # without specifying, it will use RMSE on a bootstrap sample to find teh best performer
      
      
      set.seed(300)
      m <- train(Mortality ~., data = train_data, method = "C5.0")
      
      
      # automated parameter tuning of C5.0 decision tree 
      set.seed(300)
      m <- train(default ~ ., data = credit, method = "C5.0")
      # 25 bootstrap samples, 12 models = 300 decision trees to make
      
      # summary of tuning results
      m
      # dataset has 1000 samples, 2 classes - if loan defaults or not
      
      # apply the best C5.0 candidate model to make predictions
      # the best model is stored in m$finalModel, however if you use predict() it will take the best one automatically
      p <- predict(m, credit)
      table(p, credit$default)
      # 99.8% accuracy but note this is both test and train
      # the bootstrap accuracy was 73% which is more likely a better estimator of performance
      
      # obtain predicted classes
      head(predict(m, credit))
      
      # obtain predicted probabilities
      head(predict(m, credit, type = "prob"))
      
      
      
      ## Customizing the tuning process ----
      # use trainControl() to alter resampling strategy
      ctrl <- trainControl(method = "cv", number = 10,
                           selectionFunction = "oneSE")
      
      # use expand.grid() to create grid of tuning parameters
      grid <- expand.grid(model = "tree",
                          trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                          winnow = FALSE)
      
      # look at the result of expand.grid()
      grid
      
      # customize train() with the control list and grid of parameters 
      set.seed(300)
      m <- train(default ~ ., data = credit, method = "C5.0",
                 metric = "Kappa",
                 trControl = ctrl,
                 tuneGrid = grid)
      
      # see the results
      m
      
      
      ## Bagging ----
      # Using the ipred bagged decision trees
      library(ipred)
      credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
      set.seed(300)
      mybag <- bagging(default ~ ., data = credit, nbagg = 25)
      credit_pred <- predict(mybag, credit)
      table(credit_pred, credit$default)
      
      # estimate performance of ipred bagged trees
      library(caret)
      credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
      set.seed(300)
      ctrl <- trainControl(method = "cv", number = 10)
      train(default ~ ., data = credit, method = "treebag",
            trControl = ctrl)
      
      
      
      ## For adaboost - do you use the whole model??? ##
      # create a Adaboost.M1 model
      library(adabag)
      credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
      set.seed(300)
      
      amy_adaboost <- boosting(Mortality ~., data = train_data)
      p_adaboost <- predict(amy_adaboost, test_data)
      p_adaboost
      p_adaboost$confusion
      
      # adaboost with 10-fold CV
      amy_adaboost <- boosting.cv(Mortality ~., data = train_data)
      amy_adaboost$confusion
      
      p_adaboost <- predict(amy_adaboost, test_data)
      p_adaboost
      p_adaboost$confusion
      
      library(vcd)
      kappa(amy_adaboost$confusion)
      
      
      
      m_adaboost <- boosting(default ~ ., data = credit)
      p_adaboost <- predict(m_adaboost, credit)
      head(p_adaboost$class)
      p_adaboost$confusion
      
      # create and evaluate an Adaboost.M1 model using 10-fold-CV
      set.seed(300)
      adaboost_cv <- boosting.cv(default ~ ., data = credit)
      adaboost_cv$confusion
      
      # calculate kappa
      library(vcd)
      Kappa(adaboost_cv$confusion)

      
      View(newvars)
      ## Random Forests ----
    rf <- randomForest(Mortality ~., data = newvars)
      
      
      
      # random forest with default settings
      # random forests - each tree is built on random sets of features that ensures each tree is unique. 
      library(randomForest)
      set.seed(300)
      rf <- randomForest(default ~ ., data = credit)
      # class is factor vector with classes of each row in training data
      # can input ntree, and mtry the number of features to select at each split
      rf
      # the error here is actually the out of bag error - not based on the training data but overall performance
      
      # calculate kappa on the out-of-bag estimate
      library(vcd)
      Kappa(rf$confusion[1:2,1:2])
      
      # ranger is a faster implementation of the random forest algorithm
      library(ranger)
      set.seed(300)
      m_ranger <- ranger(default ~ ., data = credit)
      m_ranger
      
      # calculate kappa
      Kappa(m_ranger$confusion.matrix)
      
      # create a GBM model with default parameters
      library(gbm)
      set.seed(300)
      # many new parameters. target is outcome, distribution is form of target, n trees, shrinkage is learning rate, interaction depth is depth of each tree, n.minobsinnode is minimum observations per node
      m_gbm <- gbm(default ~ ., data = credit_train)
      m_gbm
      
      # evaluate the simple GBM model - need to convert probabilities to binary
      p_gbm <- predict(m_gbm, credit_test, type = "response")
      p_gbm_c <- ifelse(p_gbm > 0.50, 1, 0)
      table(credit_test$default, p_gbm_c)
      
      # compute kappa 
      library(vcd)
      Kappa(table(credit_test$default, p_gbm_c))
      
      
      # compute kappa 
      library(vcd)
      Kappa(table(credit_test$default, p_gbm_c))
      
      
      
      
      # create a tuned gbm() model using caret
      # start by creating the tuning grid
      grid_gbm <- expand.grid(
        n.trees = c(100, 150, 200),
        interaction.depth = c(1, 2, 3),
        shrinkage = c(0.01, 0.1, 0.3),
        n.minobsinnode = 10
      )
      
      # define the experiment's parameters
      library(caret)
      ctrl <- trainControl(method = "cv", number = 10,
                           selectionFunction = "best")
      
      # run the caret experiment
      credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
      set.seed(300)
      m_gbm_c <- train(default ~ ., data = credit, method = "gbm",
                       trControl = ctrl, tuneGrid = grid_gbm,
                       metric = "Kappa",
                       verbose = FALSE)
      
      # see the results
      m_gbm_c
      
      
      
      
      ## XGBOOST ##
      credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
      library(Matrix)
      credit_matrix <- sparse.model.matrix(~ . -default, data = credit) # use all data except for default - our target
      
      sparse.model.matrix(~., data = newvars)
      
      # convert target to numeric
      
      newvars %>% 
        mutate(Mortality = as.numeric(Mortality)) %>% 
        mutate(across(where(is.factor), as.integer))
      train_data <- newvars[train_sample,] # 163 items long
      test_data <- newvars[-train_sample,] # 54 items long
      dim(train_data)
      dim(test_data)
      
      train_matrix <- sparse.model.matrix(Mortality ~ . -1, data = train_data)
      test_matrix <- sparse.model.matrix(Mortality ~ . -1, data = test_data)
      dim(train_matrix)
      dim(test_matrix)
      
      dtrain <- xgb.DMatrix(data = train_matrix, label = train_data$Mortality)
      dtest <- xgb.DMatrix(data = test_matrix, label = test_data$Mortality)
      

  
      
          amyloid_matrix <- sparse.model.matrix(~ . -Mortality, data = newvars)
          amyloid_matrix <- amyloid_matrix[-1,]
          set.seed(999)
          View(amyloid_matrix)
          train_data <- amyloid_matrix[train_sample,] # 163 items long
          test_data <- amyloid_matrix[-train_sample,] # 54 items long
          dim(train_data)
          dim(test_data)
          params.xgb <- list(objective   = "binary:logistic",
                             max_depth   = 6,
                             eta         = 0.3,
                             gamma       = 0,
                             colsample_bytree = 1,
                             min_child_weight = 1,
                             subsample = 1)
          
          set.seed(555)
          train_labels <- newvars$Mortality
          xgb_amyloid <- xgboost(params  = params.xgb,
                                data    = train_data,
                                label   = dtrain, 
                                nrounds = 100,
                                print_every_n = 10,
                                verbose = 1)
          # make predictions
          prob_death <- predict(xgb_amyloid, test_data)
          prob_death
          pred_death <- ifelse(prob_default > 0.50, 1, 0)
          # create a confusion matrix
          table(pred_death, test_labels)
          # compute kappa
          library(vcd)
          Kappa(table(pred_default, credit_test_labels))
          
          
            
      # examine the sparse credit_matrix
      dim(credit_matrix)
      print(credit_matrix[1:5, 1:15])
      
      # remove the intercept
      credit_matrix <- credit_matrix[, -1] 
      
      # split the matrix into train and test
      set.seed(12345)
      train_ids <- sample(1000, 900)
      credit_train <- credit_matrix[train_ids, ]
      credit_test <- credit_matrix[-train_ids, ]
      
      # check that the rows are 900 vs. 100 and the cols are 35 vs. 35
      dim(credit_train)
      dim(credit_test)
      
      
      # create 1/0 vectors for train and test data indicating loan default
      credit_train_labels <-
        ifelse(credit[train_ids, c("default")] == "yes", 1, 0)
      credit_test_labels <-
        ifelse(credit[-train_ids, c("default")] == "yes", 1, 0)
      
      # build the xgboost model
      library(xgboost)
      
      # set XGB hyperparameters
      # these are defaults
      params.xgb <- list(objective   = "binary:logistic",
                         max_depth   = 6,
                         eta         = 0.3,
                         gamma       = 0,
                         colsample_bytree = 1,
                         min_child_weight = 1,
                         subsample = 1)
      
      set.seed(555)
      xgb_credit <- xgboost(params  = params.xgb,
                            data    = credit_train,
                            label   = credit_train_labels, 
                            nrounds = 100,
                            print_every_n = 10,
                            verbose = 1)
      
      # make predictions
      prob_default <- predict(xgb_credit, credit_test)
      prob_default
      pred_default <- ifelse(prob_default > 0.50, 1, 0)
      
      # create a confusion matrix
      table(pred_default, credit_test_labels)
      
      # compute kappa
      library(vcd)
      Kappa(table(pred_default, credit_test_labels))
      
      # create a tuned xgboost() model using caret
      # start by creating the tuning grid using caret comprising a variety of options for each hyperparameter
      grid_xgb <- expand.grid(
        eta = c(0.3, 0.4),
        max_depth = c(1, 2, 3),
        colsample_bytree = c(0.6, 0.8),
        subsample = c(0.50, 0.75, 1.00),
        nrounds = c(50, 100, 150),
        gamma = c(0, 1),
        min_child_weight = 1
      ) # this grid has 2x3x2x3x3x2 = 216 options. we do this with 10-fold CV
      
      # define the control object
      library(caret)
      ctrl <- trainControl(method = "cv", number = 10,
                           selectionFunction = "best")
      
      # run the caret experiment
      set.seed(300)
      m_xgb <- train(default ~ ., data = credit, method = "xgbTree",
                     trControl = ctrl, tuneGrid = grid_xgb,
                     metric = "Kappa", verbosity = 0)
      
      # see the results of all models (not shown in book due to size of output)
      m_xgb
      
      # see the hyperparameters for the best performing model
      m_xgb$bestTune
      
      # get the best kappa out of the 216 models tested
      max(m_xgb$results["Kappa"])
      
      
      
      
  # ripper package
  library(RWeka)
  amyloidmod <- JRip(as.factor(VT_VF_WS) ~ Age + EF + NSVT.1 +
                       VE.. + NYHA + Runs.per.monitoring.period + Mayo.stage,
                     data = train_data)
  amyloidmod
  
  
  
  
  
  
  # Cross fold validation
  
  
# Neural nets
library(neuralnet)
  str(newvars)
# all data needs to be numeric
  str(newvars)
set.seed(12345) # to guarantee repeatable results
normalize <- function(x) { 
    return((x - min(x)) / (max(x) - min(x)))
  }

NNdata <- newvars %>% 
  select(c("Age", "EF", "IVST", "PWT", "LAVI",
           "Hs.cTnT", "NYHA", "Mayo.stage", "Runs.per.monitoring.period",
           "Longest", "VE..", "Mortality"))
View(NNdata)

NNdata_normalized <- as.data.frame(lapply(NNdata[,-12], normalize))
NNdata_normalized$Mortality <- newvars$Mortality  

dim(NNdata_normalized)

# set pseudorandom number generator seed
set.seed(900)
train_sample <- sample(217,163)
# test and train datsets
train_data_NN <- NNdata_normalized[train_sample,] # 163 items long
test_data_NN <- NNdata_normalized[-train_sample,] # 54 items long

View(test_data_NN)

amy_nn <- neuralnet(
  Mortality ~., 
  data = train_data_NN,
  hidden = c(5,2),
  act.fct = "logistic",
  linear.output = FALSE
)
plot(amy_nn)

predictions <- compute(amy_nn, test_data_NN[,-12])
predicted_probabilities <- predictions$net.result
predicted_classes <- ifelse(predicted_probabilities >=0.5, 1, 0)
predicted_classes



confusion_matrix <- table(Predicted = predicted_classes[,2], Actual = test_data_NN$Mortality)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))








# Neural nets - trialing to do it with factor variables
library(neuralnet)
set.seed(12345)
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
str(newvars)

NNdata <- newvars %>% 
  mutate(
    sex_male = ifelse(Sex == "M", 1, 0),
    sex_female = ifelse(Sex == "F", 1, 0)     
         ) %>% 
  select (-Sex) %>% 
  mutate(
    across(where(is.factor), ~ as.numeric(as.character(.)))
    ) %>%
  select(c("Age", "EF", "IVST", "PWT", "LAVI",
           "Hs.cTnT", "NYHA", "Mayo.stage", "Runs.per.monitoring.period",
           "Longest", "VE..", "Mortality", "sex_male", "sex_female",
           "BB", "CCB", "AA", "Syncope", "NSVT.1", "Atrial.fibrillation",
           "VT_VF_WS")) %>% 
  mutate(across(as.numeric()))

NNdata_normalized <- as.data.frame(lapply(NNdata[,-12], normalize))
NNdata_normalized$Mortality <- newvars$Mortality  


# set pseudorandom number generator seed
set.seed(900)
train_sample <- sample(217,163)
# test and train datsets
train_data_NN <- NNdata_normalized[train_sample,] # 163 items long
test_data_NN <- NNdata_normalized[-train_sample,] # 54 items long

View(test_data_NN)
dim(train_data_NN)
dim(test_data_NN)
set.seed(900)
amy_nn <- neuralnet(
  Mortality ~., 
  data = train_data_NN,
  hidden = c(20,10,5,2),
  #hidden = c(5),
  act.fct = "logistic",
  linear.output = FALSE
)
plot(amy_nn)

predictions <- compute(amy_nn, test_data_NN[,-21])
predicted_probabilities <- predictions$net.result

predicted_classes <- ifelse(predicted_probabilities >=0.5, 1, 0)


confusion_matrix <- table(Predicted = predicted_classes[,2], Actual = test_data_NN$Mortality)
confusion_matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))



# Cross validation
# Forest treee plot, and gradient boost
# random numbers
set.seed(900)
train_sample <- sample(217,163)
train_sample
length(train_sample)

# test and train datsets and make model
train_data <- newvars[train_sample,] # 163 items long
test_data <- newvars[-train_sample,] # 54 items long
freq(train_data$Mortality)
freq(test_data$Mortality)
amyloidmod <- C5.0(as.factor(Mortality) ~ Age + EF + NSVT.1 + IVST + PWT + LAVI + Tafamidis.baseline + LGE.extensive +
                     VE.. + NYHA + Runs.per.monitoring.period + Mayo.stage + Syncope + Longest + VT_VF_WS,
                   data = train_data)

summary(amyloidmod)
x11()
plot(amyloidmod)

amyloidpredict <- predict(object = amyloidmod, newdata = test_data)  # makes a vector with probabilities
amyloidpredict
# to cross tabulate predicted vs actual
CrossTable(test_data$Mortality, amyloidpredict, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual mortality', 'predicted mortality'))  
# 18 incorrect



# Cross validation






amyloidmod_10x <- C5.0(as.factor(Mortality) ~., data = train_data,trials = 10)
amyloidmod_10x
summary(amyloidmod_10x)
x11()
plot(amyloidmod_10x)

amyloidpredict_10x <- predict(object = amyloidmod_10x, newdata = test_data)  # makes a vector with probabilities
amyloidpredict_10x
# to cross tabulate predicted vs actual
CrossTable(test_data$Mortality, amyloidpredict_10x, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual mortality', 'predicted mortality'))  
# now 14 incorrect however all are not predicted to have mortality. only 2/14 correct for actual mortality





#####
## old models, published
  # Need NSVT runs on any holter
  # CMR variables

# Univariate analysis
# Age
agemod <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$Age)
summary(agemod)
# AF
af <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$Atrial.fibrillation)
summary(af)
nyha34 <- ansvt$NYHA
for (i in 1:217) {
  if (is.na(nyha34[i])) {
    nyha34[i] <- 1
  }
}
for (i in 1:217) {
  if (nyha34[i] >= 3) {
    nyha34[i] <- 1
  } else {
    nyha34[i] <- 0
  }
}
nyha <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ nyha34)
summary(nyha)
# NSVT
presnsvt <- nsvtrunsperhour <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$NSVT)
summary(presnsvt)
# Runs per monitoring period
run24h <- nsvtrunsperhour <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$Runs.per.monitoring.period)
summary(run24h)
# Longest run
longestrun <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$Longest)
summary(longestrun)
# Ventricular ectopy
ve <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$VE..)
summary(ve)
# EF continuous
nsvtef <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$EF)
summary(nsvtef)
# EF < 35%
ef35 <- ansvt$EF
output <- rep(0,217)
for (i in 1:217) {
  if (ef35[i] < 35) {
    output[i] <- 1
  } else {
    output[i] <- 0
  }
}
nsvtef35 <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ output)
summary(nsvtef35)
# septal wall thickness
nsvtseptum <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$IVST)
summary(nsvtseptum)
# Posterior wall thickness
nsvtpwt <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$PWT)
summary(nsvtpwt)
# LAVI
nsvtlavi <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$LAVI)
summary(nsvtlavi)
# Mayo stage
ms <- ansvt$Mayo.stage
for (i in 1:217) {
  if (is.na(ms[i])) {
    ms[i] <- 1
  }
}
mayostage  <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$Mayo.stage)
summary(mayostage)
# HS-TNT
hstnt <- ansvt$Hs.cTnT
output <- rep(0,217)
for (i in 1:217) {
  if (is.na(hstnt[i])) {
    hstnt[i] <- 0
  }
}
for (i in 1:217) {
  if (hstnt[i] > 0.05) {
    output[i] <- 1
  } else {
    output[i] <- 0
  }
}
hscutoff <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ output)
summary(hscutoff)
# BNP
bnp <- ansvt$NT.proBNP
output <- rep(0,217)
for (i in 1:217) {
  if (is.na(bnp[i])) {
    bnp[i] <- 0
  }
}
for (i in 1:217) {
  if (bnp[i] > 3000) {
    output[i] <- 1
  } else {
    output[i] <- 0
  }
}
bnp3000 <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ output)
summary(bnp3000)
# Beta blocker
nsvtbb <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$BB)
summary(nsvtbb)
# CCB
nsvtccb <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$CCB)
summary(nsvtccb)
# Tafamadis at baseline - has alot of NAs
tafbaseline <- ansvt$Tafamidis.baseline 
for(i in 1:217) {
  if (is.na(tafbaseline[i])) {
    tafbaseline[i] <- 0
  } 
}
nsvttafamadis <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ tafbaseline)
summary(nsvttafamadis)

# CMR stuff
lge <- subset(ansvt,subset = ansvt$Cardiac.MR.obtained.1...yes..0...no == 1)
subendo <- ifelse(lge$Subendo.LGE == 1 & lge$Midwall.LGE == 0 & lge$Subepi.LGE == 0,1,0)
lgeendoonly <- coxph(Surv(time = lge[,61], event = lge[,54]) ~ subendo)
summary(lgeendoonly)
transmural <- rep(0,93) 
transmural <- ifelse(lge$Subendo.LGE == 1 & lge$Midwall.LGE == 1 & lge$Subepi.LGE == 1,1,0)
lgetransmural <- coxph(Surv(time = lge[,61], event = lge[,54]) ~ transmural)
summary(lgetransmural)
extensivelge <- coxph(Surv(time = lge[,61], event = lge[,54]) ~ lge$LGE2nd)
summary(extensivelge)

# Multivariate model
# For NSVT
finmod <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$Age + 
                  ansvt$NSVT + nyha34)
summary(finmod) 
# For VE and not NSVT
finmod <- coxph(Surv(time = ansvt[,61], event = ansvt[,54]) ~ ansvt$Age + 
                  ansvt$VE.. + nyha34)
summary(finmod) 




