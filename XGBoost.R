library(data.table)
library(caret)


PATH <- "C:\\Users\\shrad\\Desktop\\AML\\Kaggle\\"
dtrain <- fread(paste0(PATH,"train.csv"), sep=",", na.strings = "", stringsAsFactors=T)
dtest <- fread(paste0(PATH,"test.csv"), sep=",", na.strings = "", stringsAsFactors=T)


print("Training data size in RAM:");
print(object.size(dtrain), units = 'Mb')

print(dim(dtrain))

x_train <- dtrain[, 3:59]
y_train <- as.factor(dtrain$target)

trControl = trainControl(method = 'cv',
                         number = 3,
                         verboseIter = TRUE,
                         allowParallel = TRUE)

tuneGridXGB <- expand.grid(
  nrounds=c(300),
  max_depth = c(4, 6,8),
  eta = c(0.01),
  gamma = 0.1,
  colsample_bytree = c(0.4, 0.6, 0.8),
  subsample = c(0.5, 0.75),
  min_child_weight = c(20)
)
#Fitting nrounds = 300, max_depth = 4, eta = 0.01, gamma = 0.1, colsample_bytree = 0.4, 
#min_child_weight = 20, subsample = 0.5 on full training set



xgbmod <- train(x = x_train,
                y = y_train,
                method = 'xgbTree',
                trControl = trControl,
                tuneGrid = tuneGridXGB,
                feval = xgb_normalizedgini,
                objective="binary:logistic",
                n_estimators = 200)



preds <- predict(xgbmod, newdata = dtest, type = "prob")

sub <- data.frame(id = dtest$id, target = preds$`1`)

write.csv(sub, 'output.csv', row.names = FALSE)