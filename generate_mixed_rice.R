raw.data <- read.csv("D:/統計學習/2024SL_train_data.csv")
str(raw.data[, 2:8])
summary(raw.data[, 2:8])

##Accuracy
cfmaccuracy <- function(matrix){
  
  accuracy <- sum(diag(matrix)) / sum(matrix)
  return(cat("準確率：", accuracy, "\n"))
  
}

#資料清理
vietnam <- raw.data[which(raw.data$Country == "Vietnam"), ] #抽取出rawdata中的越南樣本
v <- cbind(vietnam[,2:7], ratio_t = 0, ratio_v = 1) 

taiwan <- raw.data[which(raw.data$Country == "Taiwan"), ] # 抽取出rawdata中的臺灣樣本
t <- cbind(taiwan[,2:7], ratio_t = 1, ratio_v = 0)

### 創建混米生成的函式
mixedsample <- function(data, times){
  mixed_data <- data.frame() # 創建空的 dataframe 儲存mixeddata
  for (i in 1:times) {
    vietnam_sample <- vietnam[sample(1:70, 1), ] #隨機抽取 rawdata 中的越南米
    taiwan_sample <- taiwan[sample(1:80, 1), ]   #隨機抽取 rawdata 中的臺灣米
    
    ratio_t <- sample(1:9, 1)/10 #隨機產生混米中臺灣米的比例
    ratio_v <- 1-ratio_t #越南米的比例
    
    vietnam_values <- as.numeric(vietnam_sample[, 2:6]) 
    taiwan_values <- as.numeric(taiwan_sample[, 2:6])
    
    #計算混米比例
    mixed_values <- round(vietnam_values * ratio_v + taiwan_values * ratio_t, 3) 
    
    mixed_sample <- data.frame(t(mixed_values))
    
    #將混米的國家標記為mixed並且將比例的cbind上去
    mixed_sample <- cbind(mixed_sample, Country = "Mixed", ratio_t, ratio_v)
    #把純米跟混米資料結合成同一個資料框
    mixed_data <- rbind(mixed_data, mixed_sample)
  }
  return(mixed_data)
}

m <- mixedsample(raw.data, 150) #生成150筆混米資料
data.clean <- rbind(t, v, m) 
rownames(data.clean) <- NULL

train_index <- sample(1:nrow(data.clean), 0.8 * nrow(data.clean)) # 隨機抽建立訓練集編號

train_data <- data.clean[train_index, ]
test_data <- data.clean[-train_index, ]
train_data$Country <- as.factor(train_data$Country)
test_data$Country <- as.factor(test_data$Country)

#############
#### LDA ####
#############

library(caret)
library(MASS)

#設定cv的控制參數
cv_control <- trainControl(method = "cv", number = 10) # 10-fold

lda_model <- train(Country ~ X1 + X2 + X3 + X4 + X5, 
                      data = train_data, 
                      method = "lda", 
                      trControl = cv_control)

##訓練集
LDA_fit <- predict(lda_model, newdata = train_data)
lda_cm <- table(predict = LDA_fit, actual = train_data$Country)
cfmaccuracy(lda_cm)

##測試測試集
LDA_fit <- predict(lda_model, newdata = test_data)
lda_cm <- table(predict = LDA_fit, actual = test_data$Country)
cfmaccuracy(lda_cm)

#############
#### PLS ####
#############

library(pls)
train_response <- model.matrix(~ Country - 1, data = train_data)
test_response <- model.matrix(~ Country - 1, data = test_data)

# 執行 PLS 分析
pls_model <- plsr(train_response ~ X1 + X2 + X3 + X4 + X5, 
                  data = train_data, scale = TRUE, validation = "CV", 
                  segments = 10)

summary(pls_model)

validationplot(pls_model, val.type = "MSEP")

pls_accuracy <- function(model,data){
  result.a <- c()
  for (ncomp in 1:5) {
  pls_fit <- predict(model, newdata = data, ncomp = ncomp)
  # 轉換為分類結果
  # 找到每行的最大值索引 (對應於 Country 類別)
  pls_class <- apply(pls_fit, 1, which.max)
  
  # 將索引轉換為原始類別標籤
  levels <- colnames(train_response)  # 原始標籤名稱
  pls_class <- factor(levels[pls_class], levels = levels)

  # 建立混淆矩陣並計算準確率
  pls_cm <- table(Predicted = pls_class, Actual = data$Country)
  accuracy <- round(sum(diag(pls_cm)) / sum(pls_cm),3)
  result.a[ncomp] <- accuracy
  }
  best.ncomp <- which(result.a == max(result.a))
  best.acc <- max(result.a)
  return(cat("最佳變數數量：", best.ncomp,"\n","準確率：", best.acc))
} 

pls_accuracy(pls_model,train_data)

pls_accuracy(pls_model,test_data)

##########################
#### Ridge Regression ####
##########################

library(glmnet)

train_X <- as.matrix(train_data[, c("X1", "X2", "X3", "X4", "X5")])
test_X <- as.matrix(test_data[, c("X1", "X2", "X3", "X4", "X5")])
train_Y <- as.factor(train_data$Country)
test_Y <- as.factor(test_data$Country)

# 使用 glmnet 進行 Ridge Regression
ridge_model <- cv.glmnet(train_X, train_Y, 
                         alpha = 0,               # alpha = 0 表示 Ridge Regression
                         family = "multinomial",  # 多分類問題
                         nfolds = 10)             # 10-fold Cross-Validation

# 顯示 交叉驗證誤差最小 的 lambda 值
optimal_lambda <- ridge_model$lambda.min
plot(ridge_model)
cat("最佳的 Lambda 值：", optimal_lambda, "\n")

### 訓練集 ###
ridge_fit <- predict(ridge_model, newx = train_X, s = optimal_lambda, 
                     type = "class") 
ridge_cm <- table(Predicted = ridge_fit, Actual = train_data$Country)  # 建立混淆矩陣並計算準確率
cfmaccuracy(ridge_cm)

### 預測測試集 ###
ridge_fit <- predict(ridge_model, newx = test_X, s = optimal_lambda, 
                        type = "class")
ridge_cm <- table(Predicted = ridge_fit, Actual = test_data$Country)
cfmaccuracy(ridge_cm)

###############
#### LASSO ####
###############

# 使用 glmnet 進行 LASSO Regression
lasso_model <- cv.glmnet(train_X, train_Y, 
                         alpha = 1,               # alpha = 1 表示 LASSO
                         family = "multinomial",  # 適用於多分類問題
                         nfolds = 10)             # 10-fold Cross-Validation

# optimal lambda 
optimal_lambda <- lasso_model$lambda.min
plot(lasso_model)
cat("最佳的 Lambda 值：", optimal_lambda, "\n")

### 訓練集
lasso_fit <- predict(lasso_model, newx = train_X, s = optimal_lambda, 
                     type = "class")
lasso_cm <- table(Predicted = lasso_fit, Actual = train_data$Country)
cfmaccuracy(lasso_cm)

### 預測測試集
lasso_fit <- predict(lasso_model, newx = test_X, s = optimal_lambda, 
                        type = "class")
lasso_cm <- table(Predicted = lasso_fit, Actual = test_data$Country)
cfmaccuracy(lasso_cm)

#######################
#### Random Forest ####
#######################
library(randomForest)

rf_model <- randomForest(as.factor(Country) ~ X1 + X2 + X3 + X4 + X5, 
                         data = train_data,
                         ntree = 500,        # 樹的數量
                         mtry = sqrt(5),     # 每次分割時的特徵數
                         importance = TRUE)  # 計算特徵重要性

plot(rf_model)

rf_fit <- predict(rf_model, newdata = train_data)
RF_cm <- table(Predicted = rf_fit, Predicted = train_data$Country)
print(RF_cm)
cfmaccuracy(RF_cm)

rf_fit <- predict(rf_model, newdata = test_data)
RF_cm <- table(Predicted = rf_fit, Predicted = test_data$Country)
print(RF_cm)
cfmaccuracy(RF_cm)


#############
#### SVM ####
#############

library(e1071)

# 查看最佳參數
best.cost <- function(kernel){
  tune_result <- tune(svm, Country ~  X1 + X2 + X3 + X4 + X5, 
  data = train_data, 
  ranges = list(cost = c(0.001 , 0.01, 0.1, 1,5,10,100)),
  kernel = kernel
)
  return(tune_result$best.parameters)
}


# 訓練 SVM 模型
### linear ###
svm_model_l <- svm(Country ~  X1 + X2 + X3 + X4 + X5, 
                 data = train_data, 
                 kernel = "linear",                    # 核函數選擇：linear, radial, polynomial, sigmoid
                 cost = best.cost("linear"),           # 正則化參數
                 scale = TRUE)                         # 標準化特徵

### radial ###
svm_model_r <- svm(Country ~  X1 + X2 + X3 + X4 + X5, 
                   data = train_data, 
                   kernel = "radial",  
                   cost = best.cost("radial"),           
                   scale = TRUE)
### polynomial ###
svm_model_p <- svm(Country ~  X1 + X2 + X3 + X4 + X5, 
                   data = train_data, 
                   kernel = "polynomial",  
                   cost = best.cost("polynomial"),           
                   scale = TRUE)

### sigmoid ###
svm_model_s <- svm(Country ~  X1 + X2 + X3 + X4 + X5, 
                   data = train_data, 
                   kernel = "sigmoid",  
                   cost = best.cost("sigmoid"),           
                   scale = TRUE)

# 在測試集上進行預測
svm_fit_l <- predict(svm_model_l, newdata = test_data)
svm_fit_r <- predict(svm_model_r, newdata = test_data)
svm_fit_p <- predict(svm_model_p, newdata = test_data)
svm_fit_s <- predict(svm_model_s, newdata = test_data)

svm_cm_l <- table(Predicted = svm_fit_l, Actual = test_data$Country)
svm_cm_r <- table(Predicted = svm_fit_r, Actual = test_data$Country)
svm_cm_p <- table(Predicted = svm_fit_p, Actual = test_data$Country)
svm_cm_s <- table(Predicted = svm_fit_s, Actual = test_data$Country)

cfmaccuracy(svm_cm_l)
cfmaccuracy(svm_cm_r)
cfmaccuracy(svm_cm_p)
cfmaccuracy(svm_cm_s)

#########################################################################################
#########################################################################################
#########################################################################################

#######################
####混米比例估計模型####
#######################

m2 <- mixedsample(raw.data, 1000)
train_index <- sample(1:nrow(m2), 0.8 * nrow(m2)) # 隨機抽建立訓練集編號
train_data2 <- m2[train_index, ]
test_data2 <- m2[-train_index, ]

#### Linear Models

lm_model_t <- lm(ratio_t ~ X1 + X2 + X3 + X4 + X5, data = train_data2)
lm_model_v <- lm(ratio_v ~ X1 + X2 + X3 + X4 + X5, data = train_data2)

ratio_t_pred1 <- predict(lm_model_t, newdata = train_data2)
ratio_v_pred1 <- predict(lm_model_v, newdata = train_data2)
mean((ratio_t_pred1 - train_data2$ratio_t)^2)
mean((ratio_v_pred1 - train_data2$ratio_v)^2)

ratio_t_pred2 <- predict(lm_model_t, newdata = test_data2)
ratio_v_pred2 <- predict(lm_model_v, newdata = test_data2)
mean((ratio_t_pred2 - test_data2$ratio_t)^2)
mean((ratio_v_pred2 - test_data2$ratio_v)^2)

ratio_t_pred_with_ci <- predict(lm_model_t, newdata = test_data2, 
                                interval = "confidence", level = 0.95)
ratio_v_pred_with_ci <- predict(lm_model_v, newdata = test_data2, 
                                interval = "confidence", level = 0.95)

##結合鑑別和混米比例估計模型
MRPM <- function(newdata){
  svm_p <- predict(svm_model_p, newdata = newdata)
  input <- newdata[which(svm_p == "Mixed"),]

  t <- predict(lm_model_t, newdata = input, 
             interval = "confidence", level = 0.95)
  t <- round(t,3)

  v <- predict(lm_model_v, newdata = input, 
             interval = "confidence", level = 0.95)
  v <- round(v,3)
  row.name <- as.numeric(row.names(input))
  
  for (n in 1:nrow(input)) {
    cat("第",row.name[n],"筆資料是混米","\n",
        "台灣米比例為:",t[n,1]," 95% 信賴區間為:",t[n,2:3],"\n",
        "越南米比例為:",v[n,1]," 95% 信賴區間為:",v[n,2:3],"\n","\n")
  }
  return(cat("有",nrow(input),"筆資料是混米"))
}

##結合鑑別和混米比例估計模型並輸出表格
MRPM <- function(newdata){
  pt <- data.frame(Actual = rep(NA,nrow(newdata)),
                   Predict = rep(NA,nrow(newdata)),
                   Ratio = rep(NA,nrow(newdata)),
                   CI = rep(NA,nrow(newdata)))
  
  svm_p <- predict(svm_model_p, newdata = newdata)
  row.name <- as.numeric(row.names(newdata))
  mix <- c()
  
  v <- predict(lm_model_v, newdata = newdata, 
               interval = "confidence", level = 0.95)
  v <- round(v,3)
  
  for (n in 1:nrow(newdata)) {
    if (svm_p[n] == "Taiwan"){
      pt[n,] <- c(newdata$Country[n],"Taiwan",NA,NA)
      
    }else if (svm_p[n] == "Vietnam"){
      pt[n,] <- c(newdata$Country[n],"Vietnam",NA,NA)
    }else {
      mix <- rbind(mix,n)
      pt[n,] <- c(Actual = newdata$Country[n], Predict = "Mix",
              Ratio = v[n,1], CI = paste("[",v[n,2],",",v[n,3],"]"))
    }
  }
  return(pt)
}

result <- MRPM(dat)
View(result)
write.csv(result,".csv")

 
