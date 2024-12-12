raw.data <- read.csv("C:\\碩班\\Statistical Learning\\Final Project\\2024SL_train_data.csv", 
                 header = T)

raw.data <- raw.data

str(raw.data[, 2:8])
summary(raw.data[, 2:8])

colSums(is.na(raw.data))

#資料清理
vietnam <- raw.data[which(raw.data$Country == "Vietnam"),]
v <- cbind(vietnam[,2:7],ratio_t=0,ratio_v=1)
taiwan <- raw.data[which(raw.data$Country == "Taiwan"),]
t <- cbind(taiwan[,2:7],ratio_t=1,ratio_v=0)

### 混米生成 
mixedsample <- function(data,times){
  mixed_data <- data.frame()
  for (i in 1:times) {
    vietnam_sample <- vietnam[sample(1:70, 1), ]
    taiwan_sample <- taiwan[sample(1:80, 1), ]
    
    ratio_t <- sample(1:9, 1)/10
    ratio_v <- 1-ratio_t
    
    vietnam_values <- as.numeric(vietnam_sample[, 2:6])
    taiwan_values <- as.numeric(taiwan_sample[, 2:6])
    
    mixed_values <- round(vietnam_values * ratio_v + taiwan_values * ratio_t,3) 
    
    mixed_sample <- data.frame(t(mixed_values))
    mixed_sample <- cbind(mixed_sample,Country="Mixed", ratio_t, ratio_v)
    mixed_data <- rbind(mixed_data, mixed_sample)
  }
  return(mixed_data)
}

m <- mixedsample(raw.data, 150)
data.clean <- rbind(t, v, m)
rownames(data.clean) <- NULL

###LDA
library(MASS)
train_index <- sample(1:nrow(data.clean), 0.8 * nrow(data.clean))
train_data <- data.clean[train_index, ]
test_data <- data.clean[-train_index, ]
lda_model <- lda(Country ~ X1+X2+X3+X4+X5, data = train_data)

# 查看模型摘要
print(lda_model)
#training set
LDA_fit <- predict(lda_model)
table(predict=LDA_fit$class,actual=train_data$Country)
mean(LDA_fit$class==train_data$Country)

#testing set
LDA_tfit <- predict(lda_model, newdata = test_data)
table(predict = LDA_tfit$class, actual = test_data$Country)
mean(LDA_tfit$class == test_data$Country)

# PLS

install.packages("pls")
library(pls)

train_index <- sample(1:nrow(data.clean), 0.8 * nrow(data.clean))
train_data <- data.clean[train_index, ]
test_data <- data.clean[-train_index, ]
train_response <- model.matrix(~ Country - 1, data = train_data)
test_response <- model.matrix(~ Country - 1, data = test_data)

# 執行 PLS 分析
pls_model <- plsr(train_response ~ X1 + X2 + X3 + X4 + X5, 
                  data = train_data, scale = TRUE, validation = "CV")

summary(pls_model)

validationplot(pls_model, val.type = "MSEP")

pls_fit <- predict(pls_model, train_data, ncomp = 2)
# 如果 pls_fit 是多維數組，提取第一維的預測值
pls_fit <- pls_fit[, , 1] # 假設我們只關注第一組成分的預測值
mean((pls_fit - train_response)^2)

#testing set
pls_tfit <- predict(pls_model, test_data, ncomp = 2)
pls_tfit <- pls_tfit[, , 1]  
mean((pls_tfit - test_response)^2)
