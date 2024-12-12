raw.data <- raw.data

str(raw.data[, 2:8])
summary(raw.data[, 2:8])

colSums(is.na(raw.data))

### 混米生成 


vietnam <- subset(raw.data, Country == "Vietnam")
taiwan <- subset(raw.data, Country == "Taiwan")


mixedsample <- function(data,times){
  mixed_data <- data.frame()
  for (i in 1:times) {
    vietnam_sample <- vietnam[sample(1:70, 1), ]
    taiwan_sample <- taiwan[sample(1:80, 1), ]
    
    ratio_t <- sample(1:9, 1)/10
    ratio_v <- 1-ratio_t
    
    vietnam_values <- as.numeric(vietnam_sample[, 2:6])
    taiwan_values <- as.numeric(taiwan_sample[, 2:6])
    
    mixed_values <- vietnam_values * ratio_v + taiwan_values * ratio_t 
    
    mixed_sample <- data.frame(t(mixed_values))
    mixed_sample <- cbind(mixed_sample,ratio_t,ratio_v)
    mixed_data <- rbind(mixed_data, mixed_sample)
  }
  return(mixed_data)
}
