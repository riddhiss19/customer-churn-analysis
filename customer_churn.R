#Data Preprocessing
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(reshape2)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(knitr)
library(magrittr)
library(partykit)
library(rpart.plot)



Cust_Insights <- read.csv('dataset.csv')
str(Cust_Insights)

#missing values
sapply(Cust_Insights, function(x) sum(is.na(x)))
#remove 11 missing records
Cust_Insights <- Cust_Insights[complete.cases(Cust_Insights), ]

#Recode columns
cols_recode1 <- c(10:15)
for(i in 1:ncol(Cust_Insights[,cols_recode1])) {
  Cust_Insights[,cols_recode1][,i] <- as.factor(mapvalues
                                                (Cust_Insights[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

Cust_Insights$MultipleLines <- as.factor(mapvalues(Cust_Insights$MultipleLines, 
                                                   from=c("No phone service"),
                                                   to=c("No")))

min(Cust_Insights$tenure); max(Cust_Insights$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
Cust_Insights$tenure_group <- sapply(Cust_Insights$tenure,group_tenure)
Cust_Insights$tenure_group <- as.factor(Cust_Insights$tenure_group)

Cust_Insights$SeniorCitizen <- as.factor(mapvalues(Cust_Insights$SeniorCitizen,
                                                   from=c("0","1"),
                                                   to=c("No", "Yes")))

#Remove columns we don't need
Cust_Insights$customerID <- NULL
Cust_Insights$tenure <- NULL

Cust_Insights$Churn <- as.factor(mapvalues(Cust_Insights$Churn,
                                           from=c("No", "Yes"),
                                           to=c("0","1")))

# Identify numerical variables in the "Cust_Insights" dataset
numeric.var <- sapply(Cust_Insights, is.numeric)

# Calculate correlation matrix
corr.matrix <- cor(Cust_Insights[,numeric.var])

# Create heatmap of correlation matrix
col <- colorRampPalette(c("lightblue", "white", "skyblue"))(100)
heatmap_data <- melt(corr.matrix)
ggplot(data = heatmap_data, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradientn(colours = col) +
  theme_minimal() +
  labs(title = "\n\nCorrelation Heatmap for Numerical Variables")
print(heatmap_data)


#They are correlated so we will remove one, Total Charges
Cust_Insights$TotalCharges <- NULL

my_colors <- c("#FF69B4", "#FFA07A", "#00CED1", "#7B68EE", "#FFD700")
#Bar plots of categorical variables
p1 <- ggplot(Cust_Insights, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill = "#69b3a2") +
  ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(Cust_Insights, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill = "#404080") +
  ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(Cust_Insights, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill = "#ffcc99") +
  ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(Cust_Insights, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill = "#b3cde0") +
  ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)



p5 <- ggplot(Cust_Insights, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill=my_colors[1]) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(Cust_Insights, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill=my_colors[2]) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(Cust_Insights, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill=my_colors[3]) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(Cust_Insights, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill=my_colors[4]) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)



p9 <- ggplot(Cust_Insights, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill = my_colors[1]) + ylab("Percentage") + coord_flip() + theme_minimal()

p10 <- ggplot(Cust_Insights, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill = my_colors[2]) + ylab("Percentage") + coord_flip() + theme_minimal()

p11 <- ggplot(Cust_Insights, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill = my_colors[3]) + ylab("Percentage") + coord_flip() + theme_minimal()

p12 <- ggplot(Cust_Insights, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill = my_colors[4]) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(p9, p10, p11, p12, ncol=2)




p13 <- ggplot(Cust_Insights, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill=my_colors[1]) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(Cust_Insights, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill=my_colors[2]) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(Cust_Insights, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill=my_colors[3]) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(Cust_Insights, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill=my_colors[4]) + ylab("Percentage") + coord_flip() + theme_minimal()
p17 <- ggplot(Cust_Insights, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill="#69b3a2") + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2)




#Logistic Regression
#First, we split the data into training and testing sets:
intrain<- createDataPartition(Cust_Insights$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- Cust_Insights[intrain,]
testing<- Cust_Insights[-intrain,]
nrow(training)
nrow(testing)

#Fitting the Logistic Regression Model:
LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))

# Create a dataframe with the coefficients of the top 5 rows
top_coeffs <- summary(LogModel)$coefficients[2:6,]
print(top_coeffs)
#plotting Graph For Them
for(i in 1:nrow(top_coeffs)) {
  row_name <- rownames(top_coeffs)[i]
  coeff <- top_coeffs[i, ]
  
  # Create a data frame with the coefficient and its name
  df <- data.frame(name = names(coeff), value = coeff, row_name = row_name)
  
  # Create a bar plot
  p <- ggplot(df, aes(x = name, y = value)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = round(value, 3)), vjust = -0.5, size = 3.5) +
    ggtitle(paste0("Coefficients for ", row_name)) +
    xlab("") + ylab("Coefficient") +
    theme_bw()
  
  # Print the plot
  print(p)
}

#Feature Analysis
anova(LogModel, test="Chisq")

#Assessing the predictive ability of the Logistic Regression model
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

print("Confusion Matrix for Logistic Regression");
conf_matrix <-table(testing$Churn, fitted.results > 0.5)
# Create a data frame from the confusion matrix
conf_matrix_df <- data.frame(
  actual = factor(c(rep("False", 2), rep("True", 2)), levels = c("False", "True")),
  predicted = factor(c("False", "True", "False", "True"), levels = c("False", "True")),
  count = c(conf_matrix[1, 1], conf_matrix[1, 2], conf_matrix[2, 1], conf_matrix[2, 2])
)

# Create the plot
ggplot(conf_matrix_df, aes(x = actual, y = predicted)) +
  geom_tile(aes(fill = count), colour = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = count)) +
  labs(x = "Actual", y = "Predicted", fill = "Count")
conf_matrix

#Odds Ratio
library(MASS)
exp(cbind(OR=coef(LogModel), confint(LogModel)))




#Decision Tree
#To run decision tree part, wee need to re-run the data preprocessing part
tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, training)
plot(tree)


#Convert variables used in model to factors
training$Contract <- factor(training$Contract)
training$tenure_group <- factor(training$tenure_group)
training$PaperlessBilling <- factor(training$PaperlessBilling)

#confusion matric
pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)
pred_tree <- predict(tree, testing)
confusion_matrix <- confusionMatrix(data = pred_tree, reference = testing$Churn)
# Extract the confusion matrix values
confusion_matrix_df <- as.data.frame(confusion_matrix$table)
colnames(confusion_matrix_df) <- c("Predicted", "Actual", "Count")
# Create the plot
ggplot(confusion_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count)) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix for Decision Tree",
       x = "Actual",
       y = "Predicted")

#Decision Tree Accuracy
p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))



#Random Forest
#Initial Model
rfModel <- randomForest(Churn ~., data = training)
print(rfModel)

#Random Forest Prediction and Confusion Matrix
pred_rf <- predict(rfModel, testing)
caret::confusionMatrix(pred_rf, testing$Churn)

pred_rf <- predict(rfModel, testing)
conf_mat <- confusionMatrix(pred_rf, testing$Churn)

# Plot the confusion matrix
conf_mat_table <- conf_mat$table
conf_mat_table_df <- as.data.frame(conf_mat_table)
conf_mat_table_df$actual <- factor(conf_mat_table_df$Reference, levels = rev(levels(factor(testing$Churn))))
conf_mat_table_df$predicted <- factor(conf_mat_table_df$Prediction, levels = levels(factor(testing$Churn)))
#Random Forest Error Rate
ggplot(conf_mat_table_df, aes(x = predicted, y = actual, fill = Freq)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue", guide = guide_colorbar(title = "Count")) +
  geom_text(aes(label = Freq), color = "black") +
  labs(x = "Predicted", y = "Actual", title = "Confusion Matrix for Random Forest Model") +
  theme_bw()


 