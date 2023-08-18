# Customer Churn Analysis 

This project performs analysis on the Telco Customer Churn dataset to predict whether a customer will churn.

## Data

The data is from the Telco Customer Churn dataset available on Kaggle: https://www.kaggle.com/blastchar/telco-customer-churn

It contains information about customers of a telecom company and whether they churned or not. 

The columns are:

- `customerID` - Unique ID for each customer
- `gender` - Gender of customer 
- `SeniorCitizen` - Whether customer is a senior citizen (1, 0)
- `Partner` - Whether customer has a partner (Yes, No)
- `Dependents` - Whether customer has dependents (Yes, No)
- `tenure` - Number of months as a customer 
- `PhoneService` - Whether customer has phone service (Yes, No) 
- `MultipleLines` - Multiple phone lines (Yes, No, No phone service)
- `InternetService` - Customer's internet service provider (DSL, Fiber optic, No)
- `OnlineSecurity` - Online security service (Yes, No, No internet service)
- `OnlineBackup` - Online backup service (Yes, No, No internet service)
- `DeviceProtection` - Device protection service (Yes, No, No internet service)
- `TechSupport` - Technical support service (Yes, No, No internet service)  
- `StreamingTV` - Streaming TV service (Yes, No, No internet service)
- `StreamingMovies` - Streaming movies service (Yes, No, No internet service)
- `Contract` - Type of contract (Month-to-month, One year, Two year)
- `PaperlessBilling` - Whether customer has paperless billing (Yes, No) 
- `PaymentMethod` - Customer's payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic)) 
- `MonthlyCharges` - Amount charged each month 
- `TotalCharges` - Total amount charged 
- `Churn` - Whether customer churned (Yes or No)

## Models

The following models are evaluated:

- Logistic Regression 
- Decision Tree
- Random Forest

## Usage

To reproduce this analysis:

1. Import the Telco customer churn CSV file 
2. Split data into training and validation sets
3. Train models on training set 
4. Predict on validation set
5. Evaluate models using accuracy, confusion matrix, AUC-ROC curve
