#load library
library(data.table)
library(mice)
library(dplyr)

#load data
path <- "C:/Users/Usuario"

data <- fread(file.path(path, "data_capstone2.csv"), header=TRUE, na.strings=c("","NA"))
data$credit_limit[data$credit_limit== 0]<-0.001
data$credit_limit_log <- log(data$credit_limit)

# Mutate data
data <- mutate(data, 
     borrowed_in_months = as.factor(borrowed_in_months),
     facebook_profile = as.factor(facebook_profile),
     score_1 = as.factor(score_1),
     score_2 = as.factor(score_2),
     sign = as.factor(sign),
     gender = as.factor(gender),
     state = as.factor(state),
     real_state = as.factor(real_state),
     n_bankruptcies = as.factor(n_bankruptcies)
     )


drop.cols <- c('V1','ids', 'default','score_2','credit_limit', 'reason', 'last_payment', 'end_last_loan', 'state', 'zip','job_name')
data <- data %>% select(-one_of(drop.cols))
rm(drop.cols)

#Count missing data
md.pattern(data)

# Impute missing data using mice
init = mice(data, maxit=0) 
meth = init$method
predM = init$predictorMatrix

# It is time to run the multiple (m=5) imputation.
set.seed(103)
imputed = mice(data, method=meth, predictorMatrix=predM, m=5)

# Create a dataset after imputation
complete_imputed <- complete(imputed)
summary(complete_imputed)


#-------------------------------------------------------------------------------
# Plots

# Scatterplot credit_limit_log vs income
xyplot(imputed, credit_limit_log ~ income,pch=18,cex=1)

# Density plot original vs imputed dataset
densityplot(imputed)

# Another take on the density: stripplot()
stripplot(imputed, pch = 20, cex = 1.2)

#-------------------------------------------------------------------------------
write.csv(complete_imputed, file = "complete_imputed.csv", row.names = FALSE, quote = FALSE)
