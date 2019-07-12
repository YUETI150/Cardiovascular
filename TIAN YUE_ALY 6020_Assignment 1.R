#1.	Create a user defined function in R taking two arguments to exponentiate arg1 to the arg2 power.
  deffunc <- function(x,y){
  x <- arg1
  y <- arg2
  exp <- x^(y^2)
  return(exp)
  }

#2.	Applied Predictive Modeling: Linear regression review
#a.	Load the data http://data.princeton.edu/wws509/datasets/salary.dat into R
  salarydata <- read.table("http://data.princeton.edu/wws509/datasets/salary.dat",header=TRUE)

#b.	Split data into train/test based on a 80/20 split (use seed: 123)
  smp_siz = floor(0.8*nrow(salarydata))  # creates a value for dividing the data into train and test. In this case the value is defined as 80% of the number of rows in the dataset
  set.seed(123)   # set seed to ensure you always have same random numbers generated
  train_rownum = sample(seq_len(nrow(salarydata)),size = smp_siz)  # Randomly identifies the rows equal to sample size ( defined in previous instruction) from  all the rows of salary dataset and stores the row number in train_rownum
  train =salarydata[train_rownum,] #creates the training dataset with row numbers stored in train_rownum
  test=salarydata[-train_rownum,]  # creates the test dataset excluding the row numbers mentioned in train_rownum
  train
  test
  
#c.	Fit model with train data to predict salary based on sex, rank, year, degree, and years since degree was awarded.
  lr <- lm(sl ~ sx + rk + yr + dg + yd, data = train)
  
#d.	Interpret model results
  summary(lr)
  
#e.	Does model meet linear regression assumptions?
# Because the Adjusted R-squared which equals 0.8651 is greater than 0.8, the lr model is quite fit the linear regression.

#f.	Use trained linear regression model to make predictions with test data
  pred <- predict(lr, mytest)
  pred
  
#g.	Assess prediction error/accuracy with appropriate metrics comparing predicted vs. observed values. Interpret accordingly.
  install.packages('Metrics')
  library(Metrics)
  mape(mytest$sl, pred) #computes the average absolute percent difference between two numeric vectors.
  mase(mytest$sl, pred) #computes the mean absolute scaled error between two numeric vectors. This function is only intended for time series data, where actual and numeric are numeric vectors ordered by time.
  rmse(mytest$sl, pred) #computes the root mean squared error between two numeric vector, The lower the RMSE, the better the model.
  mae(mytest$sl, pred) #computes the average absolute difference between two numeric vectors. MAE is less sensitive to outliers compared to RMSE.
 
#3.	Applied Predictive Analytics: Logistic Regression. 
#a.	Load binary dataset using the following code:
  sales <- read.csv("http://ucanalytics.com/blogs/wp-content/uploads/2017/09/Data-L-Reg-Gradient-Descent.csv")
  sales$X1plusX2 <- NULL
  var_names <- c("expenditure", "income", "purchase")
  names(sales) <- var_names
  #Standardize predictors (X1, X2) 
  sales$expenditure <- scale(sales$expenditure, scale=TRUE, center = TRUE)
  sales$income <- scale(sales$income, scale=TRUE, center = TRUE)
  
#b.	Separate sales data into train/test sets (75/25) using seed(5689).
  install.packages('caTools')
  library(caTools)
  set.seed(5689)   #  set seed to ensure you always have same random numbers generated
  sample = sample.split(sales,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
  sales.train =subset(sales,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
  stest=subset(sales, sample==FALSE)
  sales.train
  stest
  
#c.	Train logistic regression GLM model to predict Y from X1-X2.
  head=TRUE
  sales.glm <- glm(purchase ~ expenditure - income, family = binomial(link = "logit"), sales.train)
  summary(sales.glm) 
  
#d.	Test trained logistic regression model using test data (predict Ys using trained model, and convert probabilities to 0s/1s).
  sales.pros <- predict(sales.glm, stest,type='response') #Response gives the numerical result while class gives the label assigned to that value. Response lets to determine the threshold. For instance,
  sales.pros
  sales.pred = rep(0,length(sales.pros))
  sales.pred[sales.pros>.6] = 1 #In sales.probs we have some numerical values between 0 and 1. I set the threshold value equals to 0.6, then the result has two possible outcomes, 0 or 1.
  sales.pred
  
#e.	Evaluate by comparing mismatch in observed/predicted 0s/1s. How did you model perform?
  SST <- sum((stest$purchase - mean(stest$purchase))^2)
  SSE   <- sum((stest$purchase - sales.pros)^2)
  R2 <- 1 - SSE/SST #R-squared (R2), which is the proportion of variation in the outcome that is explained by the predictor variables. In multiple regression models, R2 corresponds to the squared correlation between the observed outcome values and the predicted values by the model. The Higher the R-squared, the better the model.
  R2
  
#4.	Predictive Analytics Theory & Applied: Gradient Descent and Generalized Linear Modeling via Maximum Likelihood Estimation
#a.	Load data. Use entire sales dataset from Question 3 above.
  sales <- read.csv("http://ucanalytics.com/blogs/wp-content/uploads/2017/09/Data-L-Reg-Gradient-Descent.csv")
  sales$X1plusX2 <- NULL
  var_names <- c("expenditure", "income", "purchase")
  names(sales) <- var_names
  
#b.	Logistic regression via user-defined maximum likelihood function. Create user-defined maximum likelihood function to estimate the parameters that maximize the log likelihood of the logistic density function. Use your logistic log likelihood function and the R optim() function to estimate the parameters (for B0, B1, and B2) that maximize the log likelihood given the observed sales data. You may use the “Logistic Regression Maximum Likelihood Code Outline.R” as a starting place. Just fill in the empty pieces denoted by brackets []. For a review of maximum likelihood estimation specific to logistic regression see: https://www.statlect.com/fundamentals-of-statistics/logistic-model-maximum-likelihood. The log likelihood equation should look something like: loglik <- sum(-y*log(1 + exp(-(x%*%beta))) - (1-y)*log(1 + exp(x%*%beta))). 
#data import/preparation
  sales$expenditure <- scale(sales$expenditure, scale=TRUE, center = TRUE)
  sales$income <- scale(sales$income, scale=TRUE, center = TRUE)
  X <- as.matrix(sales[,c(1,2)])
  X <- cbind(rep(1,nrow(X)),X)
  Y <- as.matrix(sales$purchase)
  
#Logistic Regression via Maximum Likelihood Estimation
  logl <- function(theta,x,y){
    y <- y
    x <- as.matrix(x)
    beta <- theta[1:ncol(x)]
    loglik <-  sum(-y*log(1 + exp(-(x%*%beta))) - (1-y)*log(1 + exp(x%*%beta))) 
      return(-loglik)}
  theta.start = rep(0,3)
  names(theta.start) = colnames(X)
  mle = optim(theta.start, logl, x = X ,y = Y ,hessian=T)
  out = list(beta=mle$par,se=diag(sqrt(solve(mle$hessian))),ll=2*mle$value)
  
#c.	Logistc regression via user-defined gradient descent function. Create a user-defined gradient descent function to estimate beta parameters (B0, B1, B2) using the sales data. You may use the “Logistic Regression Gradient Descent Code Outline” as a starting place. For a review of gradient descent specific to logistic regression see: http://ucanalytics.com/blogs/gradient-descent-logistic-regression-simplified-step-step-visual-guide/. The gradient descent cost function should look something like: (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g))). The gradient descent delta formula should look something like: (t(X) %*% error) / length(Y).
  X <- as.matrix(sales[,c(1,2)])
  X <- cbind(rep(1,nrow(X)),X)
  Y <- as.matrix(sales$purchase)
  sigmoid <- function(z){
    g <- 1/(1+exp(-z))
    return(g)
  }
  logistic_cost_function <- function(beta){
    m <- nrow(X)
    g <- sigmoid(X%*%beta)
    J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
    return(J)
  }
  logistic_gradient_descent <- function(alpha, iterations, beta, x, y){
    for (i in 1:iterations) {
      error <- (sigmoid(X%*%beta) - Y)
      delta <- (t(X) %*% error) / length(Y)
      beta <- (beta-alpha*delta)
    }
    return(list(parameters=beta))
  }
  initial_alpha <- 0.01
  num_iterations <- 10000
  empty_beta <- matrix(c(0,0,0), nrow=3)
  output <- logistic_gradient_descent(0.01 ,10000 , empty_beta  , x = x , y = y)
  
#d.	Fit a generalized linear model logistic regression model using R’s built-in glm() function using the full sales dataset.
  
  
#e.	Summarize the parameters/weights (B0, B1, B2) from each of these three optimization approaches (parts b, c, and d). How do the results from these three approaches compare?

  
  
  
  