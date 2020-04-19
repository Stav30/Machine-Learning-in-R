# Advanced Data Mining with R

Analyzed a spam big data set consisting of 4601 observations and 57 predictors. Prepared PowerPoint slides and presented analysis; wrote 8-page paper; conducted extensive programming and graphics in R.

   1.	## Data preparation
      o	Sequentially named variables that were not acceptably named in R.
      o	Standardized my predictors and then recombined the data set with the label vector.
      o	Shuffled the data (it was grouped by label) and placed back into data frame object.
   2.	## Analysis:
      o	Applied 10 SLM’s (KNN, Ridge + Logistic Regression, Lasso + Logistic Regression, Trees, SVM with radial kernel, stepwise logistic regression reduced model, Linear Discriminant Analysis, QDA, Logistic Regression.) to data and obtained test errors for all 10 models, train errors for 3 best models.
      o	Obtained top 5 coefficients for words most associated with spam.
      o	Used for loop and matrix sub setting in order to obtain median error rates over 10 different random samplings and median lambda parameter values for Ridge + Logistic Regression over 100 random samples.
