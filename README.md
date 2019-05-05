# RegressionAnalysis-FIFA2019-OverallRating-Prediction      
Predicting Overall Rating for FIFA 2019 Dataset using only Regression based techniques      

[![Generic badge](https://img.shields.io/badge/Statistical%20Computing-R-brightgreen.svg)](https://shields.io/)


[Link for DataSet](https://www.kaggle.com/karangadiya/fifa19)     

Used All Numerical Features except Potential Rating for predicting Overall Rating.        

Final Scores :   

Adjusted R^2 = 0.90     
RMSE = 0.11      

[Link to Kaggle R Notebook](https://www.kaggle.com/h11584/regressionanalysis-fifa2019-overallrating)     


Visualize first 100 Predictions :     
![alt text](https://github.com/harshraj11584/RegressionAnalysis-FIFA2019-OverallRating-Prediction/blob/master/Graphs/final_prediction.png)


Process :   

Removed Multicollinear Features, tested Global and Individual Significance Hypotheses.      
Residual Analysis Plots - 1. Residuals vs Fitted 2. QQ Plot 3. Scale Location 4. Residuals vs Leverage     
Residuals Look like Normal Distribution, but Shapiro-Wilk test rejects it. Shown that this is expected by simulating Shapiro-Wilk Test that rejects Random Normal Variables with slight deviation for n=5000 dataset.     
Used the BoxCox transformation on y to stabilize variance, removing Outliers detected using Leverage, Cook's Distance, DFBETAS, DFFITS, and COVRATIO techniques, then retrained model.   
Also implemented olssr Model Selection Techniques- Backward Elimination, Stepwise Elimination, Step Back AIC, Step Forward AIC.     
