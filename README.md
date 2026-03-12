# Baseball Salary Prediction using Machine Learning

This project develops predictive models to estimate the salary of professional baseball players using the **Hitters dataset**.

The goal is to evaluate different statistical and machine learning techniques and identify the model with the best predictive performance.

## Objective

Build a predictive model capable of estimating the **Salary** variable for the following season using player performance statistics.

## Methodology

Several modeling techniques were implemented and compared:

- Ridge Regression
- Lasso Regression
- Elastic Net
- Random Forest
- Stacked Ensemble (automatic machine learning approach)

Model performance was evaluated using **Root Mean Squared Error (RMSE)** on test data.

## Model Comparison

Although the **Stacked Ensemble model** achieved the lowest RMSE, the improvement over **Random Forest** was minimal.

Considering the significantly higher complexity of the stacked model, **Random Forest was selected as the final model**.

## Final Model

Random Forest showed the best balance between:

- predictive performance
- interpretability
- model simplicity

It achieved the **lowest RMSE among the tested models in the test dataset**, outperforming traditional regression approaches.

## Tools

- R
- Machine Learning
- Model comparison
- Cross-validation

## Authors

- Marcelo Cabral
