# ML_PortFolio_Project


This repository showcases two machine learning projects focusing on predictive analytics using R:

1. **Electric Vehicle Range Prediction**: Predict the electric range of vehicles using features like model year, vehicle type, and MSRP.  
   - **Best Model**: Gradient Boosting  
   - **Results**: RMSE = 8.71, R² = 0.99, MAE = 4.11  

2. **Employee Turnover Prediction**: Analyze and predict employee attrition based on job satisfaction, project count, and working hours.  
   - **Model Used**: Logistic Regression  
   - **Results**: Accuracy = 78.9%, MAE = 0.211, RMSE = 0.459  

## Project Structure

```
ML_Portfolio_Project/
├── data/                  # Datasets for analysis
│   ├── Electric_Vehicle_Population_Data.csv
│   ├── turnover.csv
├── scripts/               # R scripts for modeling
│   ├── Electric_Vehicle_Prediction.R
│   ├── Employee_Turnover_Prediction.R
└── README.md              # Project overview
```

## How to Run

1. Clone the repository:  
   `git clone https://github.com/<your-username>/ML_Portfolio_Project.git`  
2. Open RStudio or any R environment.  
3. Install required packages:  
   `install.packages(c("tidyverse", "caret", "randomForest", "gbm", "e1071", "pROC"))`  
4. Run the scripts in the `scripts/` folder:  
   - `Electric_Vehicle_Prediction.R` for vehicle range analysis.  
   - `Employee_Turnover_Prediction.R` for attrition prediction.  

## Author

Ashutosh Sudhirkumar Alone  
  
 

For issues or suggestions, feel free to raise an issue in this repository.
```

---

