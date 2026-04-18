# NYC Taxi Demand Prediction Pipeline 🚖

An end-to-end Data Engineering and Machine Learning project leveraging **Snowflake**, **Snowpark**, and **LightGBM** to forecast taxi demand across New York City.

## 📊 Project Overview
This project processes over 60 million rows of NYC Yellow Taxi records. It automates the extraction, cleaning, and transformation of raw data into a time-series format, followed by training a Gradient Boosting model to predict future ride demand by hour and location.

## 🛠️ Technology Stack
* **Database:** Snowflake (Data Warehouse)
* **API:** Snowpark Python (DataFrame API)
* **ML Model:** LightGBM (Gradient Boosting)
* **Language:** Python 3.13
* **Visualization:** Snowflake Snowsight SQL Dashboards

## 🏗️ Architecture & Workflow

1.  **Ingestion:** Uploads raw datasets into Snowflake Internal Stages and into a `RAW` table.
2.  **Filtering (`filter_data.py`):** Uses Snowpark to remove "garbage" data—rides with 0 passengers, impossible distances, or outliers in the 99.9th percentile of fares/duration.
3.  **Transformation (`transform_data.py`):** Resamples individual trip records into an **Hourly Time-Series**. It ensures that "quiet hours" with 0 rides are explicitly accounted for.
4.  **Modeling (`02_train_and_predict.ipynb`):** * Engineers features: `hour_of_day`, `day_of_week`, and `is_weekend`.
    * Trains a LightGBM regressor.
    * Writes predicted demand back to a `PREDICTIONS` table in Snowflake.

## 🚀 Setup & Execution

### Prerequisites
* A Snowflake account (Standard or Trial).
* Python 3.13 environment.

### 1. Environment Configuration
Create a `.env` file in the root directory (do not commit this to Git!):
```env
SNOWFLAKE_ACCOUNT=your_account_identifier
SNOWFLAKE_USER=your_username
SNOWFLAKE_PASSWORD=your_password
SNOWFLAKE_ROLE=ACCOUNTADMIN
SNOWFLAKE_WAREHOUSE=COMPUTE_WH
SNOWFLAKE_DATABASE=NYC_DATA
SNOWFLAKE_SCHEMA=PUBLIC

2. Install Dependencies:

Bash: pip install snowflake-snowpark-python pandas python-dotenv lightgbm

3. Run the Pipeline:

Execute in order:

python filter_data.py — Cleans the raw data.

python transform_data.py — Creates the time-series aggregations.

Run 02_train_and_predict.ipynb — Trains the model and saves predictions.

📈 Visualization
Use the provided dashboard.sql in a Snowflake SQL Worksheet. The query is optimized to compare ACTUAL_RIDES vs PREDICTED_RIDES for high-traffic zones like Central Park (Location #43).

X-Axis: PICKUP_HOUR

Y-Axis: ACTUAL_RIDES and PREDICTED_RIDES
