import snowflake.snowpark as snowpark
from snowflake.snowpark.functions import col, lit, call_function
from datetime import datetime

def filter_nyc_taxi_data_snowpark(df, year, month, session):
    start_date = datetime(year, month, 1)
    if month == 12:
        end_date = datetime(year + 1, 1, 1)
    else:
        end_date = datetime(year, month + 1, 1)

    stats = {}

    # Initial count for this year/month
    total_records = df.filter((col("YEAR") == year) & (col("MONTH") == month)).count()
    stats['year'] = year
    stats['month'] = month
    stats['total_records'] = total_records

    # Filter to just this year/month for all further steps
    df = df.filter((col("YEAR") == year) & (col("MONTH") == month))

    # Drop rows with missing values in critical columns
    critical_columns = [
        "TPEP_PICKUP_DATETIME",
        "TPEP_DROPOFF_DATETIME",
        "TOTAL_AMOUNT",
        "PULOCATIONID",
        "DOLOCATIONID",
        "TRIP_DISTANCE",
        "PASSENGER_COUNT",
    ]
    before = df.count()
    for c in critical_columns:
        df = df.filter(col(c).is_not_null())
    after = df.count()
    stats['dropped_missing'] = before - after
    before = after

    # Add duration column (in seconds)
    df = df.with_column(
        "DURATION",
        call_function("DATEDIFF", lit("second"), col("TPEP_PICKUP_DATETIME"), col("TPEP_DROPOFF_DATETIME"))
    )

    # Compute 99.9th percentiles for duration, total_amount, trip_distance
    quantiles = (
        df.agg(
            call_function("APPROX_PERCENTILE", col("DURATION"), lit(0.999)).alias("MAX_DURATION"),
            call_function("APPROX_PERCENTILE", col("TOTAL_AMOUNT"), lit(0.999)).alias("MAX_TOTAL_AMOUNT"),
            call_function("APPROX_PERCENTILE", col("TRIP_DISTANCE"), lit(0.999)).alias("MAX_DISTANCE"),
        )
        .collect()[0]
    )
    max_duration = quantiles["MAX_DURATION"]
    max_total_amount = quantiles["MAX_TOTAL_AMOUNT"]
    max_distance = quantiles["MAX_DISTANCE"]

    min_total_amount = 2.5
    invalid_location_ids = [1, 264, 265]
    min_passenger_count = 1
    max_passenger_count = 5

    # Duration filter
    duration_filter = (col("DURATION") > 0) & (col("DURATION") <= max_duration)
    after = df.filter(duration_filter).count()
    stats['dropped_duration'] = before - after
    df = df.filter(duration_filter)
    before = after

    # Total amount filter
    total_amount_filter = (col("TOTAL_AMOUNT") >= min_total_amount) & (col("TOTAL_AMOUNT") <= max_total_amount)
    after = df.filter(total_amount_filter).count()
    stats['dropped_total_amount'] = before - after
    df = df.filter(total_amount_filter)
    before = after

    # Distance filter
    distance_filter = (col("TRIP_DISTANCE") > 0) & (col("TRIP_DISTANCE") <= max_distance)
    after = df.filter(distance_filter).count()
    stats['dropped_distance'] = before - after
    df = df.filter(distance_filter)
    before = after

    # NYC location filter
    nyc_location_filter = ~col("PULOCATIONID").isin(invalid_location_ids)
    after = df.filter(nyc_location_filter).count()
    stats['dropped_location'] = before - after
    df = df.filter(nyc_location_filter)
    before = after

    # Date range filter
    date_range_filter = (col("TPEP_PICKUP_DATETIME") >= lit(start_date)) & (col("TPEP_PICKUP_DATETIME") < lit(end_date))
    after = df.filter(date_range_filter).count()
    stats['dropped_date_range'] = before - after
    df = df.filter(date_range_filter)
    before = after

    # Passenger count filter
    passenger_count_filter = (col("PASSENGER_COUNT") >= min_passenger_count) & (col("PASSENGER_COUNT") <= max_passenger_count)
    after = df.filter(passenger_count_filter).count()
    stats['dropped_passenger_count'] = before - after
    df = df.filter(passenger_count_filter)
    before = after

    # Final valid records
    valid_records = df.count()
    stats['valid_records'] = valid_records
    stats['records_dropped'] = stats['total_records'] - valid_records
    stats['percent_dropped'] = (stats['records_dropped'] / stats['total_records'] * 100) if stats['total_records'] else 0

    # Rename and select columns, and add YEAR and MONTH columns
    df = (
        df.with_column_renamed("TPEP_PICKUP_DATETIME", "PICKUP_DATETIME")
          .with_column_renamed("TPEP_DROPOFF_DATETIME", "DROPOFF_DATETIME")
          .with_column_renamed("PULOCATIONID", "PICKUP_LOCATION_ID")
          .with_column_renamed("DOLOCATIONID", "DROPOFF_LOCATION_ID")
          .with_column("YEAR", lit(year))
          .with_column("MONTH", lit(month))
          .select(
              col("PICKUP_DATETIME"),
              col("DROPOFF_DATETIME"),
              col("PICKUP_LOCATION_ID"),
              col("DROPOFF_LOCATION_ID"),
              col("TRIP_DISTANCE"),
              col("FARE_AMOUNT"),
              col("TIP_AMOUNT"),
              col("PAYMENT_TYPE"),
              col("PASSENGER_COUNT"),
              col("YEAR"),
              col("MONTH"),
          )
    )
    return df, stats

def main(session: snowpark.Session):
    source_table = "NYC_DATA.PUBLIC.YELLOW_TAXI_DATA_RAW"
    dest_table = "NYC_DATA.PUBLIC.YELLOW_TAXI_DATA_FILTERED"

    # Delete the filtered table if it exists
    session.sql(f"DROP TABLE IF EXISTS {dest_table}").collect()

    taxi_df = session.table(source_table)

    unique_month_years = (
        taxi_df
        .select(col("YEAR"), col("MONTH"))
        .distinct()
        .collect()
    )

    all_stats = []
    for row in unique_month_years:
        year = row["YEAR"]
        month = row["MONTH"]
        filtered_df, stats = filter_nyc_taxi_data_snowpark(taxi_df, year, month, session)
        all_stats.append(stats)
        if stats['valid_records'] > 0:
            filtered_df.write.mode("append").save_as_table(dest_table)
        print(
            f"Year: {year}, Month: {month} | "
            f"Total: {stats['total_records']}, Valid: {stats['valid_records']}, "
            f"Dropped: {stats['records_dropped']} ({stats['percent_dropped']:.2f}%) | "
            f"Missing: {stats['dropped_missing']}, Duration: {stats['dropped_duration']}, "
            f"TotalAmt: {stats['dropped_total_amount']}, Distance: {stats['dropped_distance']}, "
            f"Location: {stats['dropped_location']}, DateRange: {stats['dropped_date_range']}, "
            f"Passenger: {stats['dropped_passenger_count']}"
        )

    # Optionally, show a sample of the filtered data
    result_df = session.table(dest_table).limit(10)
    result_df.show()

    # Return stats as a DataFrame for review in worksheet
    return session.create_dataframe(all_stats)

from snowflake.snowpark import Session
from dotenv import load_dotenv
import os

if __name__ == "__main__":
    load_dotenv()
    
    connection_parameters = {
        "account": os.getenv("SNOWFLAKE_ACCOUNT"),
        "user": os.getenv("SNOWFLAKE_USER"),
        "password": os.getenv("SNOWFLAKE_PASSWORD"),
        "role": os.getenv("SNOWFLAKE_ROLE"),
        "warehouse": os.getenv("SNOWFLAKE_WAREHOUSE"),
        "database": os.getenv("SNOWFLAKE_DATABASE"),
        "schema": os.getenv("SNOWFLAKE_SCHEMA"),
    }

    print("🚀 Connecting to Snowflake...")
    session = Session.builder.configs(connection_parameters).create()
    
    try:
        print("🧼 Starting the filtering process...")
        # THIS IS THE MISSING LINE:
        main(session) 
    finally:
        session.close()
        print("✅ Session closed.")