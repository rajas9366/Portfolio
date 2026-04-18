import snowflake.snowpark as snowpark
from snowflake.snowpark.functions import col, call_function, count, coalesce, lit, min as sf_min, max as sf_max
from datetime import timedelta
import os
from dotenv import load_dotenv
from snowflake.snowpark import Session

def main(session: snowpark.Session):
    source_table = "NYC_DATA.PUBLIC.YELLOW_TAXI_DATA_FILTERED"
    dest_table = "NYC_DATA.PUBLIC.YELLOW_TAXI_DATA_TRANSFORMED"

    df = session.table(source_table)

    # Get all unique (YEAR, MONTH) pairs
    year_months = (
        df.select(col("YEAR"), col("MONTH"))
          .distinct()
          .sort(col("YEAR"), col("MONTH"))
          .collect()
    )

    first = True
    for row in year_months:
        year = row["YEAR"]
        month = row["MONTH"]
        print(f"Processing YEAR={year}, MONTH={month}")

        # Filter for this year-month
        df_ym = df.filter((col("YEAR") == year) & (col("MONTH") == month))

        # Floor pickup_datetime to the hour
        df_ym = df_ym.with_column(
            "pickup_hour",
            call_function("DATE_TRUNC", lit("hour"), col("PICKUP_DATETIME"))
        )

        # Aggregate: count rides per hour and location
        agg_df = (
            df_ym.group_by(col("pickup_hour"), col("PICKUP_LOCATION_ID"))
                 .agg(count(lit(1)).alias("rides"))
        )

        # Get full range of hours for this month
        min_max = agg_df.agg(
            sf_min(col("pickup_hour")).alias("min_hour"),
            sf_max(col("pickup_hour")).alias("max_hour")
        ).collect()[0]
        min_hour = min_max["MIN_HOUR"]
        max_hour = min_max["MAX_HOUR"]

        if min_hour is None or max_hour is None:
            print(f"No data for YEAR={year}, MONTH={month}")
            continue

        # Generate all hours between min and max (inclusive)
        hours = []
        current = min_hour
        while current <= max_hour:
            hours.append((current,))
            current += timedelta(hours=1)
        hours_df = session.create_dataframe(hours, schema=["pickup_hour"])

        # Get all unique locations for this month
        locations_df = agg_df.select(col("PICKUP_LOCATION_ID")).distinct()

        # Cross join hours and locations to get all combinations
        all_combinations = hours_df.cross_join(locations_df)

        # Left join aggregated rides onto all combinations
        full_df = (
            all_combinations
            .join(
                agg_df,
                (all_combinations["pickup_hour"] == agg_df["pickup_hour"]) &
                (all_combinations["PICKUP_LOCATION_ID"] == agg_df["PICKUP_LOCATION_ID"]),
                how="left"
            )
            .select(
                all_combinations["pickup_hour"].alias("pickup_hour"),
                all_combinations["PICKUP_LOCATION_ID"].alias("PICKUP_LOCATION_ID"),
                coalesce(agg_df["rides"], lit(0)).cast("int").alias("rides"),
                lit(year).alias("YEAR"),
                lit(month).alias("MONTH")
            )
        )

        # Write to output table
        mode = "overwrite" if first else "append"
        full_df.write.mode(mode).save_as_table(dest_table)
        first = False

        print(f"Finished YEAR={year}, MONTH={month}")

    # Show a sample
    result = session.table(dest_table).sort(col("PICKUP_LOCATION_ID"), col("pickup_hour")).limit(10)
    result.show()
    return result

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

    print("🚀 Connecting to Snowflake for Transformation...")
    session = Session.builder.configs(connection_parameters).create()
    
    try:
        main(session)
    finally:
        session.close()
        print("✅ Transformation Complete. Session closed.")