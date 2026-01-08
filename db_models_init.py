from flask_sqlalchemy import SQLAlchemy

from models.db_models import Visualization


def db_models_init(db: SQLAlchemy):
    
    if db.session.query(Visualization).count() != 0:
        return
    
    visualizations = [
        Visualization(
            name="Sales Data History",
            description="Historical sales data visualization",
            prediction=False
        ),
        Visualization(
            name="Weather History",
            description="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
            prediction=False
        ),
        Visualization(
            name="Sales Forecasting",
            description="Forecasting future sales based on historical data and weather information. In order to use this visualization, please upload following data files: budget.xlsx, is_holiday.csv, revenue_forecast_v1.rds, sales_location_hourly.csv, total_hourly_visitors.csv, weather_data_hourly.csv",
            prediction=True
        )
    ]

    db.session.add_all(visualizations)
    db.session.commit()
    
    