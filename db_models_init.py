import os
from flask_sqlalchemy import SQLAlchemy

from Handlers import UploadHandler
from models.db_models import RScriptFile, Visualization, VisualizationInputField
from models.dto_models import FileUploadQuery
from werkzeug.datastructures import FileStorage

def db_models_init(db: SQLAlchemy):
    
    if db.session.query(Visualization).count() != 0:
        return
    
    visualizations = [
        Visualization(
            name="Sales Data History",
            description="Historical sales data visualization",
            chart_type="line",
            prediction=False
        ),
        Visualization(
            name="Weather History",
            description="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
            prediction=False,
            chart_type="line"
        ),
        Visualization(
            name="Sales Forecasting",
            description="Forecasting future sales based on historical data and weather information. In order to use this visualization, please upload following data files: budget.xlsx, is_holiday.csv, revenue_forecast_v1.rds, sales_location_hourly.csv, total_hourly_visitors.csv, weather_data_hourly.csv",
            prediction=True,
            chart_type="line"
            
        ),
        Visualization(
            name="Scenario Engine",
            description="Upload your own data and R script to create a custom visualization.",
            prediction=True,
            chart_type="bar"
        ),
    ]
    
    db.session.add_all(visualizations)
    db.session.commit()
    
    input_fields = [
        VisualizationInputField(
            visualization_id=db.session.query(Visualization).filter_by(name="Scenario Engine").first().id, # type: ignore
            field_name="loss_rate",
            field_type="number",
            required=True,
            field_label="Loss Rate",
            default_value="0.20",
        ),
        VisualizationInputField(
            visualization_id=db.session.query(Visualization).filter_by(name="Scenario Engine").first().id, # type: ignore
            field_name="stores_to_close",
            field_type="selectMulti",
            required=False,
            field_label="Stores to Close",
            options="101, 102, 108, 110, 111, 116, 120, 129, 131, 135, 138, 140, 145, 149, 156, 164",
            default_value="108, 120",
        ),
        VisualizationInputField(
            visualization_id=db.session.query(Visualization).filter_by(name="Sales Forecasting").first().id, # type: ignore
            field_name="start_date",
            field_type="date",
            required=True,
            field_label="Forecast Start Date",
            default_value="2025-01-01",
        ),
        VisualizationInputField(
            visualization_id=db.session.query(Visualization).filter_by(name="Sales Forecasting").first().id, # type: ignore
            field_name="end_date",
            field_type="date",
            required=True,
            field_label="Forecast End Date",
            default_value="2025-01-30",
        ),
        VisualizationInputField(
            visualization_id=db.session.query(Visualization).filter_by(name="Sales Forecasting").first().id, # type: ignore
            field_name="spread",
            field_type="number",
            required=True,
            field_label="Forecast Spread",
            default_value="1",
        ),
    ]
    
    db.session.add_all(input_fields)
    db.session.commit()
    
    if db.session.query(RScriptFile).count() == 0:
        file_dir = os.path.dirname(os.path.realpath('__file__'))
        with open(os.path.join(file_dir, "default_rscripts/helper_forecast.R"), "rb") as f:
            UploadHandler.upload_r_script_file(db=db, query=FileUploadQuery(
                file=FileStorage(f),
                visualization_id=db.session.query(Visualization).filter_by(name="Sales Forecasting").first().id, # type: ignore
            ))
        with open(os.path.join(file_dir, "default_rscripts/forcast_aggregator.R"), "rb") as f:
            UploadHandler.upload_r_script_file(db=db, query=FileUploadQuery(
                file=FileStorage(f),
                visualization_id=db.session.query(Visualization).filter_by(name="Sales Forecasting").first().id, # type: ignore
            ))
        with open(os.path.join(file_dir, "default_rscripts/data_prep_functions.R"), "rb") as f: 
            UploadHandler.upload_r_script_file(db=db, query=FileUploadQuery(
                file=FileStorage(f),
                visualization_id=db.session.query(Visualization).filter_by(name="Scenario Engine").first().id, # type: ignore
            ))
        with open(os.path.join(file_dir, "default_rscripts/updated_scenario_engine.R"), "rb") as f: 
            UploadHandler.upload_r_script_file(db=db, query=FileUploadQuery(
                file=FileStorage(f),
                visualization_id=db.session.query(Visualization).filter_by(name="Scenario Engine").first().id, # type: ignore
            ))
        with open(os.path.join(file_dir, "default_rscripts/get_recent_sales.R"), "rb") as f: 
            UploadHandler.upload_r_script_file(db=db, query=FileUploadQuery(
                file=FileStorage(f),
                visualization_id=db.session.query(Visualization).filter_by(name="Sales Data History").first().id, # type: ignore
            ))


    
    