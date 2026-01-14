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
            description="Historical sales data visualization. Files needed 'sales.csv'",
            chart_type="bar",
            prediction=False
        ),
        Visualization(
            name="Weather History",
            description="Historical weather data for the past week. Files needed: 'sales.csv', 'weather.xlsx'",
            prediction=False,
            chart_type="line"
        ),
        Visualization(
            name="Sales Forecasting",
            description="Forecasting future sales based on historical data and weather information. In order to use this visualization. The forcasting is 1 week ahead of the last date in 'sales.csv'. Files needed: 'sales.csv', 'hours.xlxs', 'linktables.xlsx', 'departments.xlsx', 'holidays.xlsx', 'teams.xlsx', 'store.csv', 'subgroup.csv', 'maingroup.csv', 'visitorhourly.csv', 'weather.xlsx'",
            prediction=True,
            chart_type="bar"
            
        ),
        Visualization(
            name="Scenario Engine",
            description="This tool is used to simulate best/worst case scenarios from specified store closures. With this tool you can specify what stores to close, and you will get what the distribution of sales from the closed stores to new stores will be. Files needed: 'sales.csv' and 'subgroup_catagory.csv'",
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
            default_value="[131, 135]",
        ),
        VisualizationInputField(
            visualization_id=db.session.query(Visualization).filter_by(name="Sales Forecasting").first().id, # type: ignore
            field_name="forecast_horizon_days",
            field_type="number",
            required=True,
            field_label="Forecast Horizon (Days)",
            default_value="16",
        ),
        VisualizationInputField(
            visualization_id=db.session.query(Visualization).filter_by(name="Sales Forecasting").first().id, # type: ignore
            field_name="stores",
            field_type="selectMulti",
            required=True,
            field_label="Stores to Forecast",
            options="101, 102, 108, 110, 111, 116, 120, 129, 131, 135, 138, 140, 145, 149, 156, 164",
            default_value="[101, 102, 108]",
        ),
        VisualizationInputField(
            visualization_id=db.session.query(Visualization).filter_by(name="Weather History").first().id, # type: ignore
            field_name="stores",
            field_type="selectMulti",
            required=True,
            field_label="Stores to Show",
            options="101, 102, 108, 110, 111, 116, 120, 129, 131, 135, 138, 140, 145, 149, 156, 164",
            default_value="[101, 102, 108]",
        ),
        VisualizationInputField(
            visualization_id=db.session.query(Visualization).filter_by(name="Sales Data History").first().id, # type: ignore
            field_name="stores",
            field_type="selectMulti",
            required=True,
            field_label="Stores to Show",
            options="101, 102, 108, 110, 111, 116, 120, 129, 131, 135, 138, 140, 145, 149, 156, 164",
            default_value="[101, 102, 108]",
        ),
        
    ]
    
    db.session.add_all(input_fields)
    db.session.commit()
    
    if db.session.query(RScriptFile).count() == 0:
        file_dir = os.path.dirname(os.path.realpath('__file__'))
        with open(os.path.join(file_dir, "default_rscripts/forecast_prep.R"), "rb") as f:
            UploadHandler.upload_r_script_file(db=db, query=FileUploadQuery(
                file=FileStorage(f),
                visualization_id=db.session.query(Visualization).filter_by(name="Sales Forecasting").first().id, # type: ignore
            ))
        with open(os.path.join(file_dir, "default_rscripts/forecast.r"), "rb") as f:
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
        with open(os.path.join(file_dir, "default_rscripts/recent_sales_&_weather.R"), "rb") as f: 
            UploadHandler.upload_r_script_file(db=db, query=FileUploadQuery(
                file=FileStorage(f),
                visualization_id=db.session.query(Visualization).filter_by(name="Weather History").first().id, # type: ignore
            ))


    
    