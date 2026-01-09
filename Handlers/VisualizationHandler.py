from datetime import datetime, timedelta
import subprocess
from types import SimpleNamespace
from typing import List, Optional

from flask import json
from flask_sqlalchemy import SQLAlchemy
from models.dto_models import ChartDTO, ChartQuery, FileUpdate, VisualizationDTO, VisualizationInputFieldDTO, chartEntry, DataPoint
from models.db_models import DataFile, Visualization, RScriptFile


def get_obj_dict(obj):
    return obj.__dict__

STATIC_VALUES = [DataPoint(x=i, y=v) for i, v in [
    [0, 1203],
    [1, 1345],
    [2, 1102],
    [3, 1521],
    [4, 1603],
    [5, 1357],
    [6, 1034],
    [7, 1562],
    [8, 1428],
    [9, 1307],
    [10, 1519],
    [11, 1778],
    [12, 1632],
    [13, 1255],
    [14, 1714],
    [15, 1076],
    [16, 1341],
    [17, 1187],
    [18, 1735],
    [19, 1268],
    [20, 1616],
    [21, 1392],
    [22, 1788],
    [23, 1045],
    [24, 1130],
    [25, 1536],
    [26, 1199],
    [27, 1690],
    [28, 1573],
    [29, 1162],
    [30, 1481]  
]]
STATIC_VALUES2 = [[0, 1503],
 [1, 1675],
 [2, 1402],
 [3, 1921],
 [4, 2003],
 [5, 1757],
 [6, 1334],
 [7, 1862],
 [8, 1728],
 [9, 1607],
 [10, 1819],
 [11, 2078],
 [12, 1932],
 [13, 1555],
 [14, 2014],
 [15, 1376],
 [16, 1641],
 [17, 1487],
 [18, 2035],
 [19, 1568],
 [20, 1916],
 [21, 1692],
 [22, 2088],
 [23, 1345],
 [24, 1430],
 [25, 1836],
 [26, 1499],
 [27, 1990],
 [28, 1873],
 [29, 1462],
 [30, 1781]]

"""Gets chart data for a given chart query.

Keyword arguments:
query -- ChartQuery object containing the query parameters
db -- SQLAlchemy database session
Return: 
ChartDTO object containing the chart data
"""
def get_chart(query: ChartQuery, db: SQLAlchemy) -> ChartDTO | None:
    # Implementation of the function to fetch and return visualization data
    #Gets the Visualization from query id
    visual = db.session.get(Visualization, query.id, options=[
        db.joinedload(Visualization.r_script_files).joinedload(RScriptFile.file)
    ])
    #ID check
    if not visual or not visual.r_script_files:
        return None
    
    # if not query.spread:
    #     return None
    
    # if not query.start_date or not query.end_date:
    #     return None 
    
    return run_rscript(visualization=visual,inputs=query.inputs)
    


def run_rscript(visualization: Visualization, inputs: Optional[dict] = None) -> ChartDTO | None:
    rscript: RScriptFile = visualization.r_script_files[-1] if visualization.r_script_files else None # type: ignore
    if not rscript:
        return None
    output = ""
    parsed_values: list[chartEntry] = []
    inputs_json_str = json.dumps(inputs, default=get_obj_dict) if inputs else "{}"
    
    try:
        out = subprocess.run(['Rscript', rscript.file.file_path,str(visualization.id),inputs_json_str], capture_output=True, check=True, )
        if out.returncode != 0:
            return None
        output = out.stdout.decode('utf-8')
        parsed_values = get_values_from_output(output)  
    except subprocess.CalledProcessError as e:
        # Handle errors in R script execution
            print(f"Error executing R script: {e}")
            dto = ChartDTO(
        visualization_id=visualization.id, # type: ignore
        name=visualization.name, # type: ignore
        prediction=visualization.prediction, # type: ignore
        chartType=visualization.chart_type, # type: ignore
        values= [
            chartEntry('store1', STATIC_VALUES),
            chartEntry('store2', STATIC_VALUES)
        ]
        )
            return dto
    # Parse the output to create ChartDTO
    dto = ChartDTO(
        visualization_id=visualization.id, # type: ignore
        name=visualization.name, # type: ignore
        prediction=visualization.prediction, # type: ignore
        values= parsed_values,
        chartType=visualization.chart_type, # type: ignore
        )
    return dto

def get_values_from_output(output: str) -> list[chartEntry]:
    dirty_json = json.loads(output)
    data: list[chartEntry] = []
    for entry in dirty_json:
        name = entry.get("name")
        values_list = entry.get("values", [])
        values: list[DataPoint] = []
        for val in values_list:
            x = val['x']
            y = val['y']
            values.append(DataPoint(x=x, y=y))
        data.append(chartEntry(name=name, values=values))
    return data
    


def get_visualization_max_timespan(id: int, db: SQLAlchemy):
    visual = db.session.get(Visualization, id, options=[
        db.joinedload(Visualization.r_script_files).joinedload(RScriptFile.file)
    ])
    # TODO

def get_last_data_updates(v: int, db: SQLAlchemy) -> List[FileUpdate]:
    one_month_ago = datetime.now() - timedelta(days=30)
    files = db.session.query(DataFile).filter(
        DataFile.visualization_id == v, # type: ignore
        DataFile.upload_time >= one_month_ago # type: ignore
    ).all()
    return [
        FileUpdate(
            file_id=f.id, # type: ignore
            file_name=f.name, # type: ignore
            upload_time=f.upload_time # type: ignore
        ) for f in files
    ]
    
def get_visualization_input_fields(db: SQLAlchemy, id: int) -> List[VisualizationInputFieldDTO]:
    visual = db.session.get(Visualization, id, options=[
        db.joinedload(Visualization.fields)
    ])
    if not visual:
        return []
    input_fields_dto: List[VisualizationInputFieldDTO] = []
    for field in visual.fields:
        field_dto = VisualizationInputFieldDTO(
            id=field.id, # type: ignore
            visualization_id=field.visualization_id, # type: ignore
            field_name=field.field_name, # type: ignore
            field_type=field.field_type, # type: ignore
            field_label=field.field_label, # type: ignore
            options=field.options.split(",") if field.options else None, # type: ignore
            default_value=field.default_value, # type: ignore
            required=field.required # type: ignore
        )
        input_fields_dto.append(field_dto)
    return input_fields_dto
    
def get_last_rscripts_updates(v: int, db: SQLAlchemy) -> List[FileUpdate]:
    one_month_ago = datetime.now() - timedelta(days=30)
    files = db.session.query(RScriptFile).filter(
        RScriptFile.visualization_id == v, # type: ignore
        RScriptFile.upload_time >= one_month_ago # type: ignore
    ).all()
    return [
        FileUpdate(
            file_id=f.id, # type: ignore
            file_name=f.name, # type: ignore
            upload_time=f.upload_time # type: ignore
        ) for f in files
    ]


def get_visualizations(db: SQLAlchemy) -> List[VisualizationDTO]:
    dbQuery = db.session.query(Visualization)
    query_results: List[Visualization] = dbQuery.all()
    results: List[VisualizationDTO] = []
    for v in query_results:
        vis_dto = VisualizationDTO(
            id=v.id, # type: ignore
            name=v.name, # type: ignore
            description=v.description, # type: ignore
            is_prediction=v.prediction, # type: ignore
        )
        results.append(vis_dto)
    return results

def get_visualization(db: SQLAlchemy, id: int) -> VisualizationDTO | None:
    v: Visualization = db.session.get(Visualization, id)
    if not v:
        return None
    vis_dto = VisualizationDTO(
        id=v.id, # type: ignore
        name=v.name, # type: ignore
        description=v.description, # type: ignore
        is_prediction=v.prediction, # type: ignore
    )
    return vis_dto