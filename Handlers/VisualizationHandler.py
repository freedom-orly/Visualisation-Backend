from datetime import datetime, timedelta
import subprocess
from typing import List

from flask_sqlalchemy import SQLAlchemy
from models.dto_models import ChartDTO, ChartQuery, FileUpdate, VisualizationDTO
from models.db_models import DataFile, Visualization, RScriptFile

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
    
    if not query.spread:
        return None
    
    if not query.start_date or not query.end_date:
        return None 
    
    return run_rscript(visualization=visual,start_date=query.start_date,end_date=query.end_date, spread=query.spread)
    


def run_rscript(visualization: Visualization, start_date: datetime, end_date: datetime, spread: int) -> ChartDTO | None:
    rscript: RScriptFile = visualization.r_script_files[-1] if visualization.r_script_files else None # type: ignore
    if not rscript:
        return None
    output = ""
    try:
        out = subprocess.run(['Rscript', rscript.file.file_path, start_date.strftime("%d/%m/%Y"),end_date.strftime("%d/%m/%Y")], capture_output=True, check=True)
        if out.returncode != 0:
            return None
        output = out.stdout.decode('utf-8')
    except Exception as e:
        # Handle errors in R script execution
            print(f"Error executing R script: {e}")
    
            dto = ChartDTO(
        visualization_id=visualization.id, # type: ignore
        name=visualization.name, # type: ignore
        prediction=visualization.prediction, # type: ignore
        spread=spread,
        start_date=start_date,
        end_date=end_date,
        values=[
            [0, 5],
        [1, 10],
        [2, 15],
        [3, 13],
        [4, 17],
        [5, 14],
        [6, 18],
        [7, 16],
        [8, 20],
        [9, 19],
        [10, 22],
        [11, 25],
        [12, 23],
        [13, 26],
        [14, 30],
        [15, 28],
        [16, 32],
        [17, 29],
        [18, 34],
        [19, 31],
        [20, 35],
        [21, 38],
        [22, 36],
        [23, 40],
        [24, 42],
        [25, 41],
        [26, 45],
        [27, 43],
        [28, 47],
        [29, 44],
        [30, 48]]  # Parse output to fill values
        )
            return dto
    # Parse the output to create ChartDTO
    dto = ChartDTO(
        visualization_id=visualization.id, # type: ignore
        name=visualization.name, # type: ignore
        prediction=visualization.prediction, # type: ignore
        spread=spread,
        start_date=start_date,
        end_date=end_date,
        values=[
            [0, 5],
    [1, 10],
    [2, 15],
    [3, 13],
    [4, 17],
    [5, 14],
    [6, 18],
    [7, 16],
    [8, 20],
    [9, 19],
    [10, 22],
    [11, 25],
    [12, 23],
    [13, 26],
    [14, 30],
    [15, 28],
    [16, 32],
    [17, 29],
    [18, 34],
    [19, 31],
    [20, 35],
    [21, 38],
    [22, 36],
    [23, 40],
    [24, 42],
    [25, 41],
    [26, 45],
    [27, 43],
    [28, 47],
    [29, 44],
    [30, 48]]  # Parse output to fill values
    )
    return dto

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