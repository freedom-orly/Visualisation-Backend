from datetime import datetime, timedelta
import subprocess
from typing import List

from flask_sqlalchemy import SQLAlchemy
from models.dto_models import ChartDTO, ChartQuery, VisualizationDTO
from models.db_models import Visualization, RScriptFile

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
    visual = db.session.get(Visualization, query.id)
    #ID check
    if not visual:
        return None
    
    if query.spread:
        return None
    
    if query.timespan:
        return None 
    


def run_rscript(visualization_id: int, timespan: timedelta, spread: timedelta, db: SQLAlchemy) -> ChartDTO | None:
    vis = db.session.get(Visualization, visualization_id, options=[
        db.joinedload(Visualization.r_script_files)
    ])
    if not vis:
        return None
    rscript: RScriptFile = vis.r_script_files[0] if vis.r_script_files else None # type: ignore
    if not rscript:
        return None
    out = subprocess.run(['Rscript', rscript.file.file_path, str(timespan), str(spread)], capture_output=True, check=True)
    if out.returncode != 0:
        return None
    output = out.stdout.decode('utf-8')
    # Parse the output to create ChartDTO
    dto = ChartDTO(
        visualization_id=visualization_id,
        name=vis.name, # type: ignore
        prediction=vis.prediction, # type: ignore
        spread=spread,
        timespan=timespan,
        values=[]  # Parse output to fill values
    )
    return dto
    

def get_last_updates(v: int, db: SQLAlchemy) -> List[datetime]:
    raise NotImplementedError


def get_visualizations(db: SQLAlchemy) -> List[VisualizationDTO]:
    dbQuery = db.session.query(Visualization)
    query_results: List[Visualization] = dbQuery.all()
    results: List[VisualizationDTO] = []
    for v in query_results:
        last_updates: List[datetime] = get_last_updates(v.id, db) # type: ignore
        vis_dto = VisualizationDTO(
            id=v.id, # type: ignore
            name=v.name, # type: ignore
            description=v.description, # type: ignore
            is_prediction=v.prediction, # type: ignore
            last_updates=last_updates
        )
        results.append(vis_dto)
    return results