from datetime import datetime, timedelta
from typing import List

from flask_sqlalchemy import SQLAlchemy
from models.dto_models import ChartDTO, ChartQuery, VisualizationDTO
from models.db_models import Visualization

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
    


def run_rscript(visualization_id: int, timespan: timedelta, spread: timedelta, db: SQLAlchemy) -> bool:
    raise NotImplementedError

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