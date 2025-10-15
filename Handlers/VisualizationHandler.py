from typing import List, jsonify

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
def get_chart(query: ChartQuery, db: SQLAlchemy) -> ChartDTO:
    # Implementation of the function to fetch and return visualization data
    #Gets the Visualization from query id
    visual = db.session.get(Visualization, query.id)
    #ID check
    if not visual:
        return None
    
    if query.spread:
        pass#checks spread

    pass  # Placeholder return


def get_visualizations(db: SQLAlchemy) -> List[VisualizationDTO]:
    dbQuery = db.session.query(Visualization)
    results = dbQuery.all()
    return jsonify([results]), 200  