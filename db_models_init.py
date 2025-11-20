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
            description="asdasdasdasdsadasdasdsadasdasdasdasdasdasdasdasdsad",
            prediction=False
        ),
        Visualization(
            name="Sales Forecasting",
            description="Forecasting future sales based on historical data",
            prediction=True
        )
    ]

    db.session.add_all(visualizations)
    db.session.commit()
    
    