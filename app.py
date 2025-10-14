from flask import Flask, json, request, jsonify
import pandas as pd
import io
from flask_sqlalchemy import SQLAlchemy
from models.db_models import Base, File, DataFile, RScriptFile, Visualization
from models.dto_models import ChartQuery, FileQuery
from types import SimpleNamespace

from Handlers import UploadHandler, VisualizationHandler
import os

db = SQLAlchemy(model_class=Base)
app = Flask(__name__)
app.config["MAX_CONTENT_LENGTH"] = 100 * 1024 * 1024  # 100 MB limit
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///visualizations.db'


REQUIRED_SALES_HEADERS = [
    "ReceiptDateTime", "ArticleId", "NetAmountExcl",
    "Quantity", "Article", "SubgroupId", "MaingroupId", "StoreId"
]

REQUIRED_VISITOR_HEADERS = [
    "AccessGroupId", "Date", "Time", "NumberOfUsedEntrances"
]

SAMPLE_ROWS = 1000
MAX_WARN_ROWS_SHOWN = 10

#-------Helpers---------

#Return set of lowercased column names for case-insensitive comparison.
def _normalize_header_set(cols):
    return {c.strip().lower() for c in cols}

#-----------------------

@app.route('/')
def hello_world():
    return 'Hello World'


@app.route("/api/upload/data", methods=["POST"])
#Checks if file headers are valid
def file_validation():
    return UploadHandler.upload_file(request=request, db=db)


@app.route("/api/files/search", methods=["POST"], )
def get_files():
    query: FileQuery = json.loads(request.data, object_hook=lambda d: SimpleNamespace(**d)) # This way we have mapped object with attributes instead of dict
    return UploadHandler.search_files(query=query, db=db)

@app.route("/api/visualizations", methods=["GET"])
def get_visualizations():
    return jsonify(VisualizationHandler.get_visualizations(db=db))

@app.route("/api/visualizations/chart", methods=["POST"])
def get_chart():
    query: ChartQuery = json.loads(request.data, object_hook=lambda d: SimpleNamespace(**d))
    return  jsonify(VisualizationHandler.get_chart(query=query, db=db))

if __name__ == '__main__':
    db.init_app(app)