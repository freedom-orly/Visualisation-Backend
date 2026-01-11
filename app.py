from datetime import datetime
from flask import Flask, json, request, jsonify
import pandas as pd
import io
from flask_sqlalchemy import SQLAlchemy
from models.db_models import Base, File, DataFile, RScriptFile, Visualization
from models.dto_models import ChartQuery, FileQuery, FileUploadQuery
from werkzeug.datastructures import FileStorage
from types import SimpleNamespace
from db_models_init import db_models_init
from flask_cors import CORS, cross_origin

from Handlers import UploadHandler, VisualizationHandler
import os

db = SQLAlchemy(model_class=Base)
app = Flask(__name__)
app.config["MAX_CONTENT_LENGTH"] = 100 * 1024 * 1024  # 100 MB limit
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///visualizations.db' # not clue why this path is working but ok
app.config['DEBUG'] = True
CORS(app, resources={r"/api/*": {"origins": "http://localhost:4200"}})

db.init_app(app)
with app.app_context():
    db.create_all()
    db_models_init(db)
    
@app.route('/')
def hello_world():
    return 'hello world'

@cross_origin()
@app.route("/api/upload/data", methods=["POST"])
#Checks if file headers are valid
def file_validation():
    if 'file' not in request.files:
        return jsonify({"status": "rejected", "errors": ["No file provided in 'file' field."]}), 400
    if 'visualization_id' not in request.form:
        return jsonify({"status": "rejected", "errors": ["No visualization_id provided in 'visualization_id' field."]}), 400
    try: 
        query: FileUploadQuery = FileUploadQuery(
            file=request.files['file'],
            visualization_id=int(request.form.get("visualization_id", type=int)) # type: ignore
        )
    except Exception as e:
        return jsonify({"status": "rejected", "errors": [f"Invalid input data: {str(e)}"]}), 400
    return UploadHandler.upload_data_file(query=query, db=db)


@cross_origin()
@app.route("/api/upload/rscript", methods=["POST"]) # type: ignore
def upload_rscript():
    if 'file' not in request.files:
        return jsonify({"status": "rejected", "errors": ["No file provided in 'file' field."]}), 400
    if 'visualization_id' not in request.form:
        return jsonify({"status": "rejected", "errors": ["No visualization_id provided in 'visualization_id' field."]}), 400
    try: 
        query: FileUploadQuery = FileUploadQuery(
            file=request.files['file'],
            visualization_id=int(request.form.get("visualization_id", type=int)) # type: ignore
        )
    except Exception as e:
        return jsonify({"status": "rejected", "errors": [f"Invalid input data: {str(e)}"]}), 400
    return UploadHandler.upload_r_script_file(query=query, db=db)


@cross_origin()
@app.route("/api/rscripts/search", methods=["POST"])
def get_rscript_files():
    try:
        query: FileQuery = json.loads(request.data, object_hook=lambda d: SimpleNamespace(**d)) # This way we have mapped object with attributes instead of dict
    except Exception as e:
        return jsonify({"status": "rejected", "errors": [f"Invalid input data: {str(e)}"]}), 400
    return UploadHandler.search_rscript_files(query=query, db=db)


@cross_origin()
@app.route("/api/updates/<visualization_id>", methods=["GET"])
def get_last_rscript_by_visualization(visualization_id: int):
    return jsonify(UploadHandler.get_last_files_updates(v=visualization_id, db=db))


@cross_origin()
@app.route("/api/data/<visualization_id>", methods=["GET"])
def get_last_data_by_visualization(visualization_id: int):
    return jsonify(VisualizationHandler.get_last_data_updates(v=visualization_id, db=db))

@cross_origin()
@app.route("/api/data/search", methods=["POST"])
def get_files():
    try:
        query: FileQuery = json.loads(request.data, object_hook=lambda d: SimpleNamespace(**d)) # This way we have mapped object with attributes instead of dict
    except Exception as e:
        return jsonify({"status": "rejected", "errors": [f"Invalid input data: {str(e)}"]}), 400
    return UploadHandler.search_data_files(query=query, db=db)


@cross_origin()
@app.route("/api/files", methods=["GET"])
def list_files():
    return jsonify(UploadHandler.list_files(db=db))

@cross_origin()
@app.route("/api/files/<file_id>", methods=["DELETE"])
def delete_file(file_id: int):
    return UploadHandler.delete_file(file_id=file_id, db=db)


@cross_origin()
@app.route("/api/visualizations", methods=["GET"])
def get_visualizations():
    return jsonify(VisualizationHandler.get_visualizations(db=db))

@cross_origin()
@app.route("/api/visualization/<id>", methods=["GET"])
def get_visualization_byId(id: int):
    return jsonify(VisualizationHandler.get_visualization(db=db, id=id)) # type: ignore


@cross_origin()
@app.route("/api/visualizations/chart", methods=["POST"])
def get_chart():
    try:
        query: ChartQuery = json.loads(request.data, object_hook=lambda d: SimpleNamespace(**d))
    except Exception as e:
        return jsonify({"status": "rejected", "errors": [f"Invalid input data: {str(e)}"]}), 400
    d = VisualizationHandler.get_chart(query=query, db=db)
    if d is None:
        return jsonify({"status": "rejected", "errors": ["Chart not found"]}), 404
    return  jsonify(d)

@cross_origin()
@app.route("/api/visualizations/<id>/input-fields", methods=["GET"])
def get_visualization_input_fields(id: int):
    return jsonify(VisualizationHandler.get_visualization_input_fields(db=db, id=id)) # type: ignore

@cross_origin()
@app.route("/api/files/exists", methods=["GET"])
def check_data_files_exists():
    exists = UploadHandler.check_data_files_exists(db=db)
    return jsonify(exists)

if __name__ == '__main__':
    #db.init_app(app)
    app.run(debug=True)