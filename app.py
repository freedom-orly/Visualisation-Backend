from flask import Flask, request, jsonify
import pandas as pd
import io
from flask_sqlalchemy import SQLAlchemy
from models.db_models import Base, File, DataFile, RScriptFile, Visualization

from Handlers import UploadHandler
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
    file = request.files.get("file")
    file_name = request.form.get("file_name")

    if not file:
        return jsonify({"status": "rejected", "errors": ["No file provided in 'file' field."]}), 400
    
    if not file_name:
        return jsonify({"status": "rejected", "errors": ["No file name provided in 'file_name' field."]}), 400

    #Try read sample from file
    try:
        content = file.read()
        sample_buf = io.BytesIO(content)
    except Exception as e:
        return jsonify({"status": "rejected", "errors": [f"Failed to read uploaded file: {str(e)}"]}), 400
    
    #Check if we can read the headers
    try:
        sample_headers_df = pd.read_csv(io.BytesIO(content), nrows=0, sep=";")
        received_headers = list(sample_headers_df.columns)
    except Exception as e:
        return jsonify({"status": "rejected", "errors": [f"Unable to parse CSV headers: {str(e)}"]}), 400
    
    #Lowered headers file_name is used to see which file headers to use
    if file_name == "sales":
        req_lower = _normalize_header_set(REQUIRED_SALES_HEADERS)
    elif file_name == "visitors":
        req_lower = _normalize_header_set(REQUIRED_VISITOR_HEADERS)
    else:
        req_lower = _normalize_header_set(REQUIRED_SALES_HEADERS)

    rec_lower = _normalize_header_set(received_headers)

    #Check if headers are missing
    missing = [h for h in req_lower if h not in rec_lower]
    if missing:
        return jsonify({
            "status": "rejected",
            "errors": [f"Missing required columns: {missing}. Received: {received_headers}"]
        }), 400
    
    #Read a sample of rows for content validation
    try:
        sample_df = pd.read_csv(io.BytesIO(content), dtype=str, nrows=SAMPLE_ROWS, sep=";")
    except Exception as e:
        return jsonify({"status": "rejected", "errors": [f"Failed to parse CSV sample rows: {str(e)}"]}), 400
    

    #Return success marker.
    return jsonify({"status": "ok", "message": "Validation passed on sample rows."}), 200


@app.route("/files", methods=["GET"])
def files():
    def get_file(f):
        #get file from database
        #return file.id, name, path, uploaded time
        pass

    query = request.args.get("query", type=str)
    #If nothing then returns a list of the files
    if not query:
        # query files from database
        # return jsonify([get_file(f) for f in files]), 200
        pass
    
    #Specific file search
    q_stripped = query.strip()
    try:
        q_int = int(q_stripped)
    except ValueError:
        q_int = None

    if q_int is not None:
        #Get file object
        #if fileObject:
            #return jsonify(get_file(file_id)), 200
        #return jsonify({"error": "No file found"}), 404
        pass

    else:
        return jsonify({"error": "File id not found"}), 404


if __name__ == '__main__':
    db.init_app(app)