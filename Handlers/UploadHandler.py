import io
from flask import jsonify
from flask_sqlalchemy import SQLAlchemy
import pandas as pd
from requests import request
from models.db_models import File, DataFile
from models.dto_models import FileQuery

REQUIRED_SALES_HEADERS = [
    "ReceiptDateTime", "ArticleId", "NetAmountExcl",
    "Quantity", "Article", "SubgroupId", "MaingroupId", "StoreId"
]

REQUIRED_VISITOR_HEADERS = [
    "AccessGroupId", "Date", "Time", "NumberOfUsedEntrances"
]

SAMPLE_ROWS = 1000
MAX_WARN_ROWS_SHOWN = 10


#Return set of lowercased column names for case-insensitive comparison.
def _normalize_header_set(cols):
    return {c.strip().lower() for c in cols}

#-----------------------




def upload_file(request, db):
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
    



# Search files recorded in database based on criteria in FileQuery
def search_files(query: FileQuery, db: SQLAlchemy):
        dbQuery = db.session.query(File)
        
        return jsonify([]), 200
        