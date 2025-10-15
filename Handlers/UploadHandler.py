import io
from flask import jsonify, url_for
from flask_sqlalchemy import SQLAlchemy
import pandas as pd
from requests import request
from models.db_models import File, DataFile, Visualization
from models.dto_models import FileQuery, FileUploadQuery
from pathlib import Path
from datetime import timedelta

REQUIRED_SALES_HEADERS = [
    "ReceiptDateTime", "ArticleId", "NetAmountExcl",
    "Quantity", "Article", "SubgroupId", "MaingroupId", "StoreId"
]

REQUIRED_VISITOR_HEADERS = [
    "AccessGroupId", "Date", "Time", "NumberOfUsedEntrances"
]

HEADERS_TO_ID = {
    1: REQUIRED_SALES_HEADERS,
    2: REQUIRED_VISITOR_HEADERS
}

SAMPLE_ROWS = 1000
MAX_WARN_ROWS_SHOWN = 10


#Return set of lowercased column names for case-insensitive comparison.
def _normalize_header_set(cols):
    return {c.strip().lower() for c in cols}

#-----------------------




def upload_file(query: FileUploadQuery, db: SQLAlchemy):
        
        file = query.file
        
        # vis = db.session.query(Visualization).get(query.visualization_id)
        # if not vis:
        #     return jsonify({"status": "rejected", "errors": [f"Visualization not found"]}), 404
        
        # Try read sample from file
        try:
            content = file.read()
            sample_buf = io.BytesIO(content)
        except Exception as e:
            return jsonify({"status": "rejected", "errors": [f"Failed to read uploaded file: {str(e)}"]}), 400
        
        # Check if we can read the headers
        try:
            sample_headers_df = pd.read_csv(io.BytesIO(content), nrows=0, sep=";")
            received_headers = list(sample_headers_df.columns)
        except Exception as e:
            return jsonify({"status": "rejected", "errors": [f"Unable to parse CSV headers: {str(e)}"]}), 400
        
        #Lowered headers file_name is used to see which file headers to use
        
        # Check if headers are missing
        # TODO: TEMPORARY
        required_headers = HEADERS_TO_ID.get(query.visualization_id)
        if not required_headers:
            return jsonify({"status": "rejected", "errors": [f"Unknown visualization ID: {query.visualization_id}"]}), 400
        
        if not required_headers == received_headers:
            missing = [h for h in required_headers if h not in received_headers]
            extra = [h for h in received_headers if h.strip().lower() not in required_headers]
            errors = []
            if missing:
                errors.append(f"Missing required columns: {', '.join(missing)}")
            if extra:
                errors.append(f"Unexpected columns: {', '.join(extra[:MAX_WARN_ROWS_SHOWN])}" + (f", and {len(extra) - MAX_WARN_ROWS_SHOWN} more." if len(extra) > MAX_WARN_ROWS_SHOWN else ""))
            return jsonify({"status": "rejected", "errors": errors}), 400
        
        # Read a sample of rows for content validation
        try:
            sample_df = pd.read_csv(io.BytesIO(content), dtype=str, nrows=SAMPLE_ROWS, sep=";")
        except Exception as e:
            return jsonify({"status": "rejected", "errors": [f"Failed to parse CSV sample rows: {str(e)}"]}), 400
        
        
        # If we got here, everything is fine. Save the file to disk.
        try:
            Path(f"./instance/store/{query.visualization_id}/data").mkdir(parents=True, exist_ok=True)
            file_path = f"./instance/store/{query.visualization_id}/data/{file.filename}"
            with open(file_path, "wb") as f:
                f.write(content)
            

            new_data_file = DataFile(
                name=file.filename, # type: ignore
                file_path=file_path,
                timespan= timedelta(days=1), # Placeholder, real timespan calculation needed # type: ignore
                rows_count=len(sample_df),
                extension=Path(file.filename).suffix, # type: ignore
                visualization_id=query.visualization_id,
            )
            
            db.session.add(new_data_file)
            db.session.commit()
        except Exception as e:
            return jsonify({"status": "rejected", "errors": [f"Failed to save file: {str(e)}"]}), 500

        #Return success marker.
        return jsonify({"status": "ok", "message": "File added successfully"}), 200
    



# Search files recorded in database based on criteria in FileQuery
def search_files(query: FileQuery, db: SQLAlchemy):
        # dbQuery = db.session.query(DataFile).filter(DataFile.name == f'%{query.query}%',
        #                                              DataFile.visualization_id == query.visualization_id,
        #                                              DataFile.upload_time == query.upload_time).limit(query.start)
        
        # results = dbQuery.all()

        #if no query return all files as list
        if query is None:
            return db.session.query(DataFile).all()
        #if query has id then search for file and return it
        file_obj = db.session.get(DataFile, query.visualization_id)
        if not file_obj:
            return jsonify("error: No file found"), 404
        
        return jsonify(file_obj), 200

        