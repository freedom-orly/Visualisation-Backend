from flask import Flask, request, jsonify
import pandas as pd
import io

app = Flask(__name__)
app.config["MAX_CONTENT_LENGTH"] = 100 * 1024 * 1024  # 100 MB limit

REQUIRED_SALES_HEADERS = [
    "ReceiptDateTime", "ArticleId", "NetAmountExcl",
    "Quantity", "Article", "SubgroupId", "MaingroupId", "StoreId"
]

SAMPLE_ROWS = 1000
MAX_WARN_ROWS_SHOWN = 10

#-------Helpers---------

#Return set of lowercased column names for case-insensitive comparison.
def _normalize_header_set(cols):
    return {c.strip().lower() for c in cols}

@app.route('/')
def hello_world():
    return 'Hello World'

@app.route("/api/upload/sales", methods=["POST"])
def upload_sales():
    file = request.files.get("file")
    if not file:
        return jsonify({"status": "rejected", "errors": ["No file provided in 'file' field."]}), 400
    
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
    
    #Lowered headers
    req_lower = _normalize_header_set(REQUIRED_SALES_HEADERS)
    rec_lower = _normalize_header_set(received_headers)

    #Check if headers are missing
    missing = [h for h in REQUIRED_SALES_HEADERS if h.strip().lower() not in rec_lower]
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

if __name__ == '__main__':
    
    app.run()