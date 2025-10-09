from flask import Flask, request, jsonify
import pandas as pd
import io

from controllers import UploadController

app = Flask(__name__)
app.config["MAX_CONTENT_LENGTH"] = 100 * 1024 * 1024  # 100 MB limit

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
    UploadController.upload_file(req=request)

if __name__ == '__main__':
    
    app.run(debug=True)