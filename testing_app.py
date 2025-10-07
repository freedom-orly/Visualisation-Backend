from flask import Flask, request, jsonify, Response
import pandas as pd
import io
import html

app = Flask(__name__)
app.config["MAX_CONTENT_LENGTH"] = 100 * 1024 * 1024  # 100 MB limit

REQUIRED_SALES_HEADERS = [
    "ReceiptDateTime", "ArticleId", "NetAmountExcl",
    "Quantity", "Article", "SubgroupId", "MaingroupId", "StoreId"
]

SAMPLE_ROWS = 1000
MAX_WARN_ROWS_SHOWN = 10

#-------Helpers---------
def _normalize_header_set(cols):
    return {c.strip().lower() for c in cols}

# -------------------------
# Your existing endpoint (unchanged logic)
# -------------------------
@app.route("/api/upload/sales", methods=["POST"])
def upload_sales():
    file = request.files.get("file")
    if not file:
        return jsonify({"status": "rejected", "errors": ["No file provided in 'file' field."]}), 400

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

    # Lowered headers
    req_lower = _normalize_header_set(REQUIRED_SALES_HEADERS)
    rec_lower = _normalize_header_set(received_headers)


    # Check if headers are missing
    missing = [h for h in REQUIRED_SALES_HEADERS if h.strip().lower() not in rec_lower]
    if missing:
        return jsonify({
            "status": "rejected",
            "errors": [f"Missing required columns: {missing}. Received: {received_headers}"]
        }), 400

    # Read a sample of rows for content validation
    try:
        sample_df = pd.read_csv(io.BytesIO(content), dtype=str, nrows=SAMPLE_ROWS, sep=";")
    except Exception as e:
        return jsonify({"status": "rejected", "errors": [f"Failed to parse CSV sample rows: {str(e)}"]}), 400

    # Return success marker.
    return jsonify({"status": "ok", "message": "Validation passed on sample rows."}), 200

# -------------------------
# Simple test UI
# -------------------------
VALID_SAMPLE_CSV = """ReceiptDateTime;ArticleId;NetAmountExcl;Quantity;Article;SubgroupId;MaingroupId;StoreId
2025-09-01 10:00:00;1001;12.50;1;Widget;10;1;StoreA
2025-09-01 11:00:00;1002;5.00;2;Gizmo;10;1;StoreA
"""

INVALID_SAMPLE_CSV = """ReceiptDateTime;ArticleId;NetAmountExcl;Quantity;Article;SubgroupId;MaingroupId
2025-09-01 10:00:00;1001;not_a_number;1;Widget;10;1
"""

@app.route("/test-upload", methods=["GET"])
def test_upload_page():
    # Minimal HTML page with JS to upload file or sample CSVs and display results.
    page = f"""
<!doctype html>
<html>
<head>
  <meta charset="utf-8"/>
  <title>Sales Upload Tester</title>
  <style>
    body{{font-family:system-ui,Segoe UI,Roboto,Arial; padding:20px; max-width:900px}}
    .box{{padding:10px;border:1px solid #ddd;margin:10px 0;border-radius:6px}}
    pre{{background:#f8f8f8;padding:10px;border-radius:6px;overflow:auto}}
    button{{padding:8px 12px;margin-right:8px}}
  </style>
</head>
<body>
  <h2>Sales CSV Validation Tester</h2>
  <p>Pick a CSV file or use one of the sample CSVs below. Click "Run checks" to POST to <code>/api/upload/sales</code>.</p>
  <div class="box">
    <label>Upload CSV file: <input id="fileInput" type="file" accept=".csv"/></label>
    <br/><br/>
    <button id="runBtn">Run checks</button>
    <button id="validBtn">Use valid sample</button>
    <button id="invalidBtn">Use invalid sample</button>
    <button id="clearBtn">Clear</button>
  </div>

  <div class="box">
    <h4>Result</h4>
    <div id="result"><em>No test performed yet.</em></div>
  </div>

<script>
const validCsv = `{html.escape(VALID_SAMPLE_CSV)}`;
const invalidCsv = `{html.escape(INVALID_SAMPLE_CSV)}`;

function showResult(text, ok=true) {{
  const el = document.getElementById('result');
  el.innerHTML = '<pre>' + text + '</pre>';
  el.style.borderLeft = ok ? '6px solid #2ecc71' : '6px solid #e74c3c';
}}

async function postCsvContent(csvContent) {{
  const blob = new Blob([csvContent], {{ type: 'text/csv' }});
  const fd = new FormData();
  fd.append('file', blob, 'upload.csv');

  try {{
    const resp = await fetch('/api/upload/sales', {{
      method: 'POST',
      body: fd
    }});
    const text = await resp.text();
    let pretty;
    try {{
      pretty = JSON.stringify(JSON.parse(text), null, 2);
    }} catch(e) {{
      pretty = text;
    }}
    showResult('HTTP ' + resp.status + '\\n' + pretty, resp.ok);
  }} catch (err) {{
    showResult('Network error: ' + err, false);
  }}
}}

document.getElementById('runBtn').addEventListener('click', async () => {{
  const input = document.getElementById('fileInput');
  if (!input.files || input.files.length === 0) {{
    showResult('No file chosen. Click a sample button or choose a file first.', false);
    return;
  }}
  const file = input.files[0];
  const reader = new FileReader();
  reader.onload = async (e) => {{
    const text = e.target.result;
    await postCsvContent(text);
  }};
  reader.readAsText(file);
}});

document.getElementById('validBtn').addEventListener('click', async () => {{
  await postCsvContent(validCsv);
}});

document.getElementById('invalidBtn').addEventListener('click', async () => {{
  await postCsvContent(invalidCsv);
}});

document.getElementById('clearBtn').addEventListener('click', () => {{
  document.getElementById('result').innerHTML = '<em>Cleared</em>';
  document.getElementById('fileInput').value = '';
}});
</script>
</body>
</html>
"""
    return Response(page, mimetype="text/html")

if __name__ == "__main__":
    app.run(debug=True)
