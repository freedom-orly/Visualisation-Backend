# test_app.py
# Place this file in the same directory as your app.py (which must expose `app = Flask(...)`)
# Then run: python test_app.py
from flask import Response
import html
import re

# import the existing Flask app instance from your app.py
# make sure your app.py defines `app = Flask(__name__)` at module top-level
from app import app

# --- Minimal sample CSVs for quick testing (optional) ---
VALID_SALES = """ReceiptDateTime;ArticleId;NetAmountExcl;Quantity;Article;SubgroupId;MaingroupId;StoreId
2025-09-01 10:00:00;1001;12.50;1;Widget;10;1;StoreA
2025-09-01 11:00:00;1002;5.00;2;Gizmo;10;1;StoreA
"""

INVALID_SALES = """ReceiptDateTime;ArticleId;NetAmountExcl;Quantity;Article;SubgroupId;MaingroupId
2025-09-01 10:00:00;1001;not_a_number;1;Widget;10;1
"""

VALID_VISITORS = """AccessGroupId;Date;Time;NumberOfUsedEntrances
AG1;2025-09-01;10:00:00;5
AG2;2025-09-01;11:00:00;3
"""

INVALID_VISITORS = """AccessGroupId;Date;Time
AG1;2025-09-01;10:00:00
"""

# --- The test UI route ---
@app.route("/test-upload", methods=["GET"])
def test_upload_page():
    page = f"""
<!doctype html>
<html>
<head>
  <meta charset="utf-8"/>
  <title>Multi-file Validation Tester</title>
  <style>
    body{{font-family:system-ui,Segoe UI,Roboto,Arial; padding:20px; max-width:1100px}}
    .box{{padding:10px;border:1px solid #ddd;margin:10px 0;border-radius:6px}}
    table{{width:100%;border-collapse:collapse}}
    th,td{{padding:6px 8px;border-bottom:1px solid #eee;text-align:left;vertical-align:top}}
    pre{{background:#f8f8f8;padding:10px;border-radius:6px;overflow:auto;max-height:240px}}
    button{{padding:8px 12px;margin-right:8px}}
    select{{padding:6px}}
    .status-ok{{border-left:6px solid #2ecc71;padding-left:8px}}
    .status-bad{{border-left:6px solid #e74c3c;padding-left:8px}}
  </style>
</head>
<body>
  <h2>Multi-file Validation Tester</h2>
  <p>Select one or more CSV files and choose a schema for each file. Then click <strong>Run validations</strong>.</p>

  <div class="box">
    <label>Select files: <input id="files" type="file" multiple accept=".csv,text/csv" /></label>
    <button id="runAll">Run validations</button>
    <button id="clear">Clear</button>
    <span style="margin-left:12px;color:#666">Schemas supported: <code>sales</code>, <code>visitors</code></span>
  </div>

  <div class="box">
    <h4>Files to test</h4>
    <table id="fileTable">
      <thead><tr><th>Filename</th><th>Schema</th><th>Action</th><th>Result</th></tr></thead>
      <tbody></tbody>
    </table>
  </div>

  <div class="box">
    <h4>Quick sample buttons</h4>
    <button id="useValidSales">Post valid sales sample</button>
    <button id="useInvalidSales">Post invalid sales sample</button>
    <button id="useValidVisitors">Post valid visitors sample</button>
    <button id="useInvalidVisitors">Post invalid visitors sample</button>
  </div>

<script>
const supportedSchemas = ['sales','visitors'];

function guessSchemaFromName(name) {{
  name = name.toLowerCase();
  if (name.includes('sale') || name.includes('receip')) return 'sales';
  if (name.includes('visit') || name.includes('visitor')) return 'visitors';
  return 'sales';
}}

function escapeHtml(s) {{
  return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}}

function addFiles(fileList) {{
  const tbody = document.querySelector('#fileTable tbody');
  for (const f of fileList) {{
    const tr = document.createElement('tr');
    const fnameTd = document.createElement('td');
    fnameTd.textContent = f.name;
    const schemaTd = document.createElement('td');
    const select = document.createElement('select');
    for (const s of supportedSchemas) {{
      const opt = document.createElement('option'); opt.value = s; opt.text = s;
      select.appendChild(opt);
    }}
    select.value = guessSchemaFromName(f.name);
    schemaTd.appendChild(select);

    const actionTd = document.createElement('td');
    const runBtn = document.createElement('button');
    runBtn.textContent = 'Run';
    runBtn.addEventListener('click', () => runSingle(f, select.value, tr));
    actionTd.appendChild(runBtn);

    const resultTd = document.createElement('td');
    resultTd.innerHTML = '<em>Not run</em>';

    tr._file = f; // attach file to row for later bulk run
    tr.appendChild(fnameTd);
    tr.appendChild(schemaTd);
    tr.appendChild(actionTd);
    tr.appendChild(resultTd);
    tbody.appendChild(tr);
  }}
}}

document.getElementById('files').addEventListener('change', (ev) => {{
  const files = ev.target.files;
  if (files && files.length) addFiles(files);
}});

document.getElementById('clear').addEventListener('click', () => {{
  document.querySelector('#fileTable tbody').innerHTML = '';
  document.getElementById('files').value = '';
}});

async function runSingle(file, schema, trRow) {{
  const resultTd = trRow.querySelector('td:last-child');
  resultTd.innerHTML = '...running';
  const fd = new FormData();
  fd.append('file', file, file.name);
  fd.append('file_name', schema);

  try {{
    const resp = await fetch('/api/upload/data', {{
      method: 'POST',
      body: fd
    }});
    const text = await resp.text();
    let pretty;
    try {{ pretty = JSON.stringify(JSON.parse(text), null, 2); }} catch(e) {{ pretty = text; }}
    resultTd.innerHTML = `<pre>${{escapeHtml('HTTP ' + resp.status + '\\n' + pretty)}}</pre>`;
    resultTd.className = resp.ok ? 'status-ok' : 'status-bad';
  }} catch (err) {{
    resultTd.innerHTML = `<pre>${{escapeHtml('Network error: ' + err)}}</pre>`;
    resultTd.className = 'status-bad';
  }}
}}

document.getElementById('runAll').addEventListener('click', async () => {{
  const rows = Array.from(document.querySelectorAll('#fileTable tbody tr'));
  for (const r of rows) {{
    const f = r._file;
    const schema = r.querySelector('select').value;
    // await each one sequentially, per your request "call file_validation once per file uploaded"
    await runSingle(f, schema, r);
  }}
}});

// Quick sample posting
async function postSample(text, schema) {{
  const blob = new Blob([text], {{ type: 'text/csv' }});
  const fd = new FormData();
  fd.append('file', blob, 'sample.csv');
  fd.append('file_name', schema);
  try {{
    const resp = await fetch('/api/upload/data', {{ method: 'POST', body: fd }});
    const txt = await resp.text();
    let pretty;
    try {{ pretty = JSON.stringify(JSON.parse(txt), null, 2); }} catch(e) {{ pretty = txt; }}
    alert('HTTP ' + resp.status + '\\n' + pretty);
  }} catch (err) {{
    alert('Network error: ' + err);
  }}
}}

document.getElementById('useValidSales').addEventListener('click', () => postSample(`{html.escape(VALID_SALES)}`, 'sales'));
document.getElementById('useInvalidSales').addEventListener('click', () => postSample(`{html.escape(INVALID_SALES)}`, 'sales'));
document.getElementById('useValidVisitors').addEventListener('click', () => postSample(`{html.escape(VALID_VISITORS)}`, 'visitors'));
document.getElementById('useInvalidVisitors').addEventListener('click', () => postSample(`{html.escape(INVALID_VISITORS)}`, 'visitors'));

</script>
</body>
</html>
"""
    return Response(page, mimetype="text/html")

# --- Run the app (re-uses your app instance) ---
if __name__ == "__main__":
    # If you normally run app.py directly, run this script instead so the test page is available:
    # python test_app.py
    app.run(debug=True)
