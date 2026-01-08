# Backend (Flask + SQLite + R) 

This is the backend for the visualization project. It's built with **Flask**, uses **SQLite** for storage, and integrates with **R scripts** to generate forecast data. The goal was to keep it lightweight but fully functional.

## Overview

The backend handles:

* Uploading data files and R scripts
* Storing metadata in SQLite
* Searching and listing uploaded files
* Running R forecasting scripts
* Returning chart-ready JSON data

---

## Features

###  File Upload System

There are two upload routes:

* `/api/upload/data` → for `.csv`, `.xlsx`, `.rda`, `.rds`
* `/api/upload/rscript` → for `.r` forecasting scripts

Uploaded files are stored under:

```
instance/store/<visualization_id>/data/
instance/store/<visualization_id>/rscripts/
```

Metadata is saved using SQLAlchemy models.

###  File Search & Listing

You can:

* List all stored files
* Search by `visualization_id`
* Fetch recent uploads for a visualization

###  Visualization API

Endpoints include:

* `/api/visualizations` – list all visualizations
* `/api/visualization/<id>` – get a single one
* `/api/visualizations/chart` – runs the R script and returns chart data

The POST body includes:

```json
{
  "id": 1,
  "start_date": "YYYY-MM-DD",
  "end_date": "YYYY-MM-DD",
  "spread": 0
}
```

###  R Integration

R scripts run using:

```
Rscript <script> <visualization_id> <start_date> <end_date>
```

They must output JSON like:

```json
[
  {
    "name": "Series1",
    "values": [ { "x": 1, "y": 20 }, ... ]
  }
]
```

The backend parses this into DTOs used by the frontend.

If something fails, a safe fallback is returned.

###  Database

* Uses SQLite: `sqlite:///visualizations.db`
* Tables are auto-created on startup
* Some sample visualizations and R scripts are seeded automatically

---

## Running the App

1. Install dependencies:

```bash
pip install -r requirements.txt
```

2. Start the server:

```bash
python app.py
```

3. Backend runs at:

```
http://localhost:5000/
```

---

## Notes

The backend is built to be simple, predictable, and easy to test. R integration is modular, so you can swap scripts or extend functionality without touching core logic.
