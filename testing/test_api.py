import pytest
import io
import json
from app import app, db
from models.db_models import Visualization

#CONFIGURATION & FIXTURES
@pytest.fixture
def client():
    "Sets up a temporary test client and an in-memory database."
    app.config['TESTING'] = True
    app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///:memory:' 
    
    with app.test_client() as client:
        with app.app_context():
            db.create_all()
            #Add a dummy visualization for testing
            vis = Visualization(name="Test Vis", description="Unit Test", prediction=False, chart_type="line")
            db.session.add(vis)
            db.session.commit()
        yield client

#UNIT TESTS

def test_get_visualizations_200(client):
    "Test if we can fetch the list of visualizations."
    response = client.get('/api/visualizations')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert len(data) > 0
    assert data[0]['name'] == "Sales Data History"

def test_upload_missing_file_400(client):
    "Test upload endpoint rejects requests without a file."
    response = client.post('/api/upload/data', data={
        'visualization_id': 1
    })
    assert response.status_code == 400
    assert "No file provided" in json.loads(response.data)['errors'][0]

def test_upload_valid_csv_200(client):
    "Test uploading a valid CSV file."
    #Create a fake CSV file
    data = {
        'file': (io.BytesIO(b"col1;col2\n1;2"), 'test_data.csv'),
        'visualization_id': 1
    }
    response = client.post('/api/upload/data', data=data, content_type='multipart/form-data')
    
    assert response.status_code == 200
    assert json.loads(response.data)['status'] == 'ok'

def test_search_files_valid(client):
    "Test searching for files associated with a visualization."

    query = {
        "visualization_id": 1, 
        "start": 0, 
        "query": "", 
        "extension": ".csv"
    }
    response = client.post('/api/data/search', data=json.dumps(query))
    assert response.status_code == 200
    assert isinstance(json.loads(response.data), list)

def test_chart_endpoint_missing_input_400(client):
    "Test chart generation fail, with bad input."
    #Send broken JSON (just a bracket)
    response = client.post('/api/visualizations/chart', data="{")
    assert response.status_code == 400