from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import List, Optional
from werkzeug.datastructures import FileStorage


@dataclass
class ChartDTO:
    visualization_id: int
    name: str
    start_date: datetime
    end_date: datetime
    prediction: bool
    values: List[List[int]]
    spread: int
@dataclass
class FileUpdate:
    id: int
    name: str
    time: datetime  

@dataclass
class VisualizationDTO:
    id: int
    name: str
    description: str
    is_prediction: bool
    last_updates: List[FileUpdate]

@dataclass
class ChartQuery:
    id: int
    start_date: datetime
    end_date: datetime
    spread: int


@dataclass
class FileQuery:
    visualization_id: int
    start: int
    query: str
    timespan: Optional[timedelta]
    extension: str
    
@dataclass
class FileUploadQuery:
    file: FileStorage
    visualization_id: int

@dataclass
class FileDTO:
    visualization_id: int
    id: int
    name: str
    file_path: str
    upload_time: str  #ISO time
    download_url: str 

@dataclass
class FilePage:
    start: int
    count: int
    query: FileQuery
    files: List[FileDTO]



