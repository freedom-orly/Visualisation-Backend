from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import List, Optional
from werkzeug.datastructures import FileStorage


@dataclass
class ChartDTO:
    visualization_id: int
    name: str
    timespan: Optional[timedelta]
    prediction: bool
    values: List[List[int]]
    spread: Optional[timedelta]
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
    timespan: Optional[timedelta]
    spread: Optional[timedelta]


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



