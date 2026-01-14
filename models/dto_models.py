from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import List, Optional
from werkzeug.datastructures import FileStorage


@dataclass
class DataPoint:
    x: object
    y: object

@dataclass
class chartEntry:
    name: str
    values: list[DataPoint]


@dataclass
class ChartDTO:
    visualization_id: int
    name: str
    chart_type: Optional[str]
    # start_date: datetime
    # end_date: datetime
    prediction: bool
    values: list[chartEntry]
    # spread: int
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

@dataclass
class ChartQuery:
    id: int
     #inputs?: {[key: string]: any};
    inputs: Optional[dict]
    
@dataclass
class VisualizationInputFieldDTO:
    id: int
    visualization_id: int
    field_name: str
    field_type: str
    field_label: Optional[str]
    options: Optional[List[str]]
    default_value: Optional[str]
    required: bool


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



