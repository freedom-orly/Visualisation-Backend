from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import List, Optional


@dataclass
class ChartDTO:
    visualization_id: int
    name: str
    timespan: Optional[timedelta]
    prediction: bool
    values: List[List[int]]
    spread: Optional[timedelta]


@dataclass
class VisualizationDTO:
    id: int
    name: str
    description: str
    is_prediction: bool
    last_updates: List[datetime]

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
class FilePage:
    start: int
    count: int
    query: FileQuery
    files: List['File']


@dataclass
class File:
    visualization_id: int
    id: int
    name: str
    file_path: str
    upload_time: str  #ISO time
    download_url: str = None