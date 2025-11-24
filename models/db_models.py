from datetime import datetime
from sqlalchemy import (
    Column, Integer, String, DateTime, Boolean, ForeignKey, Interval
)
from sqlalchemy.orm import relationship, declarative_base

Base = declarative_base()

class File(Base):
    __tablename__ = 'files'

    id = Column(Integer, primary_key=True, autoincrement=True)
    name = Column(String, nullable=False)
    file_path = Column(String, nullable=False)
    upload_time = Column(DateTime, default=datetime.utcnow)
    
    # Relationships
    data_file = relationship('DataFile', back_populates='file', uselist=False)
    r_script_file = relationship('RScriptFile', back_populates='file', uselist=False)
    
    def __init__(self, name: str, file_path: str):
        self.name = name
        self.file_path = file_path
        self.upload_time = datetime.now()


class Visualization(Base):
    __tablename__ = 'visualizations'

    id = Column(Integer, primary_key=True, autoincrement=True)
    name = Column(String, nullable=False)
    description = Column(String)
    prediction = Column(Boolean, default=False)

    # Relationships
    data_files = relationship('DataFile', back_populates='visualization')
    r_script_files = relationship('RScriptFile', back_populates='visualization')
    
    def __init__(self, name: str, description: str, prediction: bool = False):
        self.name = name
        self.description = description
        self.prediction = prediction


class DataFile(File):
    __tablename__ = 'data_files'

    id = Column(Integer, ForeignKey('files.id'), primary_key=True)
    timespan = Column(Interval, nullable=True)
    rows_count = Column(Integer, nullable=False)
    extension = Column(String, nullable=False)
    visualization_id = Column(Integer, ForeignKey('visualizations.id'))

    # Relationships
    visualization = relationship('Visualization', back_populates='data_files')
    file = relationship('File', back_populates='data_file')
    
    def __init__(self, name: str, file_path: str, rows_count: int, extension: str, visualization_id: int, timespan: datetime | None = None):
        super().__init__(name, file_path)
        self.rows_count = rows_count
        self.extension = extension
        self.visualization_id = visualization_id
        self.timespan = timespan


class RScriptFile(File):
    __tablename__ = 'r_script_files'

    id = Column(Integer, ForeignKey('files.id'), primary_key=True)
    visualization_id = Column(Integer, ForeignKey('visualizations.id'))

    # Relationships
    visualization = relationship('Visualization', back_populates='r_script_files')
    file = relationship('File', back_populates='r_script_file')
    
    def __init__(self, name: str, file_path: str, visualization_id: int):
        super().__init__(name, file_path)
        self.visualization_id = visualization_id
        