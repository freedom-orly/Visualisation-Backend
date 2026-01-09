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
    chart_type = Column(String, nullable=True)
    prediction = Column(Boolean, default=False)
   
    

    # Relationships
    data_files = relationship('DataFile', back_populates='visualization')
    r_script_files = relationship('RScriptFile', back_populates='visualization')
    fields = relationship('VisualizationInputField', backref='visualization', cascade='all, delete-orphan')
    
    def __init__(self, name: str, description: str, prediction: bool = False, chart_type: str | None = None):
        self.name = name
        self.description = description
        self.prediction = prediction
        self.chart_type = chart_type
        
class VisualizationInputField(Base):
    __tablename__ = 'visualization_inputfields'

    id = Column(Integer, primary_key=True, autoincrement=True)
    visualization_id = Column(Integer, ForeignKey('visualizations.id'))
    field_name = Column(String, nullable=False)
    field_label = Column(String, nullable=True)
    field_type = Column(String, nullable=False)
    options = Column(String, nullable=True)
    default_value = Column(String, nullable=True)
    required = Column(Boolean, default=True)

    def __init__(self, visualization_id: int, field_name: str, field_type: str, required: bool = True, field_label: str | None = None,
                 options: str | None = None, default_value: str | None = None):
        self.visualization_id = visualization_id
        self.field_name = field_name
        self.field_type = field_type
        self.required = required
        self.field_label = field_label
        self.options = options
        self.default_value = default_value
        


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
        