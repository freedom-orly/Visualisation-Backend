from datetime import datetime
from sqlalchemy import (
    Column, Integer, String, DateTime, Boolean, ForeignKey, Interval
)
from sqlalchemy.orm import relationship, declarative_base

Base = declarative_base()

class File(Base):
    __tablename__ = 'files'

    id = Column(Integer, primary_key=True)
    name = Column(String, nullable=False)
    file_path = Column(String, nullable=False)
    upload_time = Column(DateTime, default=datetime.utcnow)
    
    # Relationships
    data_file = relationship('DataFile', back_populates='file', uselist=False)
    r_script_file = relationship('RScriptFile', back_populates='file', uselist=False)


class Visualization(Base):
    __tablename__ = 'visualizations'

    id = Column(Integer, primary_key=True)
    name = Column(String, nullable=False)
    description = Column(String)
    prediction = Column(Boolean, default=False)

    # Relationships
    data_files = relationship('DataFile', back_populates='visualization')
    r_script_files = relationship('RScriptFile', back_populates='visualization')


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


class RScriptFile(File):
    __tablename__ = 'r_script_files'

    id = Column(Integer, ForeignKey('files.id'), primary_key=True)
    visualization_id = Column(Integer, ForeignKey('visualizations.id'))

    # Relationships
    visualization = relationship('Visualization', back_populates='r_script_files')
    file = relationship('File', back_populates='r_script_file')