#!/usr/bin/env python3
"""
Import Peloton workout CSV data into a normalized SQLite database.

Usage:
    python import_peloton.py <input_csv_file>

The output database will have the same name as the input file but with .sqlite extension.
"""

import argparse
import csv
import sqlite3
import sys
from datetime import datetime
from pathlib import Path
from typing import Optional, Dict, Any
import re


class PelotonImporter:
    def __init__(self, db_path: str):
        self.conn = sqlite3.connect(db_path)
        self.conn.execute("PRAGMA foreign_keys = ON")
        self.cursor = self.conn.cursor()
        
        # Cache for lookups to avoid repeated queries
        self.instructor_cache: Dict[str, int] = {}
        self.discipline_cache: Dict[str, int] = {}
        self.type_cache: Dict[str, int] = {}
        self.title_cache: Dict[str, int] = {}
        self.class_cache: Dict[str, int] = {}  # keyed by class_timestamp
    
    def create_schema(self):
        """Create all database tables."""
        
        # Lookup tables
        self.cursor.execute("""
            CREATE TABLE IF NOT EXISTS instructors (
                instructor_id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT UNIQUE NOT NULL
            )
        """)
        
        self.cursor.execute("""
            CREATE TABLE IF NOT EXISTS fitness_disciplines (
                discipline_id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT UNIQUE NOT NULL
            )
        """)
        
        self.cursor.execute("""
            CREATE TABLE IF NOT EXISTS workout_types (
                type_id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT UNIQUE NOT NULL
            )
        """)
        
        self.cursor.execute("""
            CREATE TABLE IF NOT EXISTS class_titles (
                title_id INTEGER PRIMARY KEY AUTOINCREMENT,
                title TEXT UNIQUE NOT NULL
            )
        """)
        
        # Classes table (Peloton's class catalog)
        self.cursor.execute("""
            CREATE TABLE IF NOT EXISTS classes (
                class_id INTEGER PRIMARY KEY AUTOINCREMENT,
                class_timestamp TEXT UNIQUE NOT NULL,
                title_id INTEGER,
                instructor_id INTEGER,
                discipline_id INTEGER NOT NULL,
                type_id INTEGER,
                length_minutes INTEGER NOT NULL,
                FOREIGN KEY (title_id) REFERENCES class_titles(title_id),
                FOREIGN KEY (instructor_id) REFERENCES instructors(instructor_id),
                FOREIGN KEY (discipline_id) REFERENCES fitness_disciplines(discipline_id),
                FOREIGN KEY (type_id) REFERENCES workout_types(type_id)
            )
        """)
        
        # Workouts table (user's actual workout sessions)
        self.cursor.execute("""
            CREATE TABLE IF NOT EXISTS workouts (
                workout_id INTEGER PRIMARY KEY AUTOINCREMENT,
                class_id INTEGER,
                discipline_id INTEGER NOT NULL,
                workout_timestamp TEXT NOT NULL,
                is_live BOOLEAN,
                length_minutes INTEGER NOT NULL,
                total_output INTEGER,
                avg_watts INTEGER,
                avg_resistance REAL,
                avg_cadence_rpm INTEGER,
                avg_speed_kph REAL,
                distance_km REAL,
                calories_burned INTEGER,
                avg_heartrate REAL,
                avg_incline REAL,
                avg_pace_min_km REAL,
                FOREIGN KEY (class_id) REFERENCES classes(class_id),
                FOREIGN KEY (discipline_id) REFERENCES fitness_disciplines(discipline_id)
            )
        """)
        
        # Create indices for common queries
        self.cursor.execute("CREATE INDEX IF NOT EXISTS idx_workouts_timestamp ON workouts(workout_timestamp)")
        self.cursor.execute("CREATE INDEX IF NOT EXISTS idx_workouts_class ON workouts(class_id)")
        self.cursor.execute("CREATE INDEX IF NOT EXISTS idx_workouts_discipline ON workouts(discipline_id)")
        self.cursor.execute("CREATE INDEX IF NOT EXISTS idx_classes_timestamp ON classes(class_timestamp)")
        self.cursor.execute("CREATE INDEX IF NOT EXISTS idx_classes_instructor ON classes(instructor_id)")
        
        self.conn.commit()
    
    def parse_timestamp(self, timestamp_str: str) -> str:
        """
        Convert Peloton timestamps to ISO 8601 format with Z (UTC).
        
        Handles formats like:
        - "2021-03-30 15:15 (-04)" - offset format
        - "2021-04-02 18:50 (EST)" - timezone abbreviation
        """
        if not timestamp_str or not timestamp_str.strip():
            return None
        
        timestamp_str = timestamp_str.strip()
        
        # Regex to extract parts: "YYYY-MM-DD HH:MM (TZ)"
        match = re.match(r'^(\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2})\s*\(([^)]+)\)$', timestamp_str)
        if not match:
            raise ValueError(f"Cannot parse timestamp: {timestamp_str}")
        
        datetime_part = match.group(1)
        tz_part = match.group(2)
        
        # Parse the datetime
        dt = datetime.strptime(datetime_part, "%Y-%m-%d %H:%M")
        
        # Handle timezone
        if tz_part.startswith(('+', '-')):
            # Offset format like "-04" or "+05:30"
            tz_part = tz_part.strip()
            if ':' in tz_part:
                # Format: "+05:30"
                sign = 1 if tz_part[0] == '+' else -1
                hours, minutes = tz_part[1:].split(':')
                offset_minutes = sign * (int(hours) * 60 + int(minutes))
            else:
                # Format: "-04" (hours only)
                offset_hours = int(tz_part)
                offset_minutes = offset_hours * 60
        else:
            # Timezone abbreviation like "EST", "EDT", etc.
            # Common timezones
            tz_offsets = {
                'EST': -5 * 60,  # Eastern Standard Time
                'EDT': -4 * 60,  # Eastern Daylight Time
                'CST': -6 * 60,  # Central Standard Time
                'CDT': -5 * 60,  # Central Daylight Time
                'MST': -7 * 60,  # Mountain Standard Time
                'MDT': -6 * 60,  # Mountain Daylight Time
                'PST': -8 * 60,  # Pacific Standard Time
                'PDT': -7 * 60,  # Pacific Daylight Time
                'CET': 1 * 60,   # Central European Time
                'CEST': 2 * 60,  # Central European Summer Time
                'BST': 1 * 60,   # British Summer Time
                'GMT': 0,        # Greenwich Mean Time
                'UTC': 0,        # Coordinated Universal Time
            }
            offset_minutes = tz_offsets.get(tz_part.upper())
            if offset_minutes is None:
                raise ValueError(f"Unknown timezone abbreviation: {tz_part}")
        
        # Convert to UTC by subtracting the offset
        from datetime import timedelta
        dt_utc = dt - timedelta(minutes=offset_minutes)
        
        # Format as ISO 8601 with Z
        return dt_utc.strftime("%Y-%m-%dT%H:%M:%SZ")
    
    def parse_percentage(self, percent_str: str) -> Optional[float]:
        """Convert percentage string like "38%" to decimal like 0.38."""
        if not percent_str or not percent_str.strip():
            return None
        percent_str = percent_str.strip()
        if percent_str.endswith('%'):
            return float(percent_str[:-1]) / 100.0
        return float(percent_str) / 100.0
    
    def parse_int(self, value: str) -> Optional[int]:
        """Parse integer, return None if empty or invalid."""
        if not value or not value.strip():
            return None
        try:
            return int(float(value))  # Handle "0" vs "0.0"
        except ValueError:
            return None
    
    def parse_float(self, value: str) -> Optional[float]:
        """Parse float, return None if empty or invalid."""
        if not value or not value.strip():
            return None
        try:
            return float(value)
        except ValueError:
            return None
    
    def get_or_create_instructor(self, name: str) -> Optional[int]:
        """Get or create instructor, return ID. Returns None if name is empty."""
        if not name or not name.strip():
            return None
        
        name = name.strip()
        if name in self.instructor_cache:
            return self.instructor_cache[name]
        
        self.cursor.execute("SELECT instructor_id FROM instructors WHERE name = ?", (name,))
        row = self.cursor.fetchone()
        if row:
            instructor_id = row[0]
        else:
            self.cursor.execute("INSERT INTO instructors (name) VALUES (?)", (name,))
            instructor_id = self.cursor.lastrowid
        
        self.instructor_cache[name] = instructor_id
        return instructor_id
    
    def get_or_create_discipline(self, name: str) -> int:
        """Get or create fitness discipline, return ID."""
        if not name or not name.strip():
            raise ValueError("Discipline name cannot be empty")
        
        name = name.strip()
        if name in self.discipline_cache:
            return self.discipline_cache[name]
        
        self.cursor.execute("SELECT discipline_id FROM fitness_disciplines WHERE name = ?", (name,))
        row = self.cursor.fetchone()
        if row:
            discipline_id = row[0]
        else:
            self.cursor.execute("INSERT INTO fitness_disciplines (name) VALUES (?)", (name,))
            discipline_id = self.cursor.lastrowid
        
        self.discipline_cache[name] = discipline_id
        return discipline_id
    
    def get_or_create_type(self, name: str) -> Optional[int]:
        """Get or create workout type, return ID. Returns None if name is empty."""
        if not name or not name.strip():
            return None
        
        name = name.strip()
        if name in self.type_cache:
            return self.type_cache[name]
        
        self.cursor.execute("SELECT type_id FROM workout_types WHERE name = ?", (name,))
        row = self.cursor.fetchone()
        if row:
            type_id = row[0]
        else:
            self.cursor.execute("INSERT INTO workout_types (name) VALUES (?)", (name,))
            type_id = self.cursor.lastrowid
        
        self.type_cache[name] = type_id
        return type_id
    
    def get_or_create_title(self, title: str) -> Optional[int]:
        """Get or create class title, return ID. Returns None if title is empty."""
        if not title or not title.strip():
            return None
        
        title = title.strip()
        if title in self.title_cache:
            return self.title_cache[title]
        
        self.cursor.execute("SELECT title_id FROM class_titles WHERE title = ?", (title,))
        row = self.cursor.fetchone()
        if row:
            title_id = row[0]
        else:
            self.cursor.execute("INSERT INTO class_titles (title) VALUES (?)", (title,))
            title_id = self.cursor.lastrowid
        
        self.title_cache[title] = title_id
        return title_id
    
    def get_or_create_class(self, class_timestamp: str, title: str, instructor_name: str,
                           discipline: str, workout_type: str, length_minutes: int) -> Optional[int]:
        """
        Get or create a class, return ID.
        Returns None for Just Ride (when class_timestamp is empty).
        """
        if not class_timestamp or not class_timestamp.strip():
            return None  # Just Ride - no class
        
        # Check cache
        if class_timestamp in self.class_cache:
            return self.class_cache[class_timestamp]
        
        # Check if class exists
        self.cursor.execute("SELECT class_id FROM classes WHERE class_timestamp = ?", (class_timestamp,))
        row = self.cursor.fetchone()
        if row:
            class_id = row[0]
            self.class_cache[class_timestamp] = class_id
            return class_id
        
        # Create new class
        title_id = self.get_or_create_title(title)
        instructor_id = self.get_or_create_instructor(instructor_name)
        discipline_id = self.get_or_create_discipline(discipline)
        type_id = self.get_or_create_type(workout_type)
        
        self.cursor.execute("""
            INSERT INTO classes (class_timestamp, title_id, instructor_id, discipline_id, type_id, length_minutes)
            VALUES (?, ?, ?, ?, ?, ?)
        """, (class_timestamp, title_id, instructor_id, discipline_id, type_id, length_minutes))
        
        class_id = self.cursor.lastrowid
        self.class_cache[class_timestamp] = class_id
        return class_id
    
    def import_csv(self, csv_path: str):
        """Import workouts from CSV file."""
        print(f"Importing from {csv_path}...")
        
        with open(csv_path, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            
            row_count = 0
            for row in reader:
                row_count += 1
                try:
                    self.import_workout(row)
                except Exception as e:
                    print(f"Error importing row {row_count}: {e}", file=sys.stderr)
                    print(f"Row data: {row}", file=sys.stderr)
                    raise
                
                if row_count % 100 == 0:
                    print(f"  Processed {row_count} workouts...")
                    self.conn.commit()
        
        self.conn.commit()
        print(f"Successfully imported {row_count} workouts!")
        
        # Print statistics
        self.print_stats()
    
    def import_workout(self, row: Dict[str, str]):
        """Import a single workout from CSV row."""
        
        # Parse timestamps
        workout_timestamp = self.parse_timestamp(row['Workout Timestamp'])
        class_timestamp_raw = row.get('Class Timestamp', '').strip()
        class_timestamp = self.parse_timestamp(class_timestamp_raw) if class_timestamp_raw else None
        
        # Get basic fields
        live_on_demand = row.get('Live/On-Demand', '').strip()
        instructor_name = row.get('Instructor Name', '').strip()
        title = row.get('Title', '').strip()
        discipline = row.get('Fitness Discipline', '').strip()
        workout_type = row.get('Type', '').strip()
        length_minutes = self.parse_int(row.get('Length (minutes)', '0')) or 0
        
        # Determine if this is Just Ride
        is_just_ride = not live_on_demand or not class_timestamp
        
        # Get or create class (None for Just Ride)
        class_id = None
        if not is_just_ride:
            class_id = self.get_or_create_class(
                class_timestamp=class_timestamp,
                title=title,
                instructor_name=instructor_name,
                discipline=discipline,
                workout_type=workout_type,
                length_minutes=length_minutes
            )
        
        # Determine is_live (None for Just Ride)
        is_live = None
        if not is_just_ride:
            is_live = 1 if live_on_demand == 'Live' else 0
        
        # Get discipline_id (all workouts have a discipline)
        discipline_id = self.get_or_create_discipline(discipline)
        
        # Parse metrics
        total_output = self.parse_int(row.get('Total Output', ''))
        avg_watts = self.parse_int(row.get('Avg. Watts', ''))
        avg_resistance = self.parse_percentage(row.get('Avg. Resistance', ''))
        avg_cadence_rpm = self.parse_int(row.get('Avg. Cadence (RPM)', ''))
        avg_speed_kph = self.parse_float(row.get('Avg. Speed (kph)', ''))
        distance_km = self.parse_float(row.get('Distance (km)', ''))
        calories_burned = self.parse_int(row.get('Calories Burned', ''))
        avg_heartrate = self.parse_float(row.get('Avg. Heartrate', ''))
        avg_incline = self.parse_float(row.get('Avg. Incline', ''))
        avg_pace_min_km = self.parse_float(row.get('Avg. Pace (min/km)', ''))
        
        # Insert workout
        self.cursor.execute("""
            INSERT INTO workouts (
                class_id, discipline_id, workout_timestamp, is_live, length_minutes,
                total_output, avg_watts, avg_resistance, avg_cadence_rpm, avg_speed_kph,
                distance_km, calories_burned, avg_heartrate, avg_incline, avg_pace_min_km
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            class_id, discipline_id, workout_timestamp, is_live, length_minutes,
            total_output, avg_watts, avg_resistance, avg_cadence_rpm, avg_speed_kph,
            distance_km, calories_burned, avg_heartrate, avg_incline, avg_pace_min_km
        ))
    
    def print_stats(self):
        """Print database statistics."""
        print("\n" + "="*60)
        print("DATABASE STATISTICS")
        print("="*60)
        
        self.cursor.execute("SELECT COUNT(*) FROM workouts")
        print(f"Total workouts:        {self.cursor.fetchone()[0]}")
        
        self.cursor.execute("SELECT COUNT(*) FROM classes")
        print(f"Unique classes:        {self.cursor.fetchone()[0]}")
        
        self.cursor.execute("SELECT COUNT(*) FROM instructors")
        print(f"Instructors:           {self.cursor.fetchone()[0]}")
        
        self.cursor.execute("SELECT COUNT(*) FROM fitness_disciplines")
        print(f"Fitness disciplines:   {self.cursor.fetchone()[0]}")
        
        self.cursor.execute("SELECT COUNT(*) FROM workout_types")
        print(f"Workout types:         {self.cursor.fetchone()[0]}")
        
        self.cursor.execute("SELECT COUNT(*) FROM class_titles")
        print(f"Class titles:          {self.cursor.fetchone()[0]}")
        
        # Just Ride count
        self.cursor.execute("SELECT COUNT(*) FROM workouts WHERE class_id IS NULL")
        just_ride_count = self.cursor.fetchone()[0]
        print(f"Just Ride workouts:    {just_ride_count}")
        
        print("\nTop 5 instructors by workout count:")
        self.cursor.execute("""
            SELECT i.name, COUNT(*) as count
            FROM workouts w
            JOIN classes c ON w.class_id = c.class_id
            JOIN instructors i ON c.instructor_id = i.instructor_id
            GROUP BY i.name
            ORDER BY count DESC
            LIMIT 5
        """)
        for i, (name, count) in enumerate(self.cursor.fetchall(), 1):
            print(f"  {i}. {name}: {count} workouts")
        
        print("\nWorkouts by discipline:")
        self.cursor.execute("""
            SELECT d.name, COUNT(*) as count
            FROM workouts w
            JOIN fitness_disciplines d ON w.discipline_id = d.discipline_id
            GROUP BY d.name
            ORDER BY count DESC
        """)
        for name, count in self.cursor.fetchall():
            print(f"  {name}: {count} workouts")
        
        print("="*60 + "\n")
    
    def close(self):
        """Close database connection."""
        self.conn.close()


def main():
    parser = argparse.ArgumentParser(
        description='Import Peloton workout CSV data into a normalized SQLite database.',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument('csv_file', help='Path to the input CSV file')
    
    args = parser.parse_args()
    
    # Validate input file
    csv_path = Path(args.csv_file)
    if not csv_path.exists():
        print(f"Error: File not found: {csv_path}", file=sys.stderr)
        sys.exit(1)
    
    if not csv_path.suffix.lower() == '.csv':
        print(f"Error: Input file must be a CSV file", file=sys.stderr)
        sys.exit(1)
    
    # Create output database path
    db_path = csv_path.with_suffix('.sqlite')
    
    # Check if database already exists
    if db_path.exists():
        response = input(f"Database {db_path} already exists. Overwrite? (y/N): ")
        if response.lower() != 'y':
            print("Import cancelled.")
            sys.exit(0)
        db_path.unlink()
    
    # Import
    try:
        importer = PelotonImporter(str(db_path))
        importer.create_schema()
        importer.import_csv(str(csv_path))
        importer.close()
        
        print(f"\n✓ Database created successfully: {db_path}")
        
    except Exception as e:
        print(f"\n✗ Import failed: {e}", file=sys.stderr)
        if db_path.exists():
            db_path.unlink()
        sys.exit(1)


if __name__ == '__main__':
    main()
