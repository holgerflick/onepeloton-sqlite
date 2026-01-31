# Peloton Workout Statistics Analyzer - Delphi CLI

A command-line application written in Delphi 13 that analyzes Peloton workout data from a SQLite database using FireDAC.

## Overview

This application demonstrates how AI can be used to quickly generate sophisticated Delphi CLI applications with complex database connectivity. It reads the normalized SQLite database created by `import_peloton.py` and provides 10 interesting statistical queries about your workout history.

## Features

- **FireDAC-based SQLite connectivity** - Uses Delphi's built-in FireDAC components for robust database access
- **Two execution modes**:
  - **Interactive Menu Mode**: Browse and select from 10 pre-built queries
  - **Direct Query Mode**: Run a specific query via command-line parameter
- **10 Comprehensive Queries**:
  1. Overall Workout Statistics
  2. Top 10 Instructors by Workout Count
  3. Workouts by Fitness Discipline
  4. Monthly Workout Activity (Last 12 Months)
  5. Workout Pattern by Day of Week
  6. Last 10 Workouts
  7. Just Ride vs Classes Comparison
  8. Personal Records (20-min Cycling)
  9. Most Repeated Classes
  10. Live vs On-Demand Comparison

## Requirements

- **Delphi 13** (Athens) or later
- **FireDAC** (included with Delphi 13)
- **SQLite database** created by the `import_peloton.py` script

## Building the Application

### Using Delphi IDE

1. Open `PelotonStats.dproj` in Delphi 13
2. Select your target platform (Win32 or Win64)
3. Build the project (Project → Build PelotonStats)
4. The executable will be in `Win32\Debug` or `Win64\Debug` (or Release folders)

### Using Command Line (MSBuild)

```batch
# For 64-bit
msbuild PelotonStats.dproj /p:Config=Release /p:Platform=Win64

# For 32-bit
msbuild PelotonStats.dproj /p:Config=Release /p:Platform=Win32
```

## Usage

### Interactive Menu Mode

Run the application with just the database file path:

```batch
PelotonStats.exe path\to\hflickster_workouts.sqlite
```

This will display an interactive menu where you can select which query to run:

```
═══════════════════════════════════════════════════════════
        PELOTON WORKOUT STATISTICS ANALYZER
═══════════════════════════════════════════════════════════

Available Queries:

[ 1] Overall Workout Statistics
     Total workouts, calories, distance, and time

[ 2] Top 10 Instructors by Workout Count
     Most frequent instructors with calorie stats

...

[ 0] Exit

Enter your choice (0-10):
```

### Direct Query Mode

Run a specific query directly by passing its number with a `-` prefix:

```batch
# Show overall statistics
PelotonStats.exe workouts.sqlite -1

# Show top instructors
PelotonStats.exe workouts.sqlite -2

# Show monthly activity
PelotonStats.exe workouts.sqlite -4
```

## Example Output

### Query 1: Overall Workout Statistics
```
═══════════════════════════════════════════════════════════
Overall Workout Statistics
═══════════════════════════════════════════════════════════

total_workouts       | total_calories      | total_distance_km   | total_minutes       | avg_calories_per_workout
─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
1234                 | 345678              | 8765.43             | 24560               | 280.1

Total rows: 1
```

### Query 2: Top 10 Instructors
```
═══════════════════════════════════════════════════════════
Top 10 Instructors by Workout Count
═══════════════════════════════════════════════════════════

name                 | workout_count       | total_calories      | avg_calories
──────────────────────────────────────────────────────────────────────────────────
Robin Arzon          | 145                 | 42350               | 292.1
Denis Morton         | 123                 | 38720               | 314.8
...
```

## Database Schema

The application expects a SQLite database with the following normalized schema:

### Lookup Tables
- `instructors` - Instructor names
- `fitness_disciplines` - Types of workouts (Cycling, Running, etc.)
- `workout_types` - Class types (HIIT, Endurance, etc.)
- `class_titles` - Class title names

### Main Tables
- `classes` - Peloton's class catalog with instructor, title, and metadata
- `workouts` - Individual workout sessions with performance metrics

For full schema details, see `import_peloton.py`.

## Technical Details

### FireDAC Connection
The application uses FireDAC with the following configuration:
- **Driver**: SQLite
- **Connection Mode**: Read-Only
- **Foreign Keys**: Enabled

### Query Caching
All 10 queries are pre-loaded into memory at startup for fast execution.

### Output Formatting
Results are displayed in a formatted table with:
- Column headers
- Separator lines
- Aligned columns (20 characters wide)
- Row count summary

## Error Handling

The application includes comprehensive error handling for:
- Missing database files
- Invalid command-line parameters
- Database connection failures
- Query execution errors

## Files in This Project

- **PelotonStats.dpr** - Main program file (Delphi Project)
- **PelotonStats.dproj** - Project configuration file
- **README.md** - This file

## Creating the Database

Before using this application, you need to create the SQLite database from your Peloton CSV export:

```bash
# In the parent directory
python import_peloton.py hflickster_workouts.csv
```

This creates `hflickster_workouts.sqlite` which can then be analyzed with this Delphi application.

## Extending the Application

To add more queries:

1. Add a new `TQueryInfo` entry in the `Queries` array
2. Update the array size (e.g., `array[1..11]`)
3. Initialize the new query in `InitializeQueries`
4. Update menu display limits and validation

## License

This is demonstration code created to showcase AI-assisted Delphi development.

## Author

Generated with AI assistance as a proof-of-concept for rapid Delphi CLI application development.

## See Also

- `../import_peloton.py` - Python script that creates the database
- `../example_queries.sql` - SQL queries that inspired these reports
- `../README.md` - Main project documentation
