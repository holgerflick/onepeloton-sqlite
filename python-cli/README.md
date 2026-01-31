# Peloton Workout Statistics Analyzer - Python CLI

A command-line application written in Python 3 that analyzes Peloton workout data from a SQLite database.

## Overview

This is the Python equivalent of the Delphi CLI application. It reads the normalized SQLite database created by `import_peloton.py` and provides 10 interesting statistical queries about your workout history.

## Features

- **Built-in SQLite support** - Uses Python's standard `sqlite3` module (no external dependencies)
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

- **Python 3.6+**
- **No external dependencies** - uses only standard library modules:
  - `sqlite3` (built-in)
  - `argparse` (built-in)
  - `pathlib` (built-in)

## Installation

No installation needed! Just ensure you have Python 3.6 or later:

```bash
python3 --version
```

Make the script executable (optional, Unix/Linux/macOS):

```bash
chmod +x peloton_stats.py
```

## Usage

### Interactive Menu Mode

Run the application with just the database file path:

```bash
python peloton_stats.py ../hflickster_workouts.sqlite
```

Or if executable:

```bash
./peloton_stats.py ../hflickster_workouts.sqlite
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

```bash
# Show overall statistics
python peloton_stats.py workouts.sqlite -1

# Show top instructors
python peloton_stats.py workouts.sqlite -2

# Show monthly activity
python peloton_stats.py workouts.sqlite -4
```

### Getting Help

```bash
python peloton_stats.py --help
```

## Example Output

### Query 1: Overall Workout Statistics
```
═══════════════════════════════════════════════════════════
Overall Workout Statistics
═══════════════════════════════════════════════════════════

total_workouts | total_calories | total_distance_km | total_minutes | avg_calories_per_workout
─────────────────────────────────────────────────────────────────────────────────────────────
1234           | 345678         | 8765.43           | 24560         | 280.1

Total rows: 1
```

### Query 2: Top 10 Instructors
```
═══════════════════════════════════════════════════════════
Top 10 Instructors by Workout Count
═══════════════════════════════════════════════════════════

name          | workout_count | total_calories | avg_calories
────────────────────────────────────────────────────────────
Robin Arzon   | 145           | 42350          | 292.1
Denis Morton  | 123           | 38720          | 314.8
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

For full schema details, see `../import_peloton.py`.

## Technical Details

### Architecture

The application uses an object-oriented design:

- **`QueryInfo` class**: Encapsulates query metadata (number, title, description, SQL)
- **`PelotonStatsAnalyzer` class**: Main application logic
  - Database connection management
  - Query execution
  - Result formatting
  - Interactive menu handling

### SQLite Connection

- Uses `sqlite3.Row` row factory for dictionary-like row access
- Read-only access to database
- Proper resource cleanup with try/finally blocks

### Output Formatting

Results are displayed in a formatted table with:
- Dynamic column width calculation
- Aligned columns with separators
- NULL value handling (displays as "N/A")
- Row count summary

### Error Handling

Comprehensive error handling for:
- Missing database files
- Invalid command-line parameters
- Database connection failures
- Query execution errors
- Keyboard interrupts (Ctrl+C)

## Comparison with Delphi Version

| Feature | Python | Delphi |
|---------|--------|--------|
| Lines of Code | ~470 | ~735 |
| External Dependencies | None | FireDAC (included) |
| Cross-platform | ✅ Yes | ❌ Windows only |
| Compilation | Not needed | Required |
| Startup Speed | Instant | Very fast |
| Memory Usage | Low | Very low |
| Performance | Excellent | Excellent |

Both implementations provide identical functionality with the same 10 queries.

## Files in This Project

- **peloton_stats.py** - Main Python script (470 lines)
- **README.md** - This file

## Creating the Database

Before using this application, you need to create the SQLite database from your Peloton CSV export:

```bash
# In the parent directory
python import_peloton.py hflickster_workouts.csv
```

This creates `hflickster_workouts.sqlite` which can then be analyzed with this Python application.

## Extending the Application

To add more queries:

1. Add a new `QueryInfo` object in `_initialize_queries()` method
2. Update the help text if needed
3. The application automatically supports any number of queries

Example:

```python
self.queries.append(QueryInfo(
    number=11,
    title='Your New Query Title',
    description='Description of what it does',
    sql="""
        SELECT ...
        FROM ...
    """
))
```

## Running from Anywhere

Add the script to your PATH or create an alias:

```bash
# Add to ~/.bashrc or ~/.zshrc
alias peloton='python ~/tinker/onepeloton-sqlite/python-cli/peloton_stats.py'

# Then use it from anywhere:
peloton ~/data/workouts.sqlite -2
```

## Performance Notes

- The application is fast even with large databases (thousands of workouts)
- All queries use proper indexes defined by `import_peloton.py`
- Results are fetched and formatted efficiently
- Memory usage scales linearly with result set size

## Troubleshooting

### "Database is locked" error

Make sure no other program has the database file open (especially the import script or Prisma Studio).

### Unicode display issues

If special characters don't display correctly, ensure your terminal supports UTF-8:

```bash
export LANG=en_US.UTF-8
```

### Permission denied (Unix/Linux/macOS)

Make the script executable:

```bash
chmod +x peloton_stats.py
```

## License

This is demonstration code created to showcase AI-assisted Python development.

## Author

Generated with AI assistance as a proof-of-concept for rapid CLI application development.

## See Also

- `../import_peloton.py` - Python script that creates the database
- `../example_queries.sql` - SQL queries that inspired these reports
- `../delphi-cli/` - Equivalent Delphi implementation
- `../README.md` - Main project documentation
