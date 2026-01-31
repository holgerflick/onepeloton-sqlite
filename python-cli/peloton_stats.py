#!/usr/bin/env python3
"""
Peloton Workout Statistics Analyzer

A command-line tool to analyze Peloton workout data from a SQLite database.
Provides 10 statistical queries with both interactive menu and direct execution modes.

Usage:
    python peloton_stats.py <database_file>
    python peloton_stats.py <database_file> -N

Examples:
    python peloton_stats.py workouts.sqlite
    python peloton_stats.py workouts.sqlite -2
"""

import argparse
import sqlite3
import sys
from pathlib import Path
from typing import List, Tuple, Optional


class QueryInfo:
    """Information about a statistical query."""
    
    def __init__(self, number: int, title: str, description: str, sql: str):
        self.number = number
        self.title = title
        self.description = description
        self.sql = sql


class PelotonStatsAnalyzer:
    """Main application class for analyzing Peloton workout statistics."""
    
    def __init__(self, db_path: str):
        self.db_path = db_path
        self.conn: Optional[sqlite3.Connection] = None
        self.queries: List[QueryInfo] = []
        self._initialize_queries()
    
    def _initialize_queries(self):
        """Initialize all available queries."""
        
        # Query 1: Overall Statistics
        self.queries.append(QueryInfo(
            number=1,
            title='Overall Workout Statistics',
            description='Total workouts, calories, distance, and time',
            sql="""
                SELECT 
                    COUNT(*) as total_workouts,
                    SUM(calories_burned) as total_calories,
                    ROUND(SUM(distance_km), 2) as total_distance_km,
                    SUM(length_minutes) as total_minutes,
                    ROUND(AVG(calories_burned), 1) as avg_calories_per_workout
                FROM workouts
            """
        ))
        
        # Query 2: Top Instructors
        self.queries.append(QueryInfo(
            number=2,
            title='Top 10 Instructors by Workout Count',
            description='Most frequent instructors with calorie stats',
            sql="""
                SELECT 
                    i.name,
                    COUNT(*) as workout_count,
                    SUM(w.calories_burned) as total_calories,
                    ROUND(AVG(w.calories_burned), 1) as avg_calories
                FROM workouts w
                JOIN classes c ON w.class_id = c.class_id
                JOIN instructors i ON c.instructor_id = i.instructor_id
                GROUP BY i.name
                ORDER BY workout_count DESC
                LIMIT 10
            """
        ))
        
        # Query 3: Workouts by Discipline
        self.queries.append(QueryInfo(
            number=3,
            title='Workouts by Fitness Discipline',
            description='Breakdown of all workout types',
            sql="""
                SELECT 
                    fd.name as discipline,
                    COUNT(*) as workout_count,
                    SUM(w.calories_burned) as total_calories,
                    ROUND(AVG(w.length_minutes), 1) as avg_length_minutes
                FROM workouts w
                JOIN fitness_disciplines fd ON w.discipline_id = fd.discipline_id
                GROUP BY fd.name
                ORDER BY workout_count DESC
            """
        ))
        
        # Query 4: Monthly Activity
        self.queries.append(QueryInfo(
            number=4,
            title='Monthly Workout Activity (Last 12 Months)',
            description='Workouts and calories by month',
            sql="""
                SELECT 
                    strftime('%Y-%m', workout_timestamp) as month,
                    COUNT(*) as workouts,
                    SUM(calories_burned) as total_calories,
                    ROUND(AVG(calories_burned), 1) as avg_calories
                FROM workouts
                WHERE workout_timestamp >= datetime('now', '-12 months')
                GROUP BY month
                ORDER BY month DESC
            """
        ))
        
        # Query 5: Day of Week Pattern
        self.queries.append(QueryInfo(
            number=5,
            title='Workout Pattern by Day of Week',
            description='Which days you work out most',
            sql="""
                SELECT 
                    CASE CAST(strftime('%w', workout_timestamp) AS INTEGER)
                        WHEN 0 THEN 'Sunday'
                        WHEN 1 THEN 'Monday'
                        WHEN 2 THEN 'Tuesday'
                        WHEN 3 THEN 'Wednesday'
                        WHEN 4 THEN 'Thursday'
                        WHEN 5 THEN 'Friday'
                        WHEN 6 THEN 'Saturday'
                    END as day_of_week,
                    COUNT(*) as workouts,
                    ROUND(AVG(calories_burned), 1) as avg_calories
                FROM workouts
                GROUP BY strftime('%w', workout_timestamp)
                ORDER BY CAST(strftime('%w', workout_timestamp) AS INTEGER)
            """
        ))
        
        # Query 6: Recent Workouts
        self.queries.append(QueryInfo(
            number=6,
            title='Last 10 Workouts',
            description='Your most recent workout sessions',
            sql="""
                SELECT 
                    datetime(w.workout_timestamp) as when_taken,
                    COALESCE(ct.title, 'Just Ride') as title,
                    COALESCE(i.name, 'N/A') as instructor,
                    w.length_minutes as duration,
                    w.calories_burned as calories
                FROM workouts w
                LEFT JOIN classes c ON w.class_id = c.class_id
                LEFT JOIN class_titles ct ON c.title_id = ct.title_id
                LEFT JOIN instructors i ON c.instructor_id = i.instructor_id
                ORDER BY w.workout_timestamp DESC
                LIMIT 10
            """
        ))
        
        # Query 7: Just Ride vs Classes
        self.queries.append(QueryInfo(
            number=7,
            title='Just Ride vs Classes Comparison',
            description='Compare free rides to structured classes',
            sql="""
                SELECT 
                    CASE 
                        WHEN class_id IS NULL THEN 'Just Ride'
                        ELSE 'Class'
                    END as workout_type,
                    COUNT(*) as workout_count,
                    ROUND(AVG(calories_burned), 1) as avg_calories,
                    ROUND(AVG(avg_watts), 1) as avg_watts,
                    ROUND(AVG(distance_km), 2) as avg_distance
                FROM workouts w
                JOIN fitness_disciplines fd ON w.discipline_id = fd.discipline_id
                WHERE fd.name = 'Cycling'
                GROUP BY workout_type
            """
        ))
        
        # Query 8: Best Performance Records
        self.queries.append(QueryInfo(
            number=8,
            title='Personal Records (20-min Cycling)',
            description='Top 5 highest calorie burns',
            sql="""
                SELECT 
                    w.calories_burned,
                    datetime(w.workout_timestamp) as workout_date,
                    COALESCE(ct.title, 'Just Ride') as title,
                    COALESCE(i.name, 'N/A') as instructor,
                    w.avg_watts
                FROM workouts w
                LEFT JOIN classes c ON w.class_id = c.class_id
                LEFT JOIN class_titles ct ON c.title_id = ct.title_id
                LEFT JOIN instructors i ON c.instructor_id = i.instructor_id
                JOIN fitness_disciplines fd ON w.discipline_id = fd.discipline_id
                WHERE fd.name = 'Cycling' AND w.length_minutes = 20
                ORDER BY w.calories_burned DESC
                LIMIT 5
            """
        ))
        
        # Query 9: Most Repeated Classes
        self.queries.append(QueryInfo(
            number=9,
            title='Most Repeated Classes',
            description="Classes you've taken multiple times",
            sql="""
                SELECT 
                    ct.title,
                    i.name as instructor,
                    COUNT(*) as times_taken,
                    ROUND(AVG(w.calories_burned), 1) as avg_calories
                FROM workouts w
                JOIN classes c ON w.class_id = c.class_id
                JOIN class_titles ct ON c.title_id = ct.title_id
                JOIN instructors i ON c.instructor_id = i.instructor_id
                GROUP BY c.class_id
                HAVING times_taken > 1
                ORDER BY times_taken DESC
                LIMIT 10
            """
        ))
        
        # Query 10: Live vs On-Demand
        self.queries.append(QueryInfo(
            number=10,
            title='Live vs On-Demand Comparison',
            description='Performance in live vs recorded classes',
            sql="""
                SELECT 
                    CASE 
                        WHEN is_live = 1 THEN 'Live'
                        WHEN is_live = 0 THEN 'On Demand'
                    END as class_type,
                    COUNT(*) as workout_count,
                    ROUND(AVG(calories_burned), 1) as avg_calories,
                    SUM(calories_burned) as total_calories
                FROM workouts
                WHERE is_live IS NOT NULL
                GROUP BY is_live
            """
        ))
    
    def connect(self):
        """Connect to the SQLite database."""
        try:
            self.conn = sqlite3.connect(self.db_path)
            self.conn.row_factory = sqlite3.Row
            print(f'Successfully connected to database: {self.db_path}')
            print()
        except sqlite3.Error as e:
            print(f'Error connecting to database: {e}', file=sys.stderr)
            sys.exit(1)
    
    def close(self):
        """Close the database connection."""
        if self.conn:
            self.conn.close()
    
    def display_menu(self):
        """Display the interactive menu."""
        print('═' * 60)
        print('        PELOTON WORKOUT STATISTICS ANALYZER')
        print('═' * 60)
        print()
        print('Available Queries:')
        print()
        
        for query in self.queries:
            print(f'[{query.number:2d}] {query.title}')
            print(f'     {query.description}')
            print()
        
        print('[ 0] Exit')
        print()
    
    def execute_query(self, query_num: int):
        """Execute a specific query and display results."""
        if query_num < 1 or query_num > len(self.queries):
            print(f'Invalid query number: {query_num}')
            return
        
        query_info = self.queries[query_num - 1]
        
        print()
        print('═' * 60)
        print(query_info.title)
        print('═' * 60)
        print()
        
        try:
            cursor = self.conn.cursor()
            cursor.execute(query_info.sql)
            
            rows = cursor.fetchall()
            
            if not rows:
                print('No data found.')
            else:
                # Get column names
                columns = [description[0] for description in cursor.description]
                
                # Calculate column widths
                col_widths = [len(col) for col in columns]
                for row in rows:
                    for i, value in enumerate(row):
                        val_str = str(value) if value is not None else 'N/A'
                        col_widths[i] = max(col_widths[i], len(val_str))
                
                # Ensure minimum width and add padding
                col_widths = [max(w, 10) for w in col_widths]
                
                # Print column headers
                header_parts = []
                for col, width in zip(columns, col_widths):
                    header_parts.append(col.ljust(width))
                print(' | '.join(header_parts))
                
                # Print separator
                separator_parts = ['─' * width for width in col_widths]
                print('─┼─'.join(separator_parts))
                
                # Print data rows
                for row in rows:
                    row_parts = []
                    for value, width in zip(row, col_widths):
                        val_str = str(value) if value is not None else 'N/A'
                        row_parts.append(val_str.ljust(width))
                    print(' | '.join(row_parts))
                
                print()
                print(f'Total rows: {len(rows)}')
            
            cursor.close()
            
        except sqlite3.Error as e:
            print(f'Error executing query: {e}', file=sys.stderr)
        
        print()
    
    def run_interactive_mode(self):
        """Run the application in interactive menu mode."""
        while True:
            self.display_menu()
            
            try:
                choice = input('Enter your choice (0-10): ').strip()
                
                if not choice:
                    continue
                
                choice_num = int(choice)
                
                if choice_num == 0:
                    print()
                    print('Thank you for using Peloton Stats Analyzer!')
                    break
                elif 1 <= choice_num <= len(self.queries):
                    self.execute_query(choice_num)
                    input('Press Enter to continue...')
                    print()
                else:
                    print(f'Invalid choice. Please enter a number between 0 and {len(self.queries)}')
                    print()
                    input('Press Enter to continue...')
                    print()
            
            except ValueError:
                print('Invalid input. Please enter a number.')
                print()
                input('Press Enter to continue...')
                print()
            except KeyboardInterrupt:
                print()
                print()
                print('Thank you for using Peloton Stats Analyzer!')
                break
            except EOFError:
                print()
                break


def show_usage():
    """Display usage information."""
    print('Peloton Workout Statistics Analyzer')
    print()
    print('Usage:')
    print('  python peloton_stats.py <database_file> [-N]')
    print()
    print('Arguments:')
    print('  database_file   Path to the SQLite database file (*.sqlite)')
    print('  -N              Optional: Query number (1-10) to run directly')
    print()
    print('Examples:')
    print('  python peloton_stats.py workouts.sqlite')
    print('  python peloton_stats.py workouts.sqlite -2')
    print('  python peloton_stats.py workouts.sqlite -5')
    print()
    print('If query number is not specified, an interactive menu will be displayed.')


def main():
    """Main entry point."""
    # Parse command line arguments
    parser = argparse.ArgumentParser(
        description='Analyze Peloton workout statistics from SQLite database',
        add_help=False
    )
    parser.add_argument('database', nargs='?', help='Path to SQLite database file')
    parser.add_argument('query', nargs='?', help='Query number to run (format: -N)')
    parser.add_argument('-h', '--help', action='store_true', help='Show help message')
    
    args = parser.parse_args()
    
    # Show help if requested or no arguments
    if args.help or not args.database:
        show_usage()
        sys.exit(0 if args.help else 1)
    
    # Validate database file
    db_path = Path(args.database)
    if not db_path.exists():
        print(f'Error: Database file not found: {db_path}', file=sys.stderr)
        sys.exit(1)
    
    if not db_path.suffix.lower() in ['.sqlite', '.db', '.sqlite3']:
        print(f'Warning: File does not have a typical SQLite extension: {db_path}')
    
    # Parse query number if provided
    query_num = None
    if args.query:
        if args.query.startswith('-'):
            try:
                query_num = int(args.query[1:])
                if query_num < 1 or query_num > 10:
                    print(f'Error: Query number must be between 1 and 10', file=sys.stderr)
                    sys.exit(1)
            except ValueError:
                print(f'Error: Invalid query number: {args.query}', file=sys.stderr)
                sys.exit(1)
        else:
            print(f'Error: Second parameter must be in format -N (e.g., -2, -5)', file=sys.stderr)
            show_usage()
            sys.exit(1)
    
    # Create analyzer and connect to database
    analyzer = PelotonStatsAnalyzer(str(db_path))
    analyzer.connect()
    
    try:
        if query_num:
            # Direct query execution mode
            analyzer.execute_query(query_num)
        else:
            # Interactive menu mode
            analyzer.run_interactive_mode()
    finally:
        analyzer.close()


if __name__ == '__main__':
    main()
