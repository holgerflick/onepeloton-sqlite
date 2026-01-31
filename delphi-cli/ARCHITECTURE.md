# Peloton Stats - Delphi CLI

A command-line application for analyzing Peloton workout data stored in SQLite databases.

## Architecture

The application has been refactored into modular units for better maintainability:

### Units

#### `PelotonTypes.pas`
- Contains shared type definitions
- `TQueryInfo`: Record type for query information (Number, Title, Description, SQL)

#### `QueryManager.pas`
- Handles loading and managing queries from JSON configuration
- **Key Methods:**
  - `Create(ConfigPath)`: Loads queries from JSON file
  - `GetQueryCount`: Returns total number of queries
  - `GetQuery(Index)`: Gets query by array index
  - `GetQueryByNumber(QueryNum)`: Gets query by its number
  - `QueryNumberExists(QueryNum)`: Checks if query number is valid
  - `GetAllQueries`: Returns all queries as array

#### `DatabaseManager.pas`
- Manages SQLite database connections and query execution
- **Key Methods:**
  - `Create(DatabasePath)`: Initializes database connection
  - `Connect`: Establishes database connection
  - `Disconnect`: Closes database connection
  - `ExecuteQuery(SQL)`: Executes SQL and returns TFDQuery result
  - `IsConnected`: Checks connection status

#### `UIManager.pas`
- Handles all user interface and display operations
- **Key Methods:**
  - `Create(QueryManager)`: Initializes with query manager reference
  - `ShowUsage`: Displays command-line usage information
  - `DisplayMenu`: Shows interactive menu with available queries
  - `DisplayQueryResults`: Formats and displays query results
  - `ShowErrorMessage`: Displays error messages
  - `ShowSuccessMessage`: Displays success messages

#### `PelotonStats.dpr` (Main Program)
- Coordinates all components
- Handles command-line argument parsing
- Manages application flow (interactive vs direct query mode)
- Proper resource cleanup with try-finally blocks

## Configuration

Queries are configured in `queries.json`:

```json
{
  "queries": [
    {
      "number": 1,
      "title": "Query Title",
      "description": "Brief description",
      "sql": "SELECT statement here"
    }
  ]
}
```

## Usage

```bash
# Interactive mode
PelotonStats workouts.sqlite

# Direct query execution
PelotonStats workouts.sqlite -2
```

## Benefits of Refactoring

1. **Separation of Concerns**: Each unit has a single, well-defined responsibility
2. **Maintainability**: Easy to locate and modify specific functionality
3. **Testability**: Units can be tested independently
4. **Reusability**: Managers can be reused in other projects
5. **Readability**: Main program is concise and clear
6. **Error Handling**: Centralized in appropriate managers
7. **Resource Management**: Proper cleanup with destructor patterns

## Dependencies

- FireDAC (Database connectivity)
- SQLite driver
- Delphi RTL (System.SysUtils, System.JSON, etc.)
