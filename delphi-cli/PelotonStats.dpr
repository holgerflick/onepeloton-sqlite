program PelotonStats;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  FireDAC.ConsoleUI.Wait;

type
  TQueryInfo = record
    Number: Integer;
    Title: string;
    Description: string;
    SQL: string;
  end;

var
  Connection: TFDConnection;
  Query: TFDQuery;
  DBPath: string;
  QueryNumber: Integer;
  ConfigPath: string;
  Queries: TArray<TQueryInfo>;
  QueryCount: Integer;

procedure LoadQueriesFromConfig;
var
  JSONString: string;
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  I: Integer;
begin
  // Look for config file in same directory as executable
  ConfigPath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'queries.json');
  
  if not FileExists(ConfigPath) then
  begin
    WriteLn('Error: Configuration file not found: ', ConfigPath);
    WriteLn('Please ensure queries.json exists in the same directory as the executable.');
    Halt(1);
  end;

  try
    // Read JSON file
    JSONString := TFile.ReadAllText(ConfigPath, TEncoding.UTF8);
    JSONValue := TJSONObject.ParseJSONValue(JSONString);
    
    if JSONValue = nil then
    begin
      WriteLn('Error: Invalid JSON in configuration file');
      Halt(1);
    end;
    
    try
      if not (JSONValue is TJSONObject) then
      begin
        WriteLn('Error: Configuration file must contain a JSON object');
        Halt(1);
      end;
      
      JSONArray := TJSONObject(JSONValue).GetValue('queries') as TJSONArray;
      
      if JSONArray = nil then
      begin
        WriteLn('Error: Configuration file must contain a "queries" array');
        Halt(1);
      end;
      
      QueryCount := JSONArray.Count;
      SetLength(Queries, QueryCount);
      
      // Parse each query
      for I := 0 to QueryCount - 1 do
      begin
        JSONObject := JSONArray.Items[I] as TJSONObject;
        
        Queries[I].Number := JSONObject.GetValue<Integer>('number');
        Queries[I].Title := JSONObject.GetValue<string>('title');
        Queries[I].Description := JSONObject.GetValue<string>('description');
        Queries[I].SQL := JSONObject.GetValue<string>('sql');
      end;
      
      WriteLn('Successfully loaded ', QueryCount, ' queries from configuration file.');
      WriteLn;
    finally
      JSONValue.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error loading configuration file: ', E.Message);
      Halt(1);
    end;
  end;
end;

function QueryNumberExists(QueryNum: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to QueryCount - 1 do
  begin
    if Queries[I].Number = QueryNum then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure ConnectToDatabase;
begin
  Connection := TFDConnection.Create(nil);
  try
    Connection.DriverName := 'SQLite';
    Connection.Params.Database := DBPath;
    Connection.Params.Add('OpenMode=ReadOnly');
    Connection.Connected := True;
    WriteLn('Successfully connected to database: ', DBPath);
    WriteLn;
  except
    on E: Exception do
    begin
      WriteLn('Error connecting to database: ', E.Message);
      Halt(1);
    end;
  end;
end;

procedure DisplayMenu;
var
  I: Integer;
begin
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('        PELOTON WORKOUT STATISTICS ANALYZER');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('Available Queries:');
  WriteLn;
  for I := 0 to QueryCount - 1 do
  begin
    WriteLn(Format('[%2d] %s', [Queries[I].Number, Queries[I].Title]));
    WriteLn('     ', Queries[I].Description);
    WriteLn;
  end;
  WriteLn('[ 0] Exit');
  WriteLn;
  Write('Enter your choice (0-', QueryCount, '): ');
end;

procedure ExecuteQuery(QueryNum: Integer);
var
  I: Integer;
  FieldName: string;
  FieldValue: string;
  MaxWidth: Integer;
  Separator: string;
  QueryIndex: Integer;
begin
  // Find query by number
  QueryIndex := -1;
  for I := 0 to QueryCount - 1 do
  begin
    if Queries[I].Number = QueryNum then
    begin
      QueryIndex := I;
      Break;
    end;
  end;
  
  if QueryIndex = -1 then
  begin
    WriteLn('Invalid query number: ', QueryNum);
    Exit;
  end;

  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn(Queries[QueryIndex].Title);
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := Queries[QueryIndex].SQL;
    
    try
      Query.Open;
      
      if Query.IsEmpty then
      begin
        WriteLn('No data found.');
      end
      else
      begin
        // Print column headers
        for I := 0 to Query.FieldCount - 1 do
        begin
          if I > 0 then Write(' | ');
          FieldName := Query.Fields[I].FieldName;
          Write(Format('%-20s', [FieldName]));
        end;
        WriteLn;
        
        // Print separator
        Separator := StringOfChar('─', (Query.FieldCount * 23) - 3);
        WriteLn(Separator);
        
        // Print data rows
        while not Query.Eof do
        begin
          for I := 0 to Query.FieldCount - 1 do
          begin
            if I > 0 then Write(' | ');
            
            if Query.Fields[I].IsNull then
              FieldValue := 'N/A'
            else
              FieldValue := Query.Fields[I].AsString;
            
            Write(Format('%-20s', [FieldValue]));
          end;
          WriteLn;
          Query.Next;
        end;
        
        WriteLn;
        WriteLn('Total rows: ', Query.RecordCount);
      end;
    except
      on E: Exception do
        WriteLn('Error executing query: ', E.Message);
    end;
  finally
    Query.Free;
  end;
  
  WriteLn;
end;

procedure ShowUsage;
begin
  WriteLn('Peloton Workout Statistics Analyzer');
  WriteLn;
  WriteLn('Usage:');
  WriteLn('  PelotonStats <database_file> [-N]');
  WriteLn;
  WriteLn('Arguments:');
  WriteLn('  database_file   Path to the SQLite database file (*.sqlite)');
  WriteLn('  -N              Optional: Query number (1-10) to run directly');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  PelotonStats workouts.sqlite');
  WriteLn('  PelotonStats workouts.sqlite -2');
  WriteLn('  PelotonStats workouts.sqlite -5');
  WriteLn;
  WriteLn('If query number is not specified, an interactive menu will be displayed.');
  Halt(1);
end;

procedure ParseCommandLine;
var
  ParamStr2: string;
begin
  if ParamCount < 1 then
    ShowUsage;

  DBPath := ParamStr(1);
  
  if not FileExists(DBPath) then
  begin
    WriteLn('Error: Database file not found: ', DBPath);
    Halt(1);
  end;

  QueryNumber := -1; // -1 means show menu
  
  if ParamCount >= 2 then
  begin
    ParamStr2 := ParamStr(2);
    if (Length(ParamStr2) > 1) and (ParamStr2[1] = '-') then
    begin
      Delete(ParamStr2, 1, 1); // Remove the '-'
      if TryStrToInt(ParamStr2, QueryNumber) then
      begin
        // Validate query number exists in config
        if QueryNumber < 1 then
        begin
          WriteLn('Error: Query number must be at least 1');
          Halt(1);
        end;
      end
      else
      begin
        WriteLn('Error: Invalid query number: -', ParamStr2);
        Halt(1);
      end;
    end
    else
    begin
      WriteLn('Error: Second parameter must be in format -N (e.g., -2, -5)');
      ShowUsage;
    end;
  end;
end;

procedure RunInteractiveMode;
var
  Choice: Integer;
  Input: string;
begin
  repeat
    DisplayMenu;
    ReadLn(Input);
    
    if TryStrToInt(Input, Choice) then
    begin
      if Choice = 0 then
      begin
        WriteLn;
        WriteLn('Thank you for using Peloton Stats Analyzer!');
        Break;
      end
      else if Choice >= 1 then
      begin
        // Check if query number exists
        if QueryNumberExists(Choice) then
        begin
          ExecuteQuery(Choice);
          WriteLn;
          Write('Press Enter to continue...');
          ReadLn;
          WriteLn;
        end
        else
        begin
          WriteLn('Invalid choice. Please enter a valid query number.');
          WriteLn;
          Write('Press Enter to continue...');
          ReadLn;
        end;
      end
      else
      begin
        WriteLn('Invalid choice. Please enter a valid query number or 0 to exit.');
        WriteLn;
        Write('Press Enter to continue...');
        ReadLn;
      end;
    end
    else
    begin
      WriteLn('Invalid input. Please enter a number.');
      WriteLn;
      Write('Press Enter to continue...');
      ReadLn;
    end;
  until False;
end;

begin
  try
    LoadQueriesFromConfig;
    ParseCommandLine;
    ConnectToDatabase;
    
    if QueryNumber > 0 then
    begin
      // Direct query execution mode
      ExecuteQuery(QueryNumber);
    end
    else
    begin
      // Interactive menu mode
      RunInteractiveMode;
    end;
    
    Connection.Free;
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
