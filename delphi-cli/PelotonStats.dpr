program PelotonStats;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils,
  FireDAC.Comp.Client,
  PelotonTypes in 'PelotonTypes.pas',
  QueryManager in 'QueryManager.pas',
  DatabaseManager in 'DatabaseManager.pas',
  UIManager in 'UIManager.pas';

var
  QManager: TQueryManager;
  DBManager: TDatabaseManager;
  UIMan: TUIManager;
  DBPath: string;
  QueryNumber: Integer;
  ConfigPath: string;

procedure ParseCommandLine;
var
  ParamStr2: string;
begin
  if ParamCount < 1 then
  begin
    UIMan.ShowUsage;
    Halt(1);
  end;

  DBPath := ParamStr(1);
  
  if not FileExists(DBPath) then
  begin
    UIMan.ShowErrorMessage('Database file not found: ' + DBPath);
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
        if QueryNumber < 1 then
        begin
          UIMan.ShowErrorMessage('Query number must be at least 1');
          Halt(1);
        end;
        
        if not QManager.QueryNumberExists(QueryNumber) then
        begin
          UIMan.ShowErrorMessage(Format('Query number %d does not exist', [QueryNumber]));
          Halt(1);
        end;
      end
      else
      begin
        UIMan.ShowErrorMessage('Invalid query number: -' + ParamStr2);
        Halt(1);
      end;
    end
    else
    begin
      UIMan.ShowErrorMessage('Second parameter must be in format -N (e.g., -2, -5)');
      UIMan.ShowUsage;
      Halt(1);
    end;
  end;
end;

procedure ExecuteQuery(QueryNum: Integer);
var
  Query: TFDQuery;
  QueryInfo: TQueryInfo;
begin
  try
    QueryInfo := QManager.GetQueryByNumber(QueryNum);
    Query := DBManager.ExecuteQuery(QueryInfo.SQL);
    try
      UIMan.DisplayQueryResults(QueryInfo, Query);
    finally
      Query.Free;
    end;
  except
    on E: Exception do
      UIMan.ShowErrorMessage(E.Message);
  end;
end;
procedure RunInteractiveMode;
var
  Choice: Integer;
  Input: string;
begin
  repeat
    UIMan.DisplayMenu;
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
        if QManager.QueryNumberExists(Choice) then
        begin
          ExecuteQuery(Choice);
          WriteLn;
          Write('Press Enter to continue...');
          ReadLn;
          WriteLn;
        end
        else
        begin
          UIMan.ShowErrorMessage('Invalid choice. Please enter a valid query number.');
          WriteLn;
          Write('Press Enter to continue...');
          ReadLn;
        end;
      end
      else
      begin
        UIMan.ShowErrorMessage('Invalid choice. Please enter a valid query number or 0 to exit.');
        WriteLn;
        Write('Press Enter to continue...');
        ReadLn;
      end;
    end
    else
    begin
      UIMan.ShowErrorMessage('Invalid input. Please enter a number.');
      WriteLn;
      Write('Press Enter to continue...');
      ReadLn;
    end;
  until False;
end;

begin
  QManager := nil;
  DBManager := nil;
  UIMan := nil;
  
  try
    // Initialize configuration path
    ConfigPath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'queries.json');
    
    // Create managers
    QManager := TQueryManager.Create(ConfigPath);
    UIMan := TUIManager.Create(QManager);
    
    WriteLn('Successfully loaded ', QManager.GetQueryCount, ' queries from configuration file.');
    WriteLn;
    
    // Parse command line
    ParseCommandLine;
    
    // Create database manager and connect
    DBManager := TDatabaseManager.Create(DBPath);
    DBManager.Connect;
    
    WriteLn('Successfully connected to database: ', DBPath);
    WriteLn;
    
    // Execute query or run interactive mode
    if QueryNumber > 0 then
    begin
      ExecuteQuery(QueryNumber);
    end
    else
    begin
      RunInteractiveMode;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  // Cleanup
  if Assigned(DBManager) then
    DBManager.Free;
  if Assigned(QManager) then
    QManager.Free;
  if Assigned(UIMan) then
    UIMan.Free;
end.
