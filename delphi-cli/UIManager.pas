unit UIManager;

interface

uses
  System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client,
  PelotonTypes,
  QueryManager;

type
  TUIManager = class
  private
    FQueryManager: TQueryManager;
  public
    constructor Create(AQueryManager: TQueryManager);
    
    procedure ShowUsage;
    procedure DisplayMenu;
    procedure DisplayQueryResults(const QueryInfo: TQueryInfo; Query: TFDQuery);
    procedure ShowErrorMessage(const ErrorMsg: string);
    procedure ShowSuccessMessage(const Msg: string);
    
    property QueryManager: TQueryManager read FQueryManager write FQueryManager;
  end;

implementation

{ TUIManager }

constructor TUIManager.Create(AQueryManager: TQueryManager);
begin
  inherited Create;
  FQueryManager := AQueryManager;
end;

procedure TUIManager.ShowUsage;
begin
  WriteLn('Peloton Workout Statistics Analyzer');
  WriteLn;
  WriteLn('Usage:');
  WriteLn('  PelotonStats <database_file> [-N]');
  WriteLn;
  WriteLn('Arguments:');
  WriteLn('  database_file   Path to the SQLite database file (*.sqlite)');
  WriteLn('  -N              Optional: Query number to run directly');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  PelotonStats workouts.sqlite');
  WriteLn('  PelotonStats workouts.sqlite -2');
  WriteLn('  PelotonStats workouts.sqlite -5');
  WriteLn;
  WriteLn('If query number is not specified, an interactive menu will be displayed.');
end;

procedure TUIManager.DisplayMenu;
var
  I: Integer;
  QueryInfo: TQueryInfo;
begin
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('        PELOTON WORKOUT STATISTICS ANALYZER');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('Available Queries:');
  WriteLn;
  
  for I := 0 to FQueryManager.GetQueryCount - 1 do
  begin
    QueryInfo := FQueryManager.GetQuery(I);
    WriteLn(Format('[%2d] %s', [QueryInfo.Number, QueryInfo.Title]));
    WriteLn('     ', QueryInfo.Description);
    WriteLn;
  end;
  
  WriteLn('[ 0] Exit');
  WriteLn;
  Write('Enter your choice (0-', FQueryManager.GetQueryCount, '): ');
end;

procedure TUIManager.DisplayQueryResults(const QueryInfo: TQueryInfo; Query: TFDQuery);
var
  I: Integer;
  FieldName: string;
  FieldValue: string;
  Separator: string;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn(QueryInfo.Title);
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;
  
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
    Query.First;
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
  
  WriteLn;
end;

procedure TUIManager.ShowErrorMessage(const ErrorMsg: string);
begin
  WriteLn('Error: ', ErrorMsg);
end;

procedure TUIManager.ShowSuccessMessage(const Msg: string);
begin
  WriteLn(Msg);
end;

end.
