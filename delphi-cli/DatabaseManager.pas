unit DatabaseManager;

interface

uses
  System.SysUtils,
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
  TDatabaseManager = class
  private
    FConnection: TFDConnection;
    FDatabasePath: string;
  public
    constructor Create(const ADatabasePath: string);
    destructor Destroy; override;
    
    procedure Connect;
    procedure Disconnect;
    function ExecuteQuery(const SQL: string): TFDQuery;
    function IsConnected: Boolean;
    
    property Connection: TFDConnection read FConnection;
    property DatabasePath: string read FDatabasePath;
  end;

implementation

{ TDatabaseManager }

constructor TDatabaseManager.Create(const ADatabasePath: string);
begin
  inherited Create;
  FDatabasePath := ADatabasePath;
  FConnection := TFDConnection.Create(nil);
  FConnection.DriverName := 'SQLite';
  FConnection.Params.Database := FDatabasePath;
  FConnection.Params.Add('OpenMode=ReadOnly');
end;

destructor TDatabaseManager.Destroy;
begin
  if Assigned(FConnection) then
  begin
    if FConnection.Connected then
      FConnection.Connected := False;
    FConnection.Free;
  end;
  inherited;
end;

procedure TDatabaseManager.Connect;
begin
  if not FileExists(FDatabasePath) then
    raise Exception.CreateFmt('Database file not found: %s', [FDatabasePath]);
    
  try
    FConnection.Connected := True;
  except
    on E: Exception do
      raise Exception.CreateFmt('Error connecting to database: %s', [E.Message]);
  end;
end;

procedure TDatabaseManager.Disconnect;
begin
  if Assigned(FConnection) and FConnection.Connected then
    FConnection.Connected := False;
end;

function TDatabaseManager.ExecuteQuery(const SQL: string): TFDQuery;
var
  Query: TFDQuery;
begin
  if not FConnection.Connected then
    raise Exception.Create('Database is not connected');
    
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := SQL;
    Query.Open;
    Result := Query;
  except
    on E: Exception do
    begin
      Query.Free;
      raise Exception.CreateFmt('Error executing query: %s', [E.Message]);
    end;
  end;
end;

function TDatabaseManager.IsConnected: Boolean;
begin
  Result := Assigned(FConnection) and FConnection.Connected;
end;

end.
