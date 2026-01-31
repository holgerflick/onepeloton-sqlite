unit QueryManager;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,
  PelotonTypes;

type
  TQueryManager = class
  private
    FQueries: TArray<TQueryInfo>;
    FConfigPath: string;
    procedure LoadFromFile(const AConfigPath: string);
  public
    constructor Create(const AConfigPath: string);
    destructor Destroy; override;
    
    function GetQueryCount: Integer;
    function GetQuery(Index: Integer): TQueryInfo;
    function GetQueryByNumber(QueryNum: Integer): TQueryInfo;
    function QueryNumberExists(QueryNum: Integer): Boolean;
    function GetAllQueries: TArray<TQueryInfo>;
    
    property Queries: TArray<TQueryInfo> read FQueries;
    property ConfigPath: string read FConfigPath;
  end;

implementation

{ TQueryManager }

constructor TQueryManager.Create(const AConfigPath: string);
begin
  inherited Create;
  FConfigPath := AConfigPath;
  LoadFromFile(FConfigPath);
end;

destructor TQueryManager.Destroy;
begin
  SetLength(FQueries, 0);
  inherited;
end;

procedure TQueryManager.LoadFromFile(const AConfigPath: string);
var
  JSONString: string;
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  I: Integer;
begin
  if not FileExists(AConfigPath) then
    raise Exception.CreateFmt('Configuration file not found: %s', [AConfigPath]);

  try
    JSONString := TFile.ReadAllText(AConfigPath, TEncoding.UTF8);
    JSONValue := TJSONObject.ParseJSONValue(JSONString);
    
    if JSONValue = nil then
      raise Exception.Create('Invalid JSON in configuration file');
    
    try
      if not (JSONValue is TJSONObject) then
        raise Exception.Create('Configuration file must contain a JSON object');
      
      JSONArray := TJSONObject(JSONValue).GetValue('queries') as TJSONArray;
      
      if JSONArray = nil then
        raise Exception.Create('Configuration file must contain a "queries" array');
      
      SetLength(FQueries, JSONArray.Count);
      
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONObject := JSONArray.Items[I] as TJSONObject;
        
        FQueries[I].Number := JSONObject.GetValue<Integer>('number');
        FQueries[I].Title := JSONObject.GetValue<string>('title');
        FQueries[I].Description := JSONObject.GetValue<string>('description');
        FQueries[I].SQL := JSONObject.GetValue<string>('sql');
      end;
    finally
      JSONValue.Free;
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt('Error loading configuration file: %s', [E.Message]);
  end;
end;

function TQueryManager.GetQueryCount: Integer;
begin
  Result := Length(FQueries);
end;

function TQueryManager.GetQuery(Index: Integer): TQueryInfo;
begin
  if (Index < 0) or (Index >= Length(FQueries)) then
    raise Exception.CreateFmt('Query index %d out of bounds', [Index]);
  Result := FQueries[Index];
end;

function TQueryManager.GetQueryByNumber(QueryNum: Integer): TQueryInfo;
var
  I: Integer;
begin
  for I := 0 to Length(FQueries) - 1 do
  begin
    if FQueries[I].Number = QueryNum then
    begin
      Result := FQueries[I];
      Exit;
    end;
  end;
  raise Exception.CreateFmt('Query number %d not found', [QueryNum]);
end;

function TQueryManager.QueryNumberExists(QueryNum: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(FQueries) - 1 do
  begin
    if FQueries[I].Number = QueryNum then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TQueryManager.GetAllQueries: TArray<TQueryInfo>;
begin
  Result := FQueries;
end;

end.
