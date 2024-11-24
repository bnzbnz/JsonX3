unit uJSONableStringList;

interface
uses
  Classes
  , JSON
  , RTTI
  , uJX3Tools
  , SysUtils
  ;

type

  TJSONableStringList = class(TStringList)
  public
    procedure   JSONInit;
    function    JSONSerialize(AFieldName: string; AField: TRttiField; AOptions: TJX3Options): TValue;
    procedure   JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
    procedure   JSONExit;
  end;

implementation

procedure TJSONableStringList.JSONInit;
begin
end;

procedure TJSONableStringList.JSONExit;
begin
end;

function TJSONableStringList.JSONSerialize(AFieldName: string; AField: TRttiField; AOptions: TJX3Options): TValue;
var
  LArr: TJSONArray;
  LStr: string;
begin
  LArr := TJSONArray.Create;
  for LStr in Self do LArr.Add(LStr);
  Result := Format('"%s":%s', [AFieldName, LArr.ToJSON]);
  LArr.Free;
end;

procedure TJSONableStringList.JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
var
  LArr: TJSONArray;
  LStr: TJSONValue;
begin
  Clear;
  LArr := AJObj.Pairs[0].JsonValue  as TJSONArray;
  for LStr in LArr do Self.Add(LStr.AsType<string>);
end;


end.
