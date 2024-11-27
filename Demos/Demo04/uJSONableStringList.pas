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
    function    JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
    procedure   JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
    procedure   JSONExit;
  end;

implementation
uses System.Generics.Collections;

procedure TJSONableStringList.JSONInit;
begin
  OwnsObjects := False;
end;

procedure TJSONableStringList.JSONExit;
begin
  Clear;
end;

function TJSONableStringList.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LArr: TJSONArray;
  LStr: string;
begin
  LArr := TJSONArray.Create;
  for LStr in Self do LArr.Add(LStr);
  Result := Format('"%s":%s', [AInfoBlock.FieldName, LArr.ToJSON]);
  LArr.Free;
end;

procedure TJSONableStringList.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LArr: TJSONArray;
  LStr: TJSONValue;
begin
  Clear;
  LArr := AInfoBlock.Obj.Pairs[0].JsonValue  as TJSONArray;
  for LStr in LArr do Self.Add(LStr.AsType<string>);
end;


end.
