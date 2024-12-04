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
  private
    FIsManaged: Boolean;
  public
    procedure   JSONCreate(AManaged: Boolean);
    function    JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
    procedure   JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
    function    JSONDestroy: Boolean;
  end;

implementation
uses System.Generics.Collections;

procedure TJSONableStringList.JSONCreate(AManaged: Boolean);
begin
  FIsManaged := AManaged;  // AManaged : true if the object is created by the json engine.
  OwnsObjects := False;
end;

function TJSONableStringList.JSONDestroy: Boolean;
begin
  Clear;
  Result := FIsManaged; // send it back to the engine
end;

function TJSONableStringList.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LArr: TJSONArray;
  LStr: string;
begin
  // Custom serialization
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
  // Custom deserialization
  Clear;
  LArr := AInfoBlock.Obj.Pairs[0].JsonValue  as TJSONArray;
  for LStr in LArr do Self.Add(LStr.AsType<string>);
end;


end.
