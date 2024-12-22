unit uJSONableStringList;

interface
uses
  Classes
  , JSON
  , RTTI
  , uJX3Object
  , SysUtils
  ;

type

  TJSONableStringList = class(TStringList)
  private
    FIsManaged: Boolean;
  public
    procedure   JSONCreate(AManaged: Boolean);
    procedure   JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
    procedure   JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
    procedure   JSONClone(ADest: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
    procedure   JSONMerge(ASrc: TJSONableStringList; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock);
    function    JSONDestroy: Boolean;
  end;

implementation
uses System.Generics.Collections;

procedure TJSONableStringList.JSONCreate(AManaged: Boolean);
begin
  FIsManaged := AManaged;  // AManaged : true if the object is created by the json engine.
end;

function TJSONableStringList.JSONDestroy: Boolean;
begin
  Result := FIsManaged; // send it back to the engine
end;

procedure TJSONableStringList.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LArr: TJSONArray;   // using std JSON libraries
  LStr: string;
begin
  // Custom serialization
  if Count = 0 then  // null;
    AInfoBlock.SetJSON( Format('"%s":null', [AInfoBlock.FieldName]) )
  else begin
    LArr := TJSONArray.Create;
    for LStr in Self do
    begin
      TJX3Object.EscapeJSONStr(LStr); // String escape to JSON Format
      LArr.Add(LStr);
    end;
    AInfoBlock.SetJSON( Format('"%s":%s', [AInfoBlock.FieldName, LArr.ToJSON]) );
    LArr.Free;
  end;
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

// Optional
procedure TJSONableStringList.JSONClone(ADest: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  AStr: string;
begin
  for AStr in Self do
    TJSONableStringList(ADest).Add(AStr);
end;

// Optional
procedure TJSONableStringList.JSONMerge(ASrc: TJSONableStringList; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  AStr: string;
begin
  Clear;
  for AStr in ASrc do
  begin
    Self.Add(AStr);
  end;
end;

end.
