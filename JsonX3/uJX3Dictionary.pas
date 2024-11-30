unit uJX3Dictionary;

interface
uses
  System.Generics.Collections
  , JSON
  , RTTI
  , uJX3Tools
  ;

type

  TObjectDictionary<V> = class(System.Generics.Collections.TObjectDictionary<string,V>);
  TJX3Dic<V:class, constructor> = class(TObjectDictionary<V>)
  public
    constructor Create;

    function  JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);

    function  GetNull: Boolean;
    procedure SetNull(ANull: Boolean);
    property  IsNull: Boolean read GetNull write SetNull;

    class function  C: TJX3Dic<V>;
    class function  CAdd(AKey: string; AValue: V): TJX3Dic<V>;
    class function  CAddRange(const AKeys: array of string; const AValues: array of V): TJX3Dic<V>;

    function Clone(AOptions: TJX3Options = [joNullToEmpty]; AInOutBlock: TJX3InOutBlock = Nil): TJX3Dic<V>;
  end;

implementation
uses
  Classes
  , Sysutils
  , StrUTils
  , uJX3String
  , uJX3Number
  , uJX3List
  , uJX3Boolean
  , uJX3Object
  , uJX3Rtti
  ;

class function TJX3Dic<V>.CAdd(AKey: string; AValue: V): TJX3Dic<V>;
begin
  Result := TJX3Dic<V>.Create;
  Result.Add(AKey, AValue);
end;

class function TJX3Dic<V>.CAddRange(const AKeys: array of string; const AValues: array of V): TJX3Dic<V>;
var
  RCnt: Integer;
begin
  Result := TJX3Dic<V>.Create;
  for RCnt := 0  to Length(AKeys) -1 do
    Result.Add(AKeys[RCnt], AValues[RCnt]);
end;

constructor TJX3Dic<V>.Create;
begin
  inherited Create([doOwnsValues]);
end;

function TJX3Dic<V>.GetNull: Boolean;
begin
  Result := Count = 0;
end;

procedure TJX3Dic<V>.SetNull(ANull: Boolean);
begin
  Clear;
end;

class function TJX3Dic<V>.C: TJX3Dic<V>;
begin
  Result := TJX3Dic<V>.Create;
end;

function TJX3Dic<V>.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LParts: TStringList;
  LPart: TValue;
  LRes: string;
  Lkp: TPair<string,V>;
  LObj: TObject;
  LInfoBlock: TJX3InfoBlock;
  LName: string;
  LNameAttr:      JX3Name;
  LNoEncodeAttr:  JX3DisableNameEncoding;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.DicsCount);

  if Assigned(AInfoBlock.Field) then
  begin
    LName := AInfoBlock.Field.Name;
    LNameAttr := JX3Name(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Name));
    if Assigned(LNameAttr) then LName := LNameAttr.Name;
    LNoEncodeAttr := JX3DisableNameEncoding(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3DisableNameEncoding));
    if not Assigned(LNoEncodeAttr) then LName := TJX3Tools.NameDecode(LName);
  end else
    LName := AInfoBlock.FieldName;

  if GetNull then
  begin
    if joNullToEmpty in AInfoBlock.Options then Exit(TValue.Empty);
    if AInfoBlock.FieldName.IsEmpty then EXit('null');
    Exit(Format('"%s":null', [LName]));
  end;

  LParts := TStringList.Create(#0, cCommaDelimiter, [soStrictDelimiter]);
  LParts.Capacity := Self.Count;
  for Lkp in Self do
  begin
    LObj := TValue.From<V>(Lkp.Value).AsObject;
    var LVal := Lkp.Key;
    if Assigned(LObj) then
    begin
      LInfoBlock := TJX3InfoBlock.Create(Nil, LVal, AInfoBlock.Field, AInfoBlock.Options);
      LPart :=  TJX3Tools.CallMethod(
                  'JSONSerialize'
                , LObj
                , [
                      TValue.From<TJX3InfoBlock>(LInfoBlock)
                    , TValue.From<TJX3InOutBlock>(AInOutBlock)
                  ]
                );
      if not LPart.IsEmpty then LParts.Add(LPart.AsString);
      LInfoBlock.Free;
    end;
  end;
  LRes := LParts.DelimitedText.Replace(cCommaDelimiter, ',');

  if AInfoBlock.FieldNAme.IsEmpty then
    Result := '{'+ LRes + '}'
  else
    Result := '"' + LName + '":{'+ LRes + '}';
  LParts.Free;
end;

procedure TJX3Dic<V>.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LPair: TJSONPair;
  LNewObj: TObject;
  LInfoBlock: TJX3InfoBlock;
begin

  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.DicsCount);

  if not Assigned(AInfoBlock.Obj) then begin setNull(True); Exit end;;
  if not Assigned(AInfoBlock.Obj.Pairs[0].JsonValue) then begin setNull(True); Exit end;
  if AInfoBlock.Obj.Pairs[0].JsonValue.Null then begin setNull(True); Exit end;;

  for LPair in AInfoBlock.Obj do
  begin
    LNewObj := V.Create;
    Add(LPair.JsonString.value, LNewObj);
    LPair.JsonValue.Owned := False;
    LPair.Owned := False;
    LInfoBlock := TJX3InfoBlock.Create(TJSONObject.Create(LPair), AInfoBlock.FieldName, AInfoBlock.Field, AInfoBlock.Options);
    TJX3Tools.CallMethod( 'JSONDeserialize', LNewObj, [ TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock) ]);
    LInfoBlock.Obj.Free;
    LInfoBlock.Free;
    LPair.Owned := True;
    LPair.JsonValue.Owned := True;
  end;
end;

function TJX3Dic<V>.Clone(AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): TJX3Dic<V>;
var
  LJson: TValue;
  LInfoBlock : TJX3InfoBlock;
begin
  LInfoBlock := Nil;
  try
    Result := TJX3Dic<V>.Create;
    LInfoBlock := TJX3InfoBlock.Create(Nil, '', Nil, AOptions);
    LJson := TJX3Tools.CallMethod('JSONSerialize', Self, [TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock)]);
    if not LJson.IsEmpty then LInfoBlock.Obj := TJSONObject.ParseJSONValue(LJson.AsString, True, True) as TJSONObject;
    LJson.Empty;
    TJX3Tools.CallMethod( 'JSONDeserialize', Result, [TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock)]);
    LInfoBlock.Obj.Free;
  finally
    LInfoBlock.Free;
  end;
end;

end.
