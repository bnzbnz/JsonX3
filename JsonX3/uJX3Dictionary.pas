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
  protected
    function  GetIsNull: Boolean;
    procedure SetIsNull(ANull: Boolean);
  public
    constructor Create;

    function  JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);

    function  Clone(AOptions: TJX3Options = [joNullToEmpty]; AInOutBlock: TJX3InOutBlock = Nil): TJX3Dic<V>;
    procedure JSONMerge(ASrc: TJX3Dic<V>; AMergeOpts: TJX3Options);

    property  IsNull: Boolean read GetIsNull write SetIsNull;
    property  N:      Boolean read GetIsNull write SetIsNull;

    class function C: TJX3Dic<V>;
    class function CAdd(AKey: string; AValue: V): TJX3Dic<V>;
    class function CAddRange(const AKeys: array of string; const AValues: array of V): TJX3Dic<V>;
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

function TJX3Dic<V>.GetIsNull: Boolean;
begin
  Result := Count = 0;
end;

procedure TJX3Dic<V>.SetIsNull(ANull: Boolean);
begin
  Clear;
end;

class function TJX3Dic<V>.C: TJX3Dic<V>;
begin
  Result := TJX3Dic<V>.Create;
end;

function TJX3Dic<V>.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LParts:     TStringList;
  LPart:      TValue;
  LRes:       string;
  Lkp:        TPair<string,V>;
  LObj:       TObject;
  LInfoBlock: TJX3InfoBlock;
  LName:      string;
  LNameAttr:  JX3Name;
  LJObj:      TJSONObject;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.DicCount);

  if Assigned(AInfoBlock.Field) then
  begin
    LName := AInfoBlock.Field.Name;
    LNameAttr := JX3Name(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Name));
    if Assigned(LNameAttr) then LName := LNameAttr.Name;
  end else
    LName := AInfoBlock.FieldName;
  LName := TJX3Tools.NameDecode(LName);

  if GetIsNull then
  begin
    if Assigned(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
      TJX3Tools.RaiseException(Format('"%s" (TJX3Dic) : a value is required', [LName]));

    if joNullToEmpty in AInfoBlock.Options then Exit(TValue.Empty);
    if AInfoBlock.FieldName.IsEmpty then EXit('null');
    Exit(Format('"%s":null', [LName]));
  end;

  LParts := TStringList.Create(#0, cCommaDelimiter, [soStrictDelimiter]);
  LParts.Capacity := Self.Count;
  for Lkp in Self do
  begin
    LObj := TValue.From<V>(Lkp.Value).AsObject;
    if Assigned(LObj) then
    begin
      LJObj := nil;
      LInfoBlock := TJX3InfoBlock.Create(LKp.Key, LJObj, Nil, AInfoBlock.Options);
      LPart :=  TJX3Tools.CallMethodFunc('JSONSerialize', LObj, [TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock)]);
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
  LJObj: TJSONObject;
begin

  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.DicCount);

  if not Assigned(AInfoBlock.Obj) then begin SetIsNull(True); Exit end;;
  if not Assigned(AInfoBlock.Obj.Pairs[0].JsonValue) then begin SetIsNull(True); Exit end;
  if AInfoBlock.Obj.Pairs[0].JsonValue.Null then begin SetIsNull(True); Exit end;;

  for LPair in AInfoBlock.Obj do
  begin
    LNewObj := V.Create;
    Add(LPair.JsonString.value, LNewObj);
    LPair.JsonValue.Owned := False;
    LPair.Owned := False;

    if LPair.JsonValue is TJSONArray then
    begin
      LJObj := TJSONObject.Create(TJSONPAir.Create('', LPair.JsonValue));
    end else
      LJObj := LPair.JsonValue as TJSONObject;

    LInfoBlock := TJX3InfoBlock.Create(AInfoBlock.FieldName, LJObj, AInfoBlock.Field, AInfoBlock.Options);
    TJX3Tools.CallMethodProc( 'JSONDeserialize', LNewObj, [ TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock) ]);
    LInfoBlock.Free;

    if LPair.JsonValue is TJSONArray then FreeAndNil(LJObj);
    LPair.Owned := True;
    LPair.JsonValue.Owned := True;
  end;

end;

function TJX3Dic<V>.Clone(AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): TJX3Dic<V>;
var
  LJson: TValue;
  LInfoBlock : TJX3InfoBlock;
  LJObj: TJSONObject;
begin
  LJObj := Nil;
  Result := TJX3Dic<V>.Create;
  LInfoBlock := TJX3InfoBlock.Create('', LJObj, Nil, AOptions);
  try
    LJson := TJX3Tools.CallMethodFunc('JSONSerialize', Self, [TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock)]);
    if LJson.IsEmpty then Exit(Nil);
    LInfoBlock.Obj := TJSONObject.ParseJSONValue(LJson.AsString, True, True) as TJSONObject;
    LJson.Empty;
    TJX3Tools.CallMethodProc('JSONDeserialize', Result, [TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock)]);
  finally
    if Assigned(LInfoBlock.Obj) then LInfoBlock.Obj.Free;
    LInfoBlock.Free;
  end;
end;

procedure TJX3Dic<V>.JSONMerge(ASrc: TJX3Dic<V>; AMergeOpts: TJX3Options);
var
  ADic: TPair<string, V>;
  LObj: V;
begin

  if ASrc.GetIsNull then
  begin
    Self.SetIsNull(True);
    Exit;
  end;

  for ADic in ASrc do
  begin
    if not Self.ContainsKey(ADic.Key) then
    begin
      LObj := V.Create;
      TJX3Tools.CallMethodProc('JSONCreate', LObj, [True]);
      TJX3Tools.CallMethodProc('JSONMerge', LObj, [ ADic.Value, TValue.From<TJX3Options>(AMergeOpts)]);
      Self.Add(ADic.Key, LObj);
    end;
  end

end;

end.
