unit uJX3Dictionary;

interface
uses
  System.Generics.Collections
  , JSON
  , RTTI
  , uJX3Object
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
    procedure JSONClone(ADest: TJX3Dic<V>; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil);
    procedure JSONMerge(ASrc: TJX3Dic<V>; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock = Nil);

    class function C: TJX3Dic<V>;
    class function CAdd(AKey: string; AValue: V): TJX3Dic<V>;
    class function CAddRange(const AKeys: array of string; const AValues: array of V): TJX3Dic<V>;

    property  IsNull: Boolean read GetIsNull write SetIsNull;
    property  N:      Boolean read GetIsNull write SetIsNull;
  end;
  TJX3Dictionary<V:class, constructor> = class(TJX3Dic<V>);
  TJX3Dict<V:class, constructor> = class(TJX3Dic<V>);

implementation
uses
  Classes
  , Sysutils
  , StrUTils
  , System.Diagnostics
  , uJX3String
  , uJX3Number
  , uJX3List
  , uJX3Boolean
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
  LName := TJX3Object.NameDecode(LName);

  if GetIsNull then
  begin
    if Assigned(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
      raise Exception.Create(Format('"%s" (TJX3Dic) : a value is required', [LName]));

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
      LPart :=  JX3CallMethodFunc('JSONSerialize', LObj, [TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock)]);
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
  LJObjDestroy: Boolean;
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
    LJObjDestroy := True;
    if LPair.JsonValue is TJSONObject then
    begin
       LJObjDestroy := False;
       LJObj := LPair.JsonValue as TJSONObject;
    end else
    if LPair.JsonValue is TJSONArray then
    begin
      LJObj := TJSONObject.Create(TJSONPAir.Create('', LPair.JsonValue));
    end else
      LJObj := TJSONObject.Create(LPair);

    LInfoBlock := TJX3InfoBlock.Create(AInfoBlock.FieldName, LJObj, AInfoBlock.Field, AInfoBlock.Options);
    JX3CallMethodProc( 'JSONDeserialize', LNewObj, [ TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock) ]);
    LInfoBlock.Free;

    if LJObjDestroy then FreeAndNil(LJObj);
    LPair.Owned := True;
    LPair.JsonValue.Owned := True;
  end;
end;

procedure TJX3Dic<V>.JSONClone(ADest: TJX3Dic<V>; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  LNewObj: TObject;
  LPair:  TPair<string, V>;
begin
  if (joStats in AOptions) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.DicCount);
  if GetIsNull then
  begin
    ADest.SetIsNull(True);
    Exit;
  end;
  for LPair in Self do
  begin
    LNewObj := JX3CreateObject(V);
    JX3CallMethodProc('JSONCreate', LNewObj, [True]);
    JX3CallMethodProc('JSONClone', LPair.Value, [LNewObj, TValue.From<TJX3Options>(AOptions), AInOutBlock]);
    ADest.Add(LPair.Key, LNewObj);
  end;
end;

procedure TJX3Dic<V>.JSONMerge(ASrc: TJX3Dic<V>; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock);
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
      JX3CallMethodProc('JSONCreate', LObj, [True]);
      JX3CallMethodProc('JSONMerge', LObj, [ ADic.Value, TValue.From<TJX3Options>(AMergeOpts), AInOutBlock]);
      Self.Add(ADic.Key, LObj)
    end;
  end
end;

end.
