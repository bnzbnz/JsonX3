unit uJX3List;

interface
uses
  System.Generics.Collections
  , JSON
  , RTTI
  , uJX3Tools
  , uJX3String
  , uJX3Boolean
  , uJX3Number
  ;

type

  TObjectList<T: class, constructor> = class(System.Generics.Collections.TObjectList<T>);
  TJX3List<T: class, constructor> = class(TObjectList<T>)
    constructor Create;
    destructor  Destroy; override;

    function    JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure   JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock =Nil);

    function        GetNull: Boolean;
    procedure       SetNull(ANull: Boolean);
    function        First:T;
    function        Last: T;
    class function  C: TJX3List<T>;
    class function  CAdd(AValue: T): TJX3List<T>;
    class function  CAddRange(const AValues: array of T): TJX3List<T>; overload;
    function        Clone(AOptions: TJX3Options = [joNullToEmpty]): TJX3List<T>;
  end;
  TJX3StrList   = class(TJX3List<TJX3String>);
  TJX3NumList   = class(TJX3List<TJX3Number>);
  TJX3BoolList  = class(TJX3List<TJX3Boolean>);

  implementation
uses
  Classes
  , SysUtils
  , StrUtils
  , uJX3Object
  , TypInfo
  , uJX3Rtti
  ;

class function TJX3List<T>.CAdd(AValue: T): TJX3List<T>;
begin
  Result := TJX3List<T>.Create;
  Result.Add(AValue);
end;

class function TJX3List<T>.CAddRange(const AValues: array of T): TJX3List<T>;
begin
  Result := TJX3List<T>.Create;
  Result.AddRange(AValues);
end;

function TJX3List<T>.GetNull: Boolean;
begin
  Result := Count = 0;
end;

procedure TJX3List<T>.SetNull(ANull: Boolean);
begin
  Clear;
end;

function TJX3List<T>.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LParts:     TStringList;
  LPart:      TValue;
  LRes:       string;
  LEle:       T;
  LInfoBlock: TJX3InfoBlock;
  LName:      string;
  LNameAttr:  JX3Name;
   LJObj:     TJSONObject;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.ListsCount);

  LName := AInfoBlock.FieldName;
  if Assigned(AInfoBlock) and Assigned(AInfoBlock.Field) then
  begin
    LNameAttr := JX3Name(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Name));
    if Assigned(LNameAttr) then LName := LNameAttr.Name;
  end;
  LName := TJX3Tools.NameDecode(LName);

  if Self.Count = 0 then
  begin
    if joNullToEmpty in AInfoBlock.Options then Exit(TValue.Empty);
    if AInfoBlock.FieldName.IsEmpty then EXit('null');
    Exit(Format('"%s":null', [LName]));
  end;

  LParts := TStringList.Create(#0, cCommaDelimiter, [soStrictDelimiter]);
  LParts.Capacity := Self.Count;
  for LEle in Self do
  begin
    LJObj := Nil;
    LInfoBlock := TJX3InfoBlock.Create('', LJObj, Nil, AInfoBlock.Options);
    LPart := TJX3Tools.CallMethodFunc('JSONSerialize', LEle, [ LInfoBlock, AInOutBlock ]);
    if not LPart.IsEmpty then LParts.Add(LPart.AsString);
    LInfoBlock.Free;
  end;
  LRes := LParts.DelimitedText.Replace(cCommaDelimiter, ',');

  if LName.IsEmpty then
    Result := '[' + LRes + ']'
  else
    Result := '"' + LName + '":[' + LRes + ']';
  LParts.Free;
end;

procedure TJX3List<T>.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LEle:       TJSONValue;
  LNewObj:    TObject;
  LInfoBlock: TJX3InfoBlock;
  LJObj:      TJSONObject;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.ListsCount);
  if not Assigned(AInfoBlock.Obj) then begin setNull(True); Exit end;;
  if AInfoBlock.Obj.Count = 0 then begin setNull(True); Exit end;;
  if not Assigned(AInfoBlock.Obj.Pairs[0].JsonValue) then begin setNull(True); Exit end;
  if AInfoBlock.Obj.Pairs[0].JsonValue.Null then begin setNull(True); Exit end;;
  if TJSONArray(AInfoBlock.Obj.Pairs[0].JsonValue) is TJSONArray then
    for LEle in TJSONArray(AInfoBlock.Obj.Pairs[0].JsonValue) do
    begin
      if LELe is TJSONObject then
      begin
         LNewObj := T.Create;
         Add(LNewObj);
         LJObj :=  LEle as TJSONObject;
         LInfoBlock := TJX3InfoBlock.Create(AInfoBlock.FieldName, LJObj, AInfoBlock.Field, AInfoBlock.Options);
         TJX3Tools.CallMethodProc( 'JSONDeserialize', LNewObj, [ TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock) ] );
         LInfoBlock.Free;
      end else begin
         LNewObj := T.Create;
         Add(LNewObj);
         LEle.Owned := False;
         LJObj :=  TJSONObject.Create(TJSONPair.Create('', LEle));
         LInfoBlock := TJX3InfoBlock.Create(AInfoBlock.FieldName, LJObj, AInfoBlock.Field, AInfoBlock.Options);
         TJX3Tools.CallMethodProc( 'JSONDeserialize', LNewObj, [ TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock) ] );
         LJObj.Free;;
         LInfoBlock.Free;
         LEle.Owned := True;
      end;
    end;
end;

class function TJX3List<T>.C: TJX3List<T>;
begin
  Result := TJX3List<T>.Create;
end;

constructor TJX3List<T>.Create;
begin
  inherited Create;
  Self.OwnsObjects := True;
end;

destructor TJX3List<T>.Destroy;
begin
  inherited;
end;

function TJX3List<T>.First: T;
begin
  Result := Self[0];
end;

function TJX3List<T>.Last: T;
begin
  Result := Self[Count - 1];
end;

function TJX3List<T>.Clone(AOptions: TJX3Options): TJX3List<T>;
var
  LJson: TValue;
  LVal: TJSONValue;
  LObj: TJSONObject;
begin
  LVal := Nil;
  LObj := Nil;
  try
    Result := TJX3List<T>.Create;
    LJson := TJX3Tools.CallMethodFunc('JSONSerialize', Self, ['', Nil, TValue.From<TJX3Options>(AOptions)]);
    if not LJson.IsEmpty then LVal := TJSONObject.ParseJSONValue(LJson.AsString, True, True ) as TJSONValue;
    LJson.Empty;
    LVal.Owned := False;
    LObj := TJSONObject.Create(TJSONPair.Create('', LVal));
    TJX3Tools.CallMethodProc( 'JSONDeserialize', Result, [LObj, Nil, TValue.From<TJX3Options>(AOptions)]);
  finally
    LObj.Free;
    LVal.Owned := True;
    LVal.Free;
  end;
end;

end.
