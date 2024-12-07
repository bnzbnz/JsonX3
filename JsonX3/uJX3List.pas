unit uJX3List;

interface
uses
  System.Generics.Collections
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

    function        JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock =Nil);
    function        ToJSON(AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): string;
    class function  FromJSON(AJson: string; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
    function        Clone(AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): TJX3List<T>;
    procedure       JSONMerge(ASrc: TJX3List<T>; AMergeOpts: TJX3MeergeOptions);

    function        GetIsNull: Boolean;
    procedure       SetIsNull(ANull: Boolean);
    function        First:T;
    function        Last: T;

    property        IsNull: Boolean read GetIsNull write SetIsNull;
    property        N: Boolean read GetIsNull write SetIsNull;

    class function  C: TJX3List<T>;
    class function  CAdd(AValue: T): TJX3List<T>;
    class function  CAddRange(const AValues: array of T): TJX3List<T>; overload;
  end;

  TJX3StrList   = class(TJX3List<TJX3String>);
  TJX3NumList   = class(TJX3List<TJX3Number>);
  TJX3BoolList  = class(TJX3List<TJX3Boolean>);

  implementation
uses
  Classes
  , JSON
  , SysUtils
  , StrUtils
  , System.Diagnostics
  , uJX3Object
  , uJX3Rtti
  ;

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
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.ListCount);

  LName := AInfoBlock.FieldName;
  if Assigned(AInfoBlock) and Assigned(AInfoBlock.Field) then
  begin
    LNameAttr := JX3Name(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Name));
    if Assigned(LNameAttr) then LName := LNameAttr.Name;
  end;
  LName := TJX3Tools.NameDecode(LName);

  if GetIsNull then
  begin

    if Assigned(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
      TJX3Tools.RaiseException(Format('"%s" (TJX3List) : a value is required', [LName]));

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
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.ListCount);
  if not Assigned(AInfoBlock.Obj) then begin SetIsNull(True); Exit end;;
  if AInfoBlock.Obj.Count = 0 then begin SetIsNull(True); Exit end;;
  if not Assigned(AInfoBlock.Obj.Pairs[0].JsonValue) then begin SetIsNull(True); Exit end;
  if AInfoBlock.Obj.Pairs[0].JsonValue.Null then begin SetIsNull(True); Exit end;;
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

class function TJX3List<T>.FromJSON(AJson: string; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): T;
var
  LInfoBlock: TJX3InfoBlock;
  LWatch: TStopWatch;
  LJObj: TJSONObject;
begin
  LInfoBlock := Nil;
  LJObj := Nil;
  try
     if (joStats in AOptions) and Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
    Result := T.Create;
    try
      LJObj := TJSONObject.ParseJSONValue(AJson, True, joRaiseException in AOptions) as TJSONObject;
      if not Assigned(LJObj) then TJX3Tools.RaiseException('TJX3Object.FromJSON: Erroneous JSON string');
      FreeAndNil(LInfoBlock);
      LInfoBlock := TJX3InfoBlock.Create( '', LJObj, Nil, AOptions);
      TJX3Tools.CallMethodProc('JSONDeserialize', Result, [LInfoBlock, AInOutBlock]);
    except
      on Ex: Exception do
      begin
        FreeAndNil(Result);
        if joRaiseException in AOptions then Raise;
      end;
    end;
  finally
    LJObj.Free;
    LInfoBlock.Free;
    if (joStats in AOptions) and Assigned(AInOutBlock) then AInOutBlock.Stats.ProcessingTimeMS := LWatch.ElapsedMilliseconds;
  end;
end;

function TJX3List<T>.ToJSON(AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): string;
var
  LInfoBlock: TJX3InfoBlock;
  LWatch:     TStopWatch;
  LJObj:      TJSONObject;
begin
  LInfoBlock := Nil;
  try
    if (joStats in AOptions) and  Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
    try
      LJObj := Nil;
      LInfoBlock := TJX3InfoBlock.Create('', LJObj, Nil, AOptions);
      Result := '{"JX3List":' + JSONSerialize(LInfoBlock, AInOutBlock).AsString + '}';
    finally
      if (joStats in LInfoBlock.Options) and Assigned(AInOutBlock) then AInOutBlock.Stats.ProcessingTimeMS := LWatch.ElapsedMilliseconds;
      LInfoBlock.Free;
    end;
  except
    on Ex: Exception do
    begin
      Result := '';
      if joRaiseException in AOptions then Raise;
    end;
  end;
end;


function TJX3List<T>.Clone(AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): TJX3List<T>;
var
  LWatch: TStopWatch;
  LJObj: TJSONObject;
  LInfoBlock: TJX3InfoBlock;
  LJson: string;
begin
  if (joStats in AOptions) and Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
  LInfoBlock := Nil;
  LJObj := Nil;
  try
    Result := TJX3List<T>.Create;
    LInfoBlock := TJX3InfoBlock.Create('T', LJObj, Nil, AOptions);
    LJson := JSONSerialize(LInfoBlock, AInOutBlock).AsString;
    LInfoBlock.Free;
    LJObj := TJSONObject.ParseJSONValue('{'  + LJson + '}', True, joRaiseException in AOptions) as TJSONObject;
    LInfoBlock := TJX3InfoBlock.Create( '', LJObj, Nil, AOptions);
    JSONDeserialize(LInfoBlock, AInOutBlock);
    TJX3Tools.CallMethodProc('JSONDeserialize', Result, [LInfoBlock, AInOutBlock]);
  finally
    LJObj.Free;
    LInfoBlock.Free;
  end;
  if (joStats in AOptions) and Assigned(AInOutBlock) then AInOutBlock.Stats.ProcessingTimeMS := LWatch.ElapsedMilliseconds;
end;

constructor TJX3List<T>.Create;
begin
  inherited Create;
  Self.OwnsObjects := True;
end;

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

function TJX3List<T>.GetIsNull: Boolean;
begin
  Result := Count = 0;
end;

procedure TJX3List<T>.SetIsNull(ANull: Boolean);
begin
  Clear;
end;

class function TJX3List<T>.C: TJX3List<T>;
begin
  Result := TJX3List<T>.Create;
end;

function TJX3List<T>.First: T;
begin
  Result := Self[0];
end;

function TJX3List<T>.Last: T;
begin
  Result := Self[Count - 1];
end;

procedure TJX3List<T>.JSONMerge(ASrc: TJX3List<T>; AMergeOpts: TJX3MeergeOptions);
begin
    for var R in ASrc do
    begin
      var LOBJ := T.Create;
      TJX3Tools.CallMethodProc('JSONMerge', LOBJ, [ R, TValue.From<TJX3MeergeOptions>(AMergeOpts)]);
      Self.Add(LObj);
    end;
end;

end.
