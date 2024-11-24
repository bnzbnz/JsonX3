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

    function    JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
    procedure   JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);

    function        GetNull: Boolean;
    procedure       SetNull(ANull: Boolean);
    function        First:T;
    function        Last: T;
    class function  C: TJX3List<T>;
    class function  CAdd(AValue: T): TJX3List<T>;
    class function  CAddRange(const AValues: array of T): TJX3List<T>; overload;
    function        Clone(AOptions: TJX3Options = [joNullToEmpty]): TJX3List<T>;
  end;
  TJX3StrList = class(TJX3List<TJX3String>);
  TJX3NumList = class(TJX3List<TJX3Number>);
  TJX3BoolList = class(TJX3List<TJX3Boolean>);

  implementation
uses
  Classes
  , SysUtils
  , StrUtils
  , uJX3Object
  , TypInfo
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

function TJX3List<T>.JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
var
  LParts: TStringList;
  LPart: TValue;
  LRes: string;
  LEle:  T;
begin
  if Self.Count = 0 then
  begin
    if joNullToEmpty in AOptions then Exit(TValue.Empty);
    if AFieldName.IsEmpty then EXit('null');
    Exit(Format('"%s":null', [AFieldName]));
  end;
  LParts := TStringList.Create(#0, cCommaDelimiter, [soStrictDelimiter]);
  for LEle in Self do
  begin
    LPart := TJX3Tools.CallMethod('JSONSerialize', LEle, ['', AField, TValue.From<TJX3Options>(AOptions)]);
     if not LPart.IsEmpty then LParts.Add(LPart.AsString);
  end;
  LRes := LParts.DelimitedText.Replace(cCommaDelimiter, ',');
  if AFieldNAme.IsEmpty then
    Result := '['+ LRes + ']'
  else
    Result := '"' + AFieldName + '":['+ LRes + ']';
  LParts.Free;
end;

procedure TJX3List<T>.JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
var
  LEle: TJSONValue;
  LNewObj: TObject;
  LObj: TJSONObject;
begin
  if not Assigned(AJObj) then begin setNull(True); Exit end;;
  if AJObj.Count = 0 then begin setNull(True); Exit end;;
  if not Assigned(AJObj.Pairs[0].JsonValue) then begin setNull(True); Exit end;
  if AJObj.Pairs[0].JsonValue.Null then begin setNull(True); Exit end;;

  if TJSONArray(AJObj.Pairs[0].JsonValue) is TJSONArray then
  for LEle in TJSONArray(AJObj.Pairs[0].JsonValue) do
  begin
    if LELe is TJSONObject then
    begin
       LNewObj := T.Create;
       Add(LNewObj);
       TJX3Tools.CallMethod( 'JSONDeserialize', LNewObj, [ LEle, AField, TValue.From<TJX3Options>(AOptions) ] );
    end else begin
       LNewObj := T.Create;
       Add(LNewObj);
       LEle.Owned := False;
       LObj := TJSONOBject.Create(TJSONPair.Create('', LEle));
       TJX3Tools.CallMethod( 'JSONDeserialize', LNewObj, [ LObj, AField, TValue.From<TJX3Options>(AOptions) ] );
       LObj.Free;
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
    LJson := TJX3Tools.CallMethod('JSONSerialize', Self, ['', Nil, TValue.From<TJX3Options>(AOptions)]);
    if not LJson.IsEmpty then LVal := TJSONObject.ParseJSONValue(LJson.AsString, True, True ) as TJSONValue;
    LJson.Empty;
    LVal.Owned := False;
    LObj := TJSONObject.Create(TJSONPair.Create('', LVal));
    TJX3Tools.CallMethod( 'JSONDeserialize', Result, [LObj, Nil, TValue.From<TJX3Options>(AOptions)]);
  finally
    LObj.Free;
    LVal.Owned := True;
    LVal.Free;
  end;
end;

end.
