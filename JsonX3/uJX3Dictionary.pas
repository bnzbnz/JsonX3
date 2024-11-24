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

    function  JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
    procedure JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);

    function  GetNull: Boolean;
    procedure SetNull(ANull: Boolean);
    property  IsNull: Boolean read GetNull write SetNull;

    class function  C: TJX3Dic<V>;
    class function  CAdd(AKey: string; AValue: V): TJX3Dic<V>;
    class function  CAddRange(const AKeys: array of string; const AValues: array of V): TJX3Dic<V>;

    function  Clone(AOptions: TJX3Options = [joNullToEmpty]): TJX3Dic<V>;
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

function TJX3Dic<V>.JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
var
  LParts: TStringList;
  LPart: TValue;
  LRes: string;
  Lkp: TPair<string,V>;
  LObj: TObject;
begin
  if GetNull then
  begin
    if joNullToEmpty in AOptions then Exit(TValue.Empty);
    if AFieldNAme.IsEmpty then EXit('null');
    Exit(Format('"%s":null', [AFieldName]));
  end;
  LParts := TStringList.Create(#0, cCommaDelimiter, [soStrictDelimiter]);

  for Lkp in Self do
  begin
    LObj := TValue.From<V>(Lkp.Value).AsObject;
    var LVal := Lkp.Key;
    if Assigned(LObj) then
    begin
      LPart := TJX3Tools.CallMethod( 'JSONSerialize', LObj, [LVal, AField, TValue.From<TJX3Options>(AOptions)]);
      if not LPart.IsEmpty then LParts.Add(LPart.AsString);
    end;
  end;
  LRes := LParts.DelimitedText.Replace(cCommaDelimiter, ',');
  if AFieldNAme.IsEmpty then
    Result := '{'+ LRes + '}'
  else
    Result := '"' + AFieldNAme + '":{'+ LRes + '}';
  LParts.Free;
end;

procedure TJX3Dic<V>.JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
var
  LPair: TJSONPair;
  LObj: TJSONObject;
  LNewObj: TObject;
begin

  if not Assigned(AJObj) then begin setNull(True); Exit end;;
  if not Assigned(AJObj.Pairs[0].JsonValue) then begin setNull(True); Exit end;
  if AJObj.Pairs[0].JsonValue.Null then begin setNull(True); Exit end;;

  for LPair in AJObj do
  begin
    LNewObj := V.Create;
    Add(LPair.JsonString.value, LNewObj);
    LPair.JsonValue.Owned := False;
    LPair.Owned := False;
    LObj := TJSONObject.Create(LPair);
    TJX3Tools.CallMethod(
        'JSONDeserialize'
        , LNewObj
      , [
            LObj
          , AField
          , TValue.From<TJX3Options>(AOptions)
        ]
    );
    FreeAndNil(LObj);
    LPair.Owned := True;
    LPair.JsonValue.Owned := True;

  end;
end;

function TJX3Dic<V>.Clone(AOptions: TJX3Options = [joNullToEmpty]): TJX3Dic<V>;
var
  LJson: TValue;
  LObj: TJSONObject;
begin
  LObj := Nil;
  try
    Result := TJX3Dic<V>.Create;
    LJson := TJX3Tools.CallMethod('JSONSerialize', Self, ['', Nil, TValue.From<TJX3Options>(AOptions)]);
    if not LJson.IsEmpty then LObj := TJSONObject.ParseJSONValue(LJson.AsString, True, True ) as TJSONObject;
    LJson.Empty;
    TJX3Tools.CallMethod( 'JSONDeserialize', Result, [LObj, Nil, TValue.From<TJX3Options>(AOptions)]);
  finally
    LObj.Free;
  end;
end;

end.
