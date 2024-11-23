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
  TJX3ObjectDictionary<V:class, constructor> = class(TObjectDictionary<V>)
  public
    constructor Create;

    function    JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
    procedure   JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);

    function  GetNull: Boolean;
    procedure SetNull(ANull: Boolean);
    property  IsNull: Boolean read GetNull write SetNull;
  end;
  TJX3Dic<Obj:class, constructor> = class(TJX3ObjectDictionary<Obj>);

implementation
uses
  Classes
  , Sysutils
  , StrUTils
  , windows
  , uJX3String
  , uJX3Number
  , uJX3List
  , uJX3Boolean
  , uJX3Object
  ;

{ TJX3Dictionar<K, V> }

constructor TJX3ObjectDictionary<V>.Create;
begin
  inherited Create([doOwnsValues]);
end;

function TJX3ObjectDictionary<V>.GetNull: Boolean;
begin
  Result := Count = 0;
end;

procedure TJX3ObjectDictionary<V>.SetNull(ANull: Boolean);
begin
  Clear;
end;

function TJX3ObjectDictionary<V>.JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
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

procedure TJX3ObjectDictionary<V>.JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
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

end.
