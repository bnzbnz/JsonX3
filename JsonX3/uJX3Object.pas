unit uJX3Object;

interface
uses uJX3Tools, JSON, RTTI;

type

  TJX3Object = class(TObject)
  public
    constructor     Create;
    destructor      Destroy; override;

    function        JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
    function        ToJSON(AOptions: TJX3Options = [joNullToEmpty]): string;
    procedure       JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);

    class function  FromJSON<T:class, constructor>(AJson: string; AOptions: TJX3Options = []): T;
    function        CLone<T:class, constructor>(AOptions: TJX3Options = []): T;
  end;
  TJX3Obj = TJX3Object;

implementation
uses
    SysUtils
  , Classes
  , TypInfo
  , uJX3String
  , uJX3Number
  , uJX3Rtti
  ;

constructor TJX3Object.Create;
var
  LFields: TArray<TRttiField>;
  LField: TRTTIField;
  LInstance: TRttiInstanceType;
  LMethod: TRTTIMEthod;
  LNewObj: TOBject;
begin
  inherited;
  LFields := JX3GetFields(Self);
  for LField in LFields do
  begin
    if  (LField.FieldType.TypeKind in [tkClass])
        and (LField.Visibility in [mvPublic, mvPublished])
        and (JX3GetFieldAttribute(LField, JX3DoNotManage) = Nil)
    then
    begin
      LInstance := LField.FieldType.AsInstance;
      LMethod := LInstance.GetMethod('Create');
      LNewObj := LMethod.Invoke(LInstance.MetaclassType,[]).AsObject;
      LField.SetValue(Self, LNewObj);
      LMethod := LInstance.GetMethod('JSONInit');
      if LMethod <> nil then LMethod.Invoke(LNewObj,[]);
    end;
  end;
end;

destructor TJX3Object.Destroy;
var
  LField: TRTTIField;
  LFields: TArray<TRttiField>;
  LObj: TOBject;
  LMethod: TRTTIMEthod;
begin
  LFields := JX3GetFields(Self);
  for LField in LFields do
    if  (LField.FieldType.TypeKind in [tkClass])
        and (LField.Visibility in [mvPublic, mvPublished])
        and (JX3GetFieldAttribute(LField, JX3DoNotManage) = Nil)
    then
    begin
      LObj := LField.GetValue(Self).AsObject;
      LMethod := LField.FieldType.GetMethod('JSONExit');
      if LMethod <> nil then LMethod.Invoke(LObj, []);
      FreeAndNil(LObj);
    end;
  inherited;
end;

function TJX3Object.JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
var
  LField: TRTTIField;
  LFields: TArray<TRTTIField>;
  LParts: TStringList;
  LPart: TValue;
  LRes: string;
begin
  Result := '';
  LParts := TStringList.Create(#0, cCommaDelimiter, [soStrictDelimiter]);
  LFields := JX3GetFields(Self);
  for LField in LFields do
    if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility = mvPublic) then
    begin
      LPart := TJX3Tools.CallMethod('JSONSerialize', LField.GetValue(Self).AsObject, [TJX3Tools.NameDecode(LField.Name), LField,TValue.From<TJX3Options>(AOptions)]);
      if not LPart.IsEmpty then LParts.Add(LPart.AsString);
      Continue;
    end;
    LRes := LParts.DelimitedText.Replace(cCommaDelimiter, ',');
    if not AFieldName.IsEmpty then
    begin
      if LRes.IsEmpty then
      begin
        if joNullToEmpty in AOptions then
          Result := TValue.Empty
        else
          Result := Format('"%s":null', [AFieldName]);
      end else
        Result := Format('"%s":{%s}', [AFieldName, LRes])
    end
  else
    Result := Format('{%s}', [LRes]);
  LParts.Free;
end;

procedure TJX3Object.JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
var
  LField: TRTTIField;
  LPair: TJSONPAir;
  LObj: TJSONObject;
begin
  for Lpair in AJObj do
  begin
    LPair.Owned := False;
    LPair.JsonValue.Owned := False;
    if (LPair.JsonValue is TJSONObject) then
      LObj := (LPair.JsonValue as TJSONObject)
    else
      LObj :=  TJSONObject.Create(LPair);

    LField := TJX3Tools.GetRTTIMember(Self, TJX3Tools.NameEncode(Lpair.JsonString.Value), tkClass, mvPublic);
    if LField = Nil then
    begin
      if JoRaiseOnMissingField in AOptions then
        TJX3Tools.RaiseException( Format('Missing Field : %s', [TJX3Tools.NameEncode(Lpair.JsonString.Value)]));
    end else
      TJX3Tools.CallMethod('JSONDeserialize', LField.GetValue(Self).AsObject, [LObj, LField, TValue.From<TJX3Options>(AOptions)]);

    if  (Assigned(LObj)) and not (LPair.JsonValue is TJSONObject) then FreeAndNil(LObj);
    LPair.JsonValue.Owned := True;
    LPair.Owned := True;
  end;
end;

class function TJX3Object.FromJSON<T>(AJson: string; AOptions: TJX3Options = []): T;
var
  LJObj: TJSONObject;
begin
  LJObj := Nil;
  try
    Result := T.Create;
    try
      LJObj := TJSONObject.ParseJSONValue(AJson, True, joRaiseException in AOptions) as TJSONObject;
      if not Assigned(LJObj) then TJX3Tools.RaiseException('TJX3Object.FromJSON: Erroneous JSON string');
      TJX3Tools.CallMethod('JSONDeserialize', Result, [LJObj, Nil, TValue.From<TJX3Options>(AOptions)]).AsObject;
    except
      on Ex: Exception do
      begin
        FreeAndNil(Result);
        if joRaiseException in AOptions then Raise Ex;
      end;
    end;
  finally
    LJObj.Free;
  end;
end;

function TJX3Object.ToJSON(AOptions: TJX3Options): string;
begin
  try
    Result := JSONSerialize('', Nil, AOptions).AsString;
  except
    on Ex: Exception do
    begin
      Result := '';
      if joRaiseException in AOptions then Raise Ex;
    end;
  end;
end;

function TJX3Object.Clone<T>(AOptions: TJX3Options= []): T;
begin
  Result := Self.FromJSON<T>(Self.ToJSON(AOptions), AOptions);
end;

end.
