unit uJX3Object;

interface
uses
  JSON,
  RTTI,
  uJX3Tools;

type

  TJX3Object = class(TObject)
  public
    function        JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);

    constructor     Create;
    destructor      Destroy; override;

    class function  FromJSON<T:class, constructor>(AJson: string; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
    function        ToJSON(AOptions: TJX3Options = [joNullToEmpty]; AInOutBlock: TJX3InOutBlock = Nil): string;
    function        CLone<T:class, constructor>(AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
  end;
  TJX3Obj = TJX3Object;

implementation
uses
    System.Diagnostics
  , SysUtils
  , Classes
  , TypInfo
  , uJX3String
  , uJX3Number
  , uJX3Rtti
  ;

constructor TJX3Object.Create;
var
  LFields:    TArray<TRttiField>;
  LField:     TRTTIField;
  LInstance:  TRttiInstanceType;
  LMethod:    TRTTIMEthod;
  LNewObj:    TOBject;
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
      LMethod := JX3GetMethod(LInstance, 'JSONInit');
      if LMethod <> nil then LMethod.Invoke(LNewObj, []);
    end;
  end;
end;

destructor TJX3Object.Destroy;
var
  LField:   TRTTIField;
  LFields:  TArray<TRttiField>;
  LObj:     TOBject;
  LMethod:  TRTTIMEthod;
begin
  LFields := JX3GetFields(Self);
  for LField in LFields do
    if  (LField.FieldType.TypeKind in [tkClass])
        and (LField.Visibility in [mvPublic, mvPublished])
        and (JX3GetFieldAttribute(LField, JX3DoNotManage) = Nil)
    then
    begin
      LObj := LField.GetValue(Self).AsObject;
      LMethod := JX3GetMethod(LField.FieldType.AsInstance, 'JSONExit');
      if  LMethod <> nil then LMethod.Invoke(LObj, []);
      FreeAndNil(LObj);
    end;
  inherited;
end;

function TJX3Object.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LField:     TRTTIField;
  LFields:    TArray<TRTTIField>;
  LParts:     TStringList;
  LPart:      TValue;
  LRes:       string;
  LInfoBlock: TJX3InfoBlock;
begin
  Result := '';
  LParts := TStringList.Create(#0, cCommaDelimiter, [soStrictDelimiter]);
  LFields := JX3GetFields(Self);
  LParts.Capacity := Length(LFields);
  for LField in LFields do
    if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility = mvPublic) then
    begin
      if joDisableNameEncoding in AInfoBlock.Options then
        LInfoBlock := TJX3InfoBlock.Create(Nil, LField.Name, LField, AInfoBlock.Options)
      else
        LInfoBlock := TJX3InfoBlock.Create(Nil, TJX3Tools.NameDecode(LField.Name), LField, AInfoBlock.Options);
      LPart :=  TJX3Tools.CallMethod(
            'JSONSerialize'
          , LField.GetValue(Self).AsObject
          , [
                TValue.From<TJX3InfoBlock>(LInfoBlock)
              , TValue.From<TJX3InOutBlock>(AInOutBlock)
            ]
      );
      if not LPart.IsEmpty then LParts.Add(LPart.AsString);
      LInfoBlock.Free;
      Continue;
    end;
    LRes := LParts.DelimitedText.Replace(cCommaDelimiter, ',');
    if not AInfoBlock.FieldName.IsEmpty then
    begin
      if LRes.IsEmpty then
      begin
        if joNullToEmpty in AInfoBlock.Options then
          Result := TValue.Empty
        else
          Result := Format('"%s":null', [AInfoBlock.FieldName]);
      end else
        Result := Format('"%s":{%s}', [AInfoBlock.FieldName, LRes])
    end
  else
    Result := Format('{%s}', [LRes]);
  LParts.Free;
end;

procedure TJX3Object.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LField: TRTTIField;
  LPair: TJSONPAir;
  LObj: TJSONObject;
  LInfoBlock: TJX3InfoBlock;
begin
  for Lpair in AInfoBlock.Obj do
  begin
    LPair.Owned := False;
    LPair.JsonValue.Owned := False;
    if (LPair.JsonValue is TJSONObject) then
      LObj := (LPair.JsonValue as TJSONObject)
    else
      LObj :=  TJSONObject.Create(LPair);
    if joDisableNameEncoding in AInfoBlock.Options then
      LField := TJX3Tools.GetRttiMember(Self, Lpair.JsonString.Value, tkClass, mvPublic)
    else
      LField := TJX3Tools.GetRttiMember(Self, TJX3Tools.NameEncode(Lpair.JsonString.Value), tkClass, mvPublic);
    if LField = Nil then
    begin
      if JoRaiseOnMissingField in AInfoBlock.Options then
        TJX3Tools.RaiseException( Format('Missing Field : %s', [TJX3Tools.NameEncode(Lpair.JsonString.Value)]));
    end else begin
      LInfoBlock := TJX3InfoBlock.Create(LObj, LField.Name, LField, AInfoBlock.Options);
      TJX3Tools.CallMethod('JSONDeserialize', LField.GetValue(Self).AsObject, [TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock)]);
      LInfoBlock.Free;
    end;
    if (Assigned(LObj)) and not (LPair.JsonValue is TJSONObject) then FreeAndNil(LObj);
    LPair.JsonValue.Owned := True;
    LPair.Owned := True;
  end;
end;

class function TJX3Object.FromJSON<T>(AJson: string; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): T;
var
  LInfoBlock: TJX3InfoBlock;
  LWatch: TStopWatch;
begin
  LInfoBlock := Nil;
  try
    if Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
    Result := T.Create;
    try
      LInfoBlock := TJX3InfoBlock.Create(
                        TJSONObject.ParseJSONValue(AJson, True, joRaiseException in AOptions) as TJSONObject
                      , ''
                      , Nil
                      , AOptions
                    );
      if not Assigned(LInfoBlock.Obj) then TJX3Tools.RaiseException('TJX3Object.FromJSON: Erroneous JSON string');
      TJX3Tools.CallMethod(
          'JSONDeserialize'
        , Result
        , [
              TValue.From<TJX3InfoBlock>(LInfoBlock)
            , TValue.From<TJX3InOutBlock>(AInOutBlock)
          ]
      );
      LInfoBlock.Obj.Free;
    except
      on Ex: Exception do
      begin
        FreeAndNil(Result);
        if joRaiseException in AOptions then Raise Ex;
      end;
    end;
  finally
    if Assigned(AInOutBlock) then AInOutBlock.ProcessingTimeMS := LWatch.ElapsedMilliseconds;
    LInfoBlock.Free;
  end;
end;

function TJX3Object.ToJSON(AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): string;
var
  LInfoBlock: TJX3InfoBlock;
  LWatch: TStopWatch;
begin
  LInfoBlock := Nil;
  try
    if Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
    try
      LInfoBlock := TJX3InfoBlock.Create(Nil, '', Nil, AOptions);
      Result := JSONSerialize(LInfoBlock, AInOutBlock).AsString;
    except
      on Ex: Exception do
      begin
        Result := '';
        if joRaiseException in AOptions then Raise Ex;
      end;
    end;
  finally
    if Assigned(AInOutBlock) then AInOutBlock.ProcessingTimeMS := LWatch.ElapsedMilliseconds;
    LInfoBlock.Free;
  end;
end;

function TJX3Object.Clone<T>(AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): T;
var
  LWatch: TStopWatch;
begin
  if Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
  Result := Self.FromJSON<T>(Self.ToJSON(AOptions), AOptions, AInOutBlock);
  if Assigned(AInOutBlock) then AInOutBlock.ProcessingTimeMS := LWatch.ElapsedMilliseconds;
end;

end.
