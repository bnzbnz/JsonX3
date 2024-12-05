unit uJX3Object;

interface
uses
  JSON,
  RTTI,
  uJX3Tools;

type

  TJX3Object = class(TObject)
  public
    constructor     Create; virtual;
    destructor      Destroy; override;

    function        JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);

    class function  FromJSON<T:class, constructor>(AJson: string; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
    function        ToJSON(AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): string;
    function        Clone<T:class, constructor>(AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
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
  LNewObj:    TObject;
begin
  inherited Create;
  LFields := JX3GetFields(Self);
  for LField in LFields do
  begin
    if  (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic, mvPublished]) then
    begin
      if not Assigned(uJX3Rtti.JX3GetFieldAttribute(LField, JX3Unmanaged)) then
      begin
        LNewObj := Nil;
        LInstance := LField.FieldType.AsInstance;
        if not Assigned(LInstance) then Continue;
        LMethod := LInstance.GetMethod('Create');
        if not Assigned(LMethod) then Continue else
          LNewObj := LMethod.Invoke(LInstance.MetaclassType,[]).AsObject;
        if Assigned(LNewObj) then
          TJX3Tools.CallMethodProc('JSONCreate', LNewObj, [True]);
        LField.SetValue(Self, LNewObj);
      end else begin
        LField.SetValue(Self, Nil);
      end;
    end;
  end;
end;

destructor TJX3Object.Destroy;
var
  LField:   TRTTIField;
  LFields:  TArray<TRttiField>;
  LObj:     TOBject;
begin
  LFields := JX3GetFields(Self);
  for LField in LFields do
    if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic, mvPublished]) then
    begin
      LObj := LField.GetValue(Self).AsObject;
      if not Assigned(LObj) then Continue;
      if Assigned(uJX3Rtti.JX3GetFieldAttribute(LField, JX3Unmanaged)) then
      begin
        if TJX3Tools.CallMethodFunc('JSONDestroy', LObj, []).AsBoolean then
          FreeAndNil(LObj);
      end else
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
  LJObj:      TJSONObject;
  LInstance:  TRttiInstanceType;
  LMethod:    TRTTIMEthod;
  LObj:       TOBject;
begin
  Result := '';
  LInfoBlock := Nil;
  LParts := TStringList.Create(#0, cCommaDelimiter, [soStrictDelimiter]);
  try
    LFields := JX3GetFields(Self);
    LParts.Capacity := Length(LFields);
    for LField in LFields do
      if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic, mvPublished]) then
      begin
        LObj := LField.GetValue(Self).AsObject;
        if  (LObj = nil) then
        begin
          LInstance := LField.FieldType.AsInstance;
          LMethod := LInstance.GetMethod('Create');
          LObj := LMethod.Invoke(LInstance.MetaclassType,[]).AsObject;
          LField.SetValue(Self, LObj);
          TJX3Tools.CallMethodProc('JSONCreate', LObj, [True]);
        end;
        LJObj := Nil;
        LInfoBlock := TJX3InfoBlock.Create(LField.Name, LJObj, LField, AInfoBlock.Options);
        LPart :=  TJX3Tools.CallMethodFunc('JSONSerialize', LObj, [LInfoBlock, AInOutBlock]);
        if not LPart.IsEmpty then LParts.Add(LPart.AsString);
        FreeAndNil(LInfoBlock);
        Continue;
      end;
      LRes := LParts.DelimitedText.Replace(cCommaDelimiter, ',');
      if not AInfoBlock.FieldName.IsEmpty then
      begin
        if LRes.IsEmpty then
        begin

          if Assigned(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
            TJX3Tools.RaiseException(Format('"%s" (TJX3Object) : a value is required', [AInfoBlock.FieldName]));

          if joNullToEmpty in AInfoBlock.Options then
            Result := TValue.Empty
          else
            Result := Format('"%s":null', [AInfoBlock.FieldName]);
        end else
          Result := Format('"%s":{%s}', [AInfoBlock.FieldName, LRes])
      end
    else
      Result := Format('{%s}', [LRes]);
  finally
    LParts.Free;
    LInfoBlock.Free;
  end;
end;

procedure TJX3Object.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LField:     TRTTIField;
  LJPair:     TJSONPAir;
  LJObj:      TJSONObject;
  LInfoBlock: TJX3InfoBlock;
  LNameAttr:  JX3Name;
  LName:      string;
  LInstance:  TRttiInstanceType;
  LMethod:    TRTTIMEthod;
  LObj:       TOBject;
begin
  try
    for LField in JX3GetFields(Self) do
    begin

      LName :=  TJX3Tools.NameDecode(LField.Name);
      LNameAttr := JX3Name(uJX3Rtti.JX3GetFieldAttribute(LField, JX3Name));
      if Assigned(LNameAttr) then LName := LNameAttr.Name;

      LJPair := AInfoBlock.Obj.Get(LName);
      if LJPair <> Nil then
      begin
        LJPair.Owned := False;
        LJPair.JsonValue.Owned := False;
        if (LJPair.JsonValue is TJSONObject) then
          LJObj := (LJPair.JsonValue as TJSONObject)
        else
          LJObj :=  TJSONObject.Create(LJPair);

        LObj := LField.GetValue(Self).AsObject;
        if  (LObj = nil) then
        begin
          LInstance := LField.FieldType.AsInstance;
          LMethod := LInstance.GetMethod('Create');
          LObj := LMethod.Invoke(LInstance.MetaclassType,[]).AsObject;
          LField.SetValue(Self, LObj);
          TJX3Tools.CallMethodProc('JSONCreate', LObj, [True]);
        end;

        LInfoBlock := TJX3InfoBlock.Create(LField.Name, LJObj, LField, AInfoBlock.Options);
        TJX3Tools.CallMethodProc('JSONDeserialize', LObj, [LInfoBlock, AInOutBlock]);
        FreeAndNil(LInfoBlock);
        if (Assigned(LJObj)) and not (LJPair.JsonValue is TJSONObject) then FreeAndNil(LJObj);
        LJPair.JsonValue.Owned := True;
        LJPair.Owned := True;

      end else begin

        var LAttr := JX3Default(JX3GetFieldAttribute(LField, JX3Default));
        if Assigned(LAttr) then
        begin
          LJObj := TJSONObject.Create(TJSONPair.Create('', TJSONNull.Create));
          LInfoBlock := TJX3InfoBlock.Create(LField.Name, LJObj, LField, AInfoBlock.Options);
          TJX3Tools.CallMethodProc('JSONDeserialize', LField.GetValue(Self).AsObject, [LInfoBlock, AInOutBlock]);
          FreeAndNil(LInfoBlock);
          LJObj.Free;
          Continue;
        end;

        if Assigned(JS3Required(JX3GetFieldAttribute(LField, JS3Required))) then
          TJX3Tools.RaiseException(Format('"%s" is required but not defined', [LName]));

        if (JoRaiseOnMissingField in AInfoBlock.Options)  then
          TJX3Tools.RaiseException(Format('Missing Field : %s', [LName]));
      end;
    end;
  finally
    LInfoBlock.Free;
  end;
end;

class function TJX3Object.FromJSON<T>(AJson: string; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
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

function TJX3Object.ToJSON(AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): string;
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
      Result := JSONSerialize(LInfoBlock, AInOutBlock).AsString;
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

function TJX3Object.Clone<T>(AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): T;
var
  LWatch: TStopWatch;
begin
  if (joStats in AOptions) and Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
  Result := Self.FromJSON<T>(Self.ToJSON(AOptions), AOptions, AInOutBlock);
  if (joStats in AOptions) and Assigned(AInOutBlock) then AInOutBlock.Stats.ProcessingTimeMS := LWatch.ElapsedMilliseconds;
end;

end.
