unit uJX3Object;

interface
uses
    RTTI
  , JSON
  ;

const
  cCommaDelimiter = #0;

type

  TJS3Option  = (
        joNullToEmpty
      , joRaiseException
      , joRaiseOnMissingField
      , joStats
      // Merge
      , joCloneRTTI
  );
  TJX3Options = set of TJS3Option;

  JX3Name = class(TCustomAttribute)
  public
    Name: string;
    constructor Create(const AName: string);
  end;

  JX3Default = class(TCustomAttribute)
  public
    Value: string;
    constructor Create(const AValue: string);
  end;

  JS3Required = class(TCustomAttribute);

  JX3Unmanaged = class(TCustomAttribute);

    TJX3InfoBlock = class
    Obj: TJSONObject;
    FieldName: string;
    Field: TRttiField;
    Options: TJX3Options;
    constructor Create( AFieldName: string; AObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
  end;

  TJX3InOutStats = class
    ProcessingTimeMS: Int64;
    PrimitiveCount: Int64;
    ListCount: Int64;
    DicCount: Int64;
    BooleanCount: Int64;
    NumCount: Int64;
    function OpCount: Int64;
    procedure Clear;
  end;

  TJX3InOutBlock = class
    Stats: TJX3InOutStats;
    User1: TValue;
    User2: TValue;
    User3: TValue;
    User4: TValue;
    constructor Create;
    destructor Destroy; override;
  end;

  TJX3Object = class(TObject)
  public
    constructor     Create; virtual;
    destructor      Destroy; override;

    function        JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONClone(ADest: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONMerge(ASrc: TJX3Object; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock = Nil);

    class function  FromJSON<T:class, constructor>(AJson: string; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
    class function  ToJSON(AObj: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): string;
    class function  Clone<T:class, constructor>(AObj: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;

    class function  EscapeJSONStr(const AStr: string): string; static;
    class function  NameDecode(const ToDecode: string): string; static;
    class function  NameEncode(const ToEncode: string): string; static;
    class function  FormatJSON(const AJson: string; AIndentation: Integer = 4): string;
  end;

  TJX3Obj = TJX3Object;
  TJX3    = TJX3Object;

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

constructor JX3Name.Create(const AName: string);
begin
  Name := AName;
end;

constructor JX3Default.Create(const AValue: string);
begin
  Value := AValue;
end;

constructor TJX3InfoBlock.Create( AFieldName: string; AObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
begin
  Obj := AObj;
  FieldName := AFieldName;
  Field := AField;
  Options := AOptions;
end;

procedure TJX3InOutStats.Clear;
begin
  ProcessingTimeMS  := 0;
  PrimitiveCount    := 0;
  ListCount         := 0;
  DicCount          := 0;
  BooleanCount      := 0;
  NumCount          := 0;
end;

function TJX3InOutStats.OpCount: Int64;
begin
  Result := PrimitiveCount;
  Result := Result + ListCount;
  Result := Result + DicCount;
  Result := Result + BooleanCount;
  Result := Result + NumCount;
end;

constructor TJX3InOutBlock.Create;
begin
  inherited;
  Stats := TJX3InOutStats.Create;
  Stats.Clear;
  User1 := TValue.Empty;
  User2 := TValue.Empty;
end;

destructor TJX3InOutBlock.Destroy;
begin
  Stats.Free;
  inherited;
end;

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
        LInstance := LField.FieldType.AsInstance;
        if not Assigned(LInstance) then Continue;
        LMethod := LInstance.GetMethod('Create');
        if not Assigned(LMethod) then Continue;
        LNewObj := LMethod.Invoke(LInstance.MetaclassType,[]).AsObject;
        if not Assigned(LNewObj) then Continue;
        JX3CallMethodProc('JSONCreate', LNewObj, [True]);
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
        if JX3CallMethodFunc('JSONDestroy', LObj, []).AsBoolean then
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
        if  not Assigned(LObj) then
        begin
          LObj := JX3CreateObject(LField.FieldType.AsInstance);
          JX3CallMethodProc('JSONCreate', LObj, [True]);
          LField.SetValue(Self, LObj);
          JX3CallMethodProc('JSONCreate', LObj, [True]);
        end;
        LInfoBlock := TJX3InfoBlock.Create(LField.Name, Nil, LField, AInfoBlock.Options);
        LPart :=  JX3CallMethodFunc('JSONSerialize', LObj, [LInfoBlock, AInOutBlock]);
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
            raise Exception.Create(Format('"%s" (TJX3Object) : a value is required', [AInfoBlock.FieldName]));

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
  LAttr:      JX3Name;
  LName:      string;
  LObj:       TObject;
begin
  try
    for LField in JX3GetFields(Self) do
    begin

      LName :=  NameDecode(LField.Name);
      LAttr := JX3Name(uJX3Rtti.JX3GetFieldAttribute(LField, JX3Name));
      if Assigned(LAttr) then LName := LAttr.Name;

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
          LObj := JX3CreateObject(LField.FieldType.AsInstance);
          JX3CallMethodProc('JSONCreate', LObj, [True]);
          LField.SetValue(Self, LObj);
        end;

        LInfoBlock := TJX3InfoBlock.Create(LField.Name, LJObj, LField, AInfoBlock.Options);
        JX3CallMethodProc('JSONDeserialize', LObj, [LInfoBlock, AInOutBlock]);
        FreeAndNil(LInfoBlock);
        if (Assigned(LJObj)) and not (LJPair.JsonValue is TJSONObject) then FreeAndNil(LJObj);
        LJPair.JsonValue.Owned := True;
        LJPair.Owned := True;

      end else begin

        if Assigned( JX3Default(JX3GetFieldAttribute(LField, JX3Default)) ) then
        begin
          LJObj := TJSONObject.Create(TJSONPair.Create('', TJSONNull.Create));
          LInfoBlock := TJX3InfoBlock.Create(LField.Name, LJObj, LField, AInfoBlock.Options);
          JX3CallMethodProc('JSONDeserialize', LField.GetValue(Self).AsObject, [LInfoBlock, AInOutBlock]);
          FreeAndNil(LInfoBlock);
          LJObj.Free;
          Continue;
        end;

        if Assigned(JS3Required(JX3GetFieldAttribute(LField, JS3Required))) then
          raise Exception.Create(Format('"%s" is required but not defined', [LName]));

        if (JoRaiseOnMissingField in AInfoBlock.Options)  then
          raise Exception.Create(Format('Missing Field : %s', [LName]));
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
      JX3CallMethodProc('JSONDeserialize', Result, [LInfoBlock, AInOutBlock]);
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

class function TJX3Object.ToJSON(AObj: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): string;
var
  LInfoBlock: TJX3InfoBlock;
  LWatch: TStopWatch;
begin
  LInfoBlock := Nil;
  try
  try
    if (joStats in AOptions) and Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
    LInfoBlock := TJX3InfoBlock.Create('', nil, nil, AOptions);
    Result := JX3CallMethodFunc('JSONSerialize', AObj, [LInfoBlock, AInOutBlock]).AsString;
  finally
    LInfoBlock.Free;
    if (joStats in AOptions) and Assigned(AInOutBlock) then AInOutBlock.Stats.ProcessingTimeMS := LWatch.ElapsedMilliseconds
  end;
  except
    on Ex: Exception do
    begin
      Result := '';
      if joRaiseException in AOptions then Raise;
    end;
  end;
end;

class function TJX3Object.Clone<T>(AObj: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
var
  LWatch: TStopWatch;
begin
  Result := Nil;
  try
  try
    if (joStats in AOptions) and Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
    Result := T.Create;
    JX3CallMethodProc('JSONCreate', Result, [True]);
    JX3CallMethodProc('JSONClone', AObj, [Result, TValue.From<TJX3Options>(AOptions), AInOutBlock])
  finally
    if (joStats in AOptions) and Assigned(AInOutBlock) then AInOutBlock.Stats.ProcessingTimeMS := LWatch.ElapsedMilliseconds
  end;
  except
    on Ex: Exception do
    begin
      FreeAndNil(Result);
      if joRaiseException in AOptions then Raise;
    end;
  end;
end;

procedure TJX3Object.JSONClone(ADest: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  LSrcField:  TRTTIField;
  LDestField: TRTTIField;
  LNewObj:    TObject;
  begin
  for LDestField in JX3GetFields(ADest) do
    begin
    for LSrcField in JX3GetFields(Self) do
    begin
      if LSrcField.Name = LDestField.Name then
      begin
        LNewObj := LDestField.GetValue(ADest).AsObject;
        if not Assigned(LNewObj) then
        begin
          LNewObj := JX3CreateObject(LDestField.FieldType.AsInstance);
          JX3CallMethodProc('JSONCreate', LNewObj, [True]);
        end;
        JX3CallMethodProc('JSONClone', LSrcField.GetValue(Self).AsObject, [LNewObj,  TValue.From<TJX3Options>(AOptions), AInOutBlock]);
        LDestField.SetValue(ADest, LNewObj);
        continue;
      end;
    end;
  end;
end;

procedure TJX3Object.JSONMerge(ASrc: TJX3Object; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  LField:     TRTTIField;
  LInstance:  TRttiInstanceType;
  LMethod:    TRTTIMEthod;
  LObj:       TObject;
begin
  for LField in JX3GetFields(Self) do
  begin
    for var LSrcField in JX3GetFields(ASrc) do
    begin
      if LField.Name = LSrcField.Name then
      begin
        LObj := LField.GetValue(Self).AsObject;
        if not Assigned(LObj) then
        begin
          LInstance := LField.FieldType.AsInstance;
          if not Assigned(LInstance) then Continue;
          LMethod := LInstance.GetMethod('Create');
          if not Assigned(LMethod) then Continue;
          LObj := LMethod.Invoke(LInstance.MetaclassType,[]).AsObject;
          JX3CallMethodProc('JSONCreate', LObj, [True]);
          JX3CallMethodProc('JSONMerge', LObj, [ LSrcField.GetValue(ASrc).AsObject, TValue.From<TJX3Options>(AMergeOpts), AInOutBlock]);
          LField.SetValue(Self, LObj);
          continue;
        end;
        JX3CallMethodProc('JSONMerge', LObj, [ LSrcField.GetValue(ASrc).AsObject, TValue.From<TJX3Options>(AMergeOpts), AInOutBlock]);
        continue
      end;
    end;
  end;
end;

class function TJX3Object.FormatJSON(const AJson: string; AIndentation: Integer): string;
var
  TmpJson: TJsonObject;
begin
  TmpJson := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
  Result := TJSONAncestor(TmpJson).Format(AIndentation);
  FreeAndNil(TmpJson);
end;

class function TJX3Object.EscapeJSONStr(const AStr: string): string;
var
  LP: PChar;
  LEndP: PChar;
  // LStr : TJSONString;
begin
  (*
  LStr := TJSONString.Create(AStr);
  Result := LStr.ToJSON([TJSONAncestor.TJSONOutputOption.EncodeBelow32]);
  LStr.Free;
  Exit;
  *)
  Result := '';
  LP := PChar(Pointer(AStr));
  LEndP := LP + Length(AStr);
  while LP < LendP do
  begin
    case LP^ of
      #0 .. #31, '\', '"':
        Result := Result + '\' + LP^;
      else
        Result := Result + LP^;
    end;
    Inc(LP);
  end;
end;

class function TJX3Object.NameDecode(const ToDecode: string): string;
var
  Index: Integer;
  CharCode: Integer;
begin;
  if Pos('_', ToDecode) <> 1 then Exit(ToDecode);
  Result := ''; Index := 2;
  while (Index <= Length(ToDecode)) do
    begin
      if (ToDecode[Index] = '_') and TryStrToInt('$' + Copy(ToDecode, Index + 1, 2), CharCode) then
      begin
        Result := Result + Chr(CharCode);
        Inc(Index, 3);
      end
        else
      begin
        Result := Result + ToDecode[Index];
        Inc(Index, 1);
      end;
    end;
end;

class function TJX3Object.NameEncode(const ToEncode: string): string;
var
  Encoded: Boolean;
begin
  Result := ''; Encoded := False;
  for var i := 1 to Length(ToEncode) do
    if true then //ToEncode[i].IsLetterOrDigit  then
      Result := Result + ToEncode[i]
    else begin
      Encoded := True;
      Result := Result + '_' + Format('%2x', [Ord(ToEncode[i])]);
    end;
  if Encoded then Result := '_' + Result;
end;

end.
