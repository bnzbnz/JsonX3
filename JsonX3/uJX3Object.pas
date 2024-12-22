(*****************************************************************************
The MIT License (MIT)

Copyright (c) 2020-2025 Laurent Meyer JsonX3@ea4d.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*****************************************************************************)
unit uJX3Object;

{.$DEFINE JX3SPEEDUP} // Speed vs Memory, only if you parse multiple large Json (10-20% faster, your mileage may vary...)

interface
uses
  System.Generics.Collections
  , RTTI
  , JSON
  , uJX3MiniPool
  ;

type

  TJS3Option  = (
        joNullToEmpty
      , joRaiseException
      , joRaiseOnMissingField
      , joStats
      // Merge,
      , jomOverload
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
    //  Results
    IsEmpty: Boolean;
    Part: string;
    // Helpers
    constructor Create; overload;
    constructor Create( AFieldName: string; AObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options); overload;
    procedure   Init( AFieldName: string; AObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
    procedure   SetJSON(AJson: string; AToEmpty: Boolean = False);
  end;

  TJX3InOutStats = class
    ProcessingTimeMS: Int64;
    FieldCount: Int64;
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

  TJX3Primitive = class(TObject)
  protected
    function          GetIsNull: Boolean; virtual; abstract;
    procedure         SetIsNull(ANull: Boolean); virtual; abstract;
  published
    property IsNull:  Boolean read GetIsNull write SetIsNull;
    property Null:    Boolean read GetIsNull write SetIsNull;
  end;

  TJX3Object = class(TObject)
  public
    constructor     Create; virtual;
    destructor      Destroy; override;

    // procedure    JSONCreate(AManaged: Boolean);
    procedure       JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONClone(ADest: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONMerge(ASrc: TObject; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock = Nil);
    // function     JSONDestroy: Boolean;

    class function  FromJSON<T:class, constructor>(const AJson: string; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
    class function  ToJSON(AObj: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): string;
    class function  Clone<T:class, constructor>(AObj: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
    function        Merge(ASrc: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): Boolean;

    // Internals / Utils
    class procedure VarEscapeJSONStr(var AStr: string); overload; static;
    class function  EscapeJSONStr(const AStr: string): string; overload; static;
    class function  NameDecode(const ToDecode: string): string; static;
    class function  NameEncode(const ToEncode: string): string; static;
    class function  FormatJSON(const AJson: string; AIndentation: Integer = 4): string;
    class function  JsonListToJsonString(const AList: TList<string>): string;

  end;

  TJX3Obj = TJX3Object;
  TJX3    = TJX3Object;

var
{$IFDEF JX3SPEEDUP}
  GJSX3PooledStr:  TJX3MiniPool;
  GJSX3PooledNum:  TJX3MiniPool;
  GJSX3PooledBool: TJX3MiniPool;
{$ENDIF JX3SPEEDUP}
  GJSX3PooledJSON: TJX3MiniPool;

implementation
uses
    System.Diagnostics
  , System.Character
  , SysUtils
  , Classes
  , TypInfo
  , uJX3String
  , uJX3Number
  , uJX3Boolean
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

constructor TJX3InfoBlock.Create;
begin
  IsEmpty := True;
end;

constructor TJX3InfoBlock.Create(AFieldName: string; AObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
begin
  Init(AFieldName, AObj, AField, AOptions);
end;

procedure TJX3InfoBlock.Init(AFieldName: string; AObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
begin
  Obj := AObj;
  FieldName := AFieldName;
  Field := AField;
  Options := AOptions;
  IsEmpty := True;
end;

procedure TJX3InfoBlock.SetJSON(AJson: string; AToEmpty: Boolean);
begin
  Self.IsEmpty := AToEmpty;
  Part := AJson;
end;

procedure TJX3InOutStats.Clear;
begin
  ProcessingTimeMS  := 0;
  FieldCount        := 0;
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
  LField:     TRTTIField;
  LNewObj:    TObject;
begin
  inherited Create;
  for LField in TxRTTI.GetFields(Self) do
  begin
    if  (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic]) then
    begin
      if not Assigned(TxRTTI.GetFieldAttribute(LField, JX3Unmanaged)) then
      begin
        if  LField.FieldType.AsInstance.MetaclassType = TJX3String then
        {$IFDEF JX3SPEEDUP}
           LNewObj := GJSX3PooledStr.Get<TJX3String>
        {$ELSE}
           LNewObj := TJX3String.Create
        {$ENDIF JX3SPEEDUP}
        else if  LField.FieldType.AsInstance.MetaclassType = TJX3Num then
        {$IFDEF JX3SPEEDUP}
           LNewObj := GJSX3PooledNum.Get<TJX3Num>
        {$ELSE}
           LNewObj := TJX3Num.Create
        {$ENDIF JX3SPEEDUP}
        else if  LField.FieldType.AsInstance.MetaclassType = TJX3Bool then
        {$IFDEF JX3SPEEDUP}
           LNewObj := GJSX3PooledBool.Get<TJX3Bool>
        {$ELSE}
           LNewObj := TJX3Bool.Create
        {$ENDIF JX3SPEEDUP}
         else
           LNewObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
        if not Assigned(LNewObj) then Continue;
        if not (LNewObj is TJX3Primitive) then TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
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
  LFields := TxRTTI.GetFields(Self);
  for LField in LFields do
    if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic]) then
    begin
      LObj := LField.GetValue(Self).AsObject;
      if not Assigned(LObj) then Continue;
      if Assigned(TxRTTI.GetFieldAttribute(LField, JX3Unmanaged)) then
      begin
        if not (LObj is TJX3Object) then
          if TxRTTI.CallMethodFunc('JSONDestroy', LObj, []).AsBoolean then
            FreeAndNil(LObj);
      end else
        {$IFDEF JX3SPEEDUP}
        if LObj.ClassType = TJX3String then begin TJX3String(LObj).IsNull := True; GJSX3PooledStr.Put(LObj); end
        else if LObj.ClassType = TJX3Num then begin TJX3Num(LObj).IsNull := True; GJSX3PooledNum.Put(LObj); end
        else if LObj.ClassType = TJX3Bool then begin TJX3Bool(LObj).IsNull := True; GJSX3PooledBool.Put(LObj); end
        else FreeAndNil(LObj);
        {$ELSE}
        FreeAndNil(LObj);
        {$ENDIF}
        LField.SetValue(Self,Nil);
      end;
  inherited;
end;

procedure TJX3Object.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LField:     TRTTIField;
  LFields:    TArray<TRTTIField>;
  LParts:     TList<string>;
  LRes:       string;
  LInfoBlock: TJX3InfoBlock;
  LObj:       TOBject;
begin
  LInfoBlock := TJX3InfoBlock.Create;
  LParts := TList<string>.Create;
  try
    LFields := TxRTTI.GetFields(Self);
    LParts.Capacity := Length(LFields);
    for LField in LFields do
      if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic]) then
      begin
        if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.FieldCount);
        LObj := LField.GetValue(Self).AsObject;
        if  not Assigned(LObj) then
        begin
          if  LField.FieldType.AsInstance.MetaclassType = TJX3String then
          {$IFDEF JX3SPEEDUP}
            LObj := GJSX3PooledStr.Get<TJX3String>
          {$ELSE}
            LObj := TJX3String.Create
          {$ENDIF JX3SPEEDUP}
          else if  LField.FieldType.AsInstance.MetaclassType = TJX3Num then
            {$IFDEF JX3SPEEDUP}
            LObj := GJSX3PooledNum.Get<TJX3Num>
            {$ELSE}
            LObj := TJX3Num.Create
            {$ENDIF JX3SPEEDUP}
          else if  LField.FieldType.AsInstance.MetaclassType = TJX3Bool then
            {$IFDEF JX3SPEEDUP}
            LObj := GJSX3PooledBool.Get<TJX3Bool>
          {$ELSE}
           LObj := TJX3Bool.Create
          {$ENDIF JX3SPEEDUP}
         else
         begin
            LObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
            if not (LObj is TJX3Primitive) then TxRTTI.CallMethodProc('JSONCreate', LObj, [True]);
          end;
          LField.SetValue(Self, LObj);
        end;
        LInfoBlock.Init(LField.Name, Nil, LField, AInfoBlock.Options);
        if LObj is TJX3String  then TJX3String(LObj).JSONSerialize(LInfoBlock, AInOutBlock)
        else if LObj is TJX3Number  then TJX3Number(LObj).JSONSerialize(LInfoBlock, AInOutBlock)
        else if LObj is TJX3Boolean  then TJX3Boolean(LObj).JSONSerialize(LInfoBlock, AInOutBlock)
        else TxRTTI.CallMethodFunc('JSONSerialize', LObj, [LInfoBlock, AInOutBlock]);
        if not LInfoBlock.IsEmpty then LParts.Add(LInfoBlock.Part);
        Continue;
      end;
      LRes := JsonListToJsonString(LParts);
      if not AInfoBlock.FieldName.IsEmpty then
      begin
        if LRes.IsEmpty then
        begin
          if Assigned(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
            raise Exception.Create(Format('"%s" (TJX3Object) : a value is required', [AInfoBlock.FieldName]));

          if joNullToEmpty in AInfoBlock.Options then Exit;
          AInfoBlock.IsEmpty := False;
          AInfoBlock.Part := '"' + AInfoBlock.FieldName + '":null';
        end else begin
          AInfoBlock.IsEmpty := False;
          AInfoBlock.Part := '"' + AInfoBlock.FieldName + '":{' + LRes + '}';
        end;
      end
      else begin
        AInfoBlock.IsEmpty := False;
        AInfoBlock.Part := '{' + LRes + '}';
      end;
  finally
    LParts.Free;
    LInfoBlock.Free;
  end;
end;

procedure TJX3Object.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LField:       TRTTIField;
  LJPair:       TJSONPAir;
  LJObj:        TJSONObject;
  LInfoBlock:   TJX3InfoBlock;
  LName:        string;
  LObj:         TObject;
  LFieldFound:  Boolean;
  LAttr:        TCustomAttribute;
  LJPairList:   TStringList;
begin
  LJPairList := Nil;
  LInfoBlock := TJX3InfoBlock.Create;
  try

    for LField in TxRTTI.GetFields(Self) do
    begin
      if not (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic]) then Continue;

      if (JoRaiseOnMissingField in AInfoBlock.Options) and (Length(TxRTTI.GetFields(Self)) < AInfoBlock.Obj.count) then
      begin
        LJPairList := TStringList.Create;
        for LJPair in AInfoBlock.Obj do LJPairList.Add(LJPair.JsonString.Value);
        raise Exception.Create(Format('Missing Property(ies) in class %s, from JOSN fields: %s%s', [Self.ClassName, sLineBreak, LJPairList.Text]));
      end;

      LName := NameDecode(LField.Name);
      LAttr := JX3Name(TxRTTI.GetFieldAttribute(LField, JX3Name));
      if Assigned(LAttr) then LName := JX3Name(LAttr).Name;

      LFieldFound := False;
      for LJPair in  AInfoBlock.Obj do
      begin

        if LJPair.JsonValue is TJSONNull then Break;

        if LName = LJPair.JsonString.Value then
        begin

          if LJPair.JsonValue is TJSONNull then Break;
          if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.FieldCount);
          LFieldFound := True;  // ?? Hint: neve used ?? WHY ??

          LJPair.Owned := False;
          LJPair.JsonString.Owned := False;
          LJPair.JsonValue.Owned := False;
          if (LJPair.JsonValue is TJSONObject) then
            LJObj := (LJPair.JsonValue as TJSONObject)
          else
          begin
            LJObj := GJSX3PooledJSON.Get<TJSONObject>;
            LJObj.AddPair(LJPair);
          end;

          LObj := LField.GetValue(Self).AsObject;
          if not Assigned(LObj) then
          begin
            LObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
            if not (LObj is TJX3Primitive) then TxRTTI.CallMethodProc('JSONCreate', LObj, [True]);
            LField.SetValue(Self, LObj);
          end;

          LInfoBlock.Init(LField.Name, LJObj, LField, AInfoBlock.Options);
          if LObj is TJX3String then TJX3String(LObj).JSONDeserialize(LInfoBlock, AInOutBlock)
          else if LObj is TJX3Number then TJX3Number(LObj).JSONDeserialize(LInfoBlock, AInOutBlock)
          else if LObj is TJX3Boolean then TJX3Boolean(LObj).JSONDeserialize(LInfoBlock, AInOutBlock)
          else TxRTTI.CallMethodProc('JSONDeserialize', LObj, [LInfoBlock, AInOutBlock]);

          if not (LJPair.JsonValue is TJSONObject) then
          begin
            LJObj.Pairs[0].JsonString.Owned := False;
            LJObj.Pairs[0].JsonValue.Owned := False;
            LJObj.RemovePair(LJObj.Pairs[0].JsonString.Value);
            GJSX3PooledJSON.Put(LJObj);
          end;
          LJPair.JsonString.Owned := True;
          LJPair.JsonValue.Owned := True;
          LJPair.Owned := True;

          Break;
        end;

        if not LFieldFound then
        begin

          LAttr := TxRTTI.GetFieldAttribute(LField, JX3Default);
          if Assigned(LAttr) then
          begin
            LObj := LField.GetValue(Self).AsObject;
            if LObj is TJX3String then TJX3String(LObj).Value := JX3Default(LAttr).Value
            else if LObj is TJX3Number then TJX3Number(LObj).Value := JX3Default(LAttr).Value
            else if LObj is TJX3Boolean then TJX3Boolean(LObj).Value := JX3Default(LAttr).Value.ToBoolean
            else raise Exception.Create(Format('Class: %s : "%s", only Primitives may have a default Value (%s)', [Self.ClassName, LName, JX3Default(LAttr).Value]));
            continue;
          end;

          if Assigned(JS3Required(TxRTTI.GetFieldAttribute(LField, JS3Required))) then
            raise Exception.Create(Format('Class: %s : "%s" is required but not defined', [Self.ClassName, LName]));

          if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.FieldCount);

        end;
        Continue;
      end;
    end;

  finally
    LJPairList.Free;
    LInfoBlock.Free;
  end;
end;

class function TJX3Object.FromJSON<T>(const AJson: string; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
var
  LInfoBlock: TJX3InfoBlock;
  LWatch:     TStopWatch;
  LJObj:      TJSONObject;
begin
  LInfoBlock := Nil;
  LJObj := Nil;
  try
    if (joStats in AOptions) and Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
    Result := T.Create;
    try
      LJObj := TJSONObject.ParseJSONValue(AJson, True, joRaiseException in AOptions) as TJSONObject;
      LInfoBlock := TJX3InfoBlock.Create( '', LJObj, Nil, AOptions);
      TxRTTI.CallMethodProc('JSONDeserialize', Result, [LInfoBlock, AInOutBlock]);
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
  LWatch:     TStopWatch;
begin
  LInfoBlock := Nil;
  try
  try
    if (joStats in AOptions) and Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
    LInfoBlock := TJX3InfoBlock.Create('', nil, nil, AOptions);
    TxRTTI.CallMethodProc('JSONSerialize', AObj, [LInfoBlock, AInOutBlock]);
    if not LInfoBlock.IsEmpty then Result := LInfoBlock.Part;
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
    if not (Result is TJX3Primitive) then TxRTTI.CallMethodProc('JSONCreate', Result, [True]);
    TxRTTI.CallMethodProc('JSONClone', AObj, [Result, TValue.From<TJX3Options>(AOptions), AInOutBlock])
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

function TJX3Object.Merge(ASrc: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): Boolean;
var
  LWatch: TStopWatch;
begin
  Result := False;
  try
  try
    if (joStats in AOptions) and Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
    TxRTTI.CallMethodProc('JSONMerge', Self, [ASrc, TValue.From<TJX3Options>(AOptions), AInOutBlock]);
    Result := True;
  finally
    if (joStats in AOptions) and Assigned(AInOutBlock) then AInOutBlock.Stats.ProcessingTimeMS := LWatch.ElapsedMilliseconds
  end;
  except
    on Ex: Exception do
    begin
      if joRaiseException in AOptions then Raise;
    end;
  end;
end;

procedure TJX3Object.JSONClone(ADest: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  LSrcField:  TRTTIField;
  LDestField: TRTTIField;
  LObj:       TObject;
  LNewObj:    TObject;
  LSrc:       TArray<TRTTIField>;
begin
  LSrc := TxRTTI.GetFields(Self);
  for LDestField in TxRTTI.GetFields(ADest) do
    begin
    for LSrcField in LSrc do
    begin
      if LSrcField.Name = LDestField.Name then
      begin
        if (joStats in AOptions) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.FieldCount);
        LNewObj := LDestField.GetValue(ADest).AsObject;
        if not Assigned(LNewObj) then
        begin
        {$IFDEF JX3SPEEDUP}
          if  LDestField.FieldType.AsInstance.MetaclassType = TJX3String then
            LNewObj := GJSX3PooledStr.Get<TJX3String>
          else if  LDestField.FieldType.AsInstance.MetaclassType = TJX3Num then
            LNewObj := GJSX3PooledNum.Get<TJX3Num>
          else if  LDestField.FieldType.AsInstance.MetaclassType = TJX3Bool then
            LNewObj := GJSX3PooledBool.Get<TJX3Bool>
          else
        {$ENDIF JX3SPEEDUP}
          begin
            LNewObj := TxRTTI.CreateObject(LDestField.FieldType.AsInstance);
            if not (LNewObj is TJX3Primitive) then TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
          end;
          LDestField.SetValue(ADest, LNewObj);
        end;
        LObj := LSrcField.GetValue(Self).AsObject;
        if LObj is TJX3String  then TJX3String(LObj).JSONClone(TJX3String(LNewObj), AOptions, AInOutBlock)
        else if LObj is TJX3Number  then TJX3Number(LObj).JSONClone(TJX3Number(LNewObj), AOptions, AInOutBlock)
        else if LObj is TJX3Boolean  then TJX3Boolean(LObj).JSONClone(TJX3Boolean(LNewObj), AOptions, AInOutBlock)
        else TxRTTI.CallMethodProc('JSONClone', LObj, [LNewObj,  TValue.From<TJX3Options>(AOptions), AInOutBlock]);
        continue;
      end;
    end;
  end;
end;

procedure TJX3Object.JSONMerge(ASrc: TObject; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  LField:     TRTTIField;
  LObj:       TObject;
  LSrcField:  TRTTIField;
  LSrc:       TArray<TRTTIField>;
begin
  LSrc := TxRTTI.GetFields(ASrc);
  for LField in TxRTTI.GetFields(Self) do
  begin
    for LSrcField in LSrc do
    begin
      if LField.Name = LSrcField.Name then
      begin
        if (joStats in AMergeOpts) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.FieldCount);
        LObj := LField.GetValue(Self).AsObject;
        if not Assigned(LObj) then
        begin
        {$IFDEF JX3SPEEDUP}
          if  LField.FieldType.AsInstance.MetaclassType = TJX3String then
            LObj := GJSX3PooledStr.Get<TJX3String>
          else if  LField.FieldType.AsInstance.MetaclassType = TJX3Num then
            LObj := GJSX3PooledNum.Get<TJX3Num>
          else if  LField.FieldType.AsInstance.MetaclassType = TJX3Bool then
           LObj := GJSX3PooledBool.Get<TJX3Bool>
          else
        {$ENDIF JX3SPEEDUP}
          begin
            LObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
            if (not (LObj is TJX3Object)) and (not (LObj is TJX3String)) and (not (LObj is TJX3Number)) and (not (LObj is TJX3Boolean))  then
              TxRTTI.CallMethodProc('JSONCreate', LObj, [True]);
          end;
        end;
        if LObj is TJX3String then TJX3String(LObj).JSONMerge(TJX3String(LSrcField.GetValue(ASrc).AsObject), AMergeOpts, AInOutBlock)
        else if LObj is TJX3Number then TJX3Number(LObj).JSONMerge(TJX3Number(LSrcField.GetValue(ASrc).AsObject), AMergeOpts, AInOutBlock)
        else if LObj is TJX3Boolean then TJX3Boolean(LObj).JSONMerge(TJX3Boolean(LSrcField.GetValue(ASrc).AsObject), AMergeOpts, AInOutBlock)
        else
        TxRTTI.CallMethodProc('JSONMerge', LObj, [ LSrcField.GetValue(ASrc).AsObject, TValue.From<TJX3Options>(AMergeOpts), AInOutBlock]);
        continue;
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

class procedure TJX3Object.VarEscapeJSONStr(var AStr: string);
const
  HexChars: array[0..15] of Char = '0123456789abcdef';
var
  LP: PChar;
  LEndP: PChar;
  LSb: TStringBuilder;
  LMatch: Pointer;
begin
  LMatch := nil ;
  LP := PChar(Pointer(AStr));
  LEndP := LP + Length(AStr);
  while LP < LendP do
  begin
    case LP^ of
      #0..#31, '\', '"' : begin LMatch := LP; Break; end;
    end;
    Inc(LP);
  end;

  if not Assigned(LMatch) then Exit;

  LSb := TStringBuilder.Create(Copy(AStr, 1, LMatch - PChar(Pointer(AStr))));
  LP := LMatch;
  while LP < LendP do
  begin
    case LP^ of
      #0..#7, #11, #14..#31:
        begin
          LSb.Append('\u00');
          LSb.Append(HexChars[Word(LP^) shr 4]);
          LSb.Append(HexChars[Word(LP^) and $F]);
        end;
      #8: LSb.Append('\b');
      #9: LSb.Append('\t');
      #10: LSb.Append('\n');
      #12: LSb.Append('\f');
      #13: LSb.Append('\r');
      '\': LSb.Append('\\');
      '"': LSb.Append('\"');
    else
      LSb.Append(LP^);
    end;
    Inc(LP);
  end;
  AStr := LSb.ToString;
  LSb.Free;
end;

class function TJX3Object.EscapeJSONStr(const AStr: string): string;
begin
  Result := AStr;
  VarEscapeJSONStr(Result);
end;

class function TJX3Object.JsonListToJsonString(const AList: TList<string>): string;
var
  LSb:  TStringBuilder;
  LIdx: integer;
begin
  if AList.Count = 0 then Exit('');
  if AList.Count = 1 then Exit(AList[0]);
  LSb := TStringBuilder.Create;
  for LIdx:= 0 to AList.Count -1 do
  begin
    LSb.Append(AList[LIdx]);
    if LIdx <> AList.Count -1 then LSb.Append(',') ;
  end;
  Result := LSb.ToString;
  LSb.Free;
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
    if ToEncode[i].IsLetterOrDigit  then
      Result := Result + ToEncode[i]
    else begin
      Encoded := True;
      Result := Result + '_' + Format('%2x', [Ord(ToEncode[i])]);
    end;
  if Encoded then Result := '_' + Result;
end;

initialization
  {$IFDEF JX3SPEEDUP}
  GJSX3PooledStr  := TJX3MiniPool.GetInstance<TJX3String>(50000);
  GJSX3PooledNum  := TJX3MiniPool.GetInstance<TJX3Number>(10000);
  GJSX3PooledBool := TJX3MiniPool.GetInstance<TJX3Boolean>(5000);
  {$ENDIF JX3SPEEDUP}
  GJSX3PooledJSON := TJX3MiniPool.GetInstance<TJSONObject>(100);
finalization
  GJSX3PooledJSON.Free;
  {$IFDEF JX3SPEEDUP}
  GJSX3PooledStr.Free;
  GJSX3PooledNum.Free;
  GJSX3PooledBool.Free;
  {$ENDIF JX3SPEEDUP}
end.
