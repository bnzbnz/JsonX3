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

interface
uses
  System.Generics.Collections
  , RTTI
  , JSON
  , SysUtils
  , uJX3MiniPool
  ;

type

  TJS3Option  = (
        joNullToEmpty
      , joRaiseException
      , joRaiseOnMissingField
      , joStats
      //Merge
      , jmoOverride
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

  TJX3InOutBlock = class
    User1: TValue;
    User2: TValue;
    User3: TValue;
    User4: TValue;
    constructor Create;
    destructor Destroy; override;
  end;

  TJX3Abstract = class(TObject)
  protected
    function          GetIsNull: Boolean; virtual; abstract;
    procedure         SetIsNull(ANull: Boolean = True); virtual; abstract;
    procedure         SetDefaultValue(AVal: string); virtual; abstract;
  public
    procedure         JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil); virtual; abstract;
    procedure         JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil); virtual; abstract;
    procedure         JSONClone(ADest: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil); virtual; abstract;
    procedure         JSONMerge(AMergedWith: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock = Nil); virtual; abstract;

    property Null:  Boolean read  GetIsNull write SetIsNull;
    property IsNull:  Boolean read  GetIsNull write SetIsNull;
  end;

  TJX3Object = class(TJX3Abstract)
  protected
    function        GetIsNull: Boolean; override;        
    procedure       SetIsNull(ANull: Boolean); override;
    procedure       SetDefaultValue(AVal: string); override;
  public
    constructor     Create; virtual;
    destructor      Destroy; override;

    // procedure    JSONCreate(AManaged: Boolean);
    procedure       JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil); override;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil); override;
    procedure       JSONClone(ADest: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil); override;
    procedure       JSONMerge(AMergedWith: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock = Nil); override;
    // function     JSONDestroy: Boolean;

    class function  FromJSON<T:class, constructor>(const AJson: string; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
    class function  ToJSON(AObj: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): string;
    class function  Clone<T:class, constructor>(AObj: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
    class function  Merge(AToBeMerged, AMergedWith: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): Boolean;

    // Internals / Utils
    class procedure VarEscapeJSONStr(var AStr: string); overload; static;
    class function  EscapeJSONStr(const AStr: string): string; overload; static;
    class function  NameDecode(const ToDecode: string): string; static;
    class function  NameEncode(const ToEncode: string): string; static;
    class function  JsonListToJsonString(const AList: TList<string>): string;
    class function  FormatJSON(const AJson: string; AIndentation: Integer = 4): string;

    // Tools

    class function LoadFromFile(const AFilename: string; var AStr: string; AEncoding: TEncoding): Int64;
    class function SaveToFile(const Filename: string; const AStr: string; AEncoding: TEncoding): Int64;

  end;

  TJX3Obj = TJX3Object;
  TJX3    = TJX3Object;

var
  GJSX3PooledJSON: TJX3MiniPool;

implementation
uses
    System.Diagnostics
  , System.Character
  , TypInfo
  , Classes
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

constructor TJX3InOutBlock.Create;
begin
  inherited;
  User1 := TValue.Empty;
  User2 := TValue.Empty;
  User3 := TValue.Empty;
  User4 := TValue.Empty;
end;

destructor TJX3InOutBlock.Destroy;
begin
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
        if LField.FieldType.AsInstance.MetaclassType  = TJX3String then LNewObj := TJX3String.Create
        else if LField.FieldType.AsInstance.MetaclassType = TJX3Number then LNewObj := TJX3Number.Create
        else if LField.FieldType.AsInstance.MetaclassType = TJX3Boolean then LNewObj := TJX3Boolean.Create
        else LNewObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
        if not Assigned(LNewObj) then Continue;
        if not (LNewObj is TJX3Abstract) then TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
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
      end else FreeAndNil(LObj);
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
        LObj := LField.GetValue(Self).AsObject;
        if  not Assigned(LObj) then Continue;  // Unmanaged missing value
        LInfoBlock.Init(LField.Name, Nil, LField, AInfoBlock.Options);
        if LObj is TJX3Abstract  then TJX3Abstract(LObj).JSONSerialize(LInfoBlock, AInOutBlock)
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
        LFieldFound := AInfoBlock.Obj.Count > 0;
        for LJPair in  AInfoBlock.Obj do
         if LField.Name = LJPair.JsonString.Value then
         begin
           LFieldFound := True;
           Break;
         end;
        if Not LFieldFound then
          raise Exception.Create(Format('Missing Property %S in class %s, from JOSN fields: %s%s', [LField.Name, Self.ClassName, sLineBreak, LJPairList.Text]));
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
          LFieldFound := True;  // ?? Hint: never used ?? WHY ??

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
            if not (LObj is TJX3Abstract) then TxRTTI.CallMethodProc('JSONCreate', LObj, [True]);
            LField.SetValue(Self, LObj);
          end;

          LInfoBlock.Init(LField.Name, LJObj, LField, AInfoBlock.Options);
          if LObj is TJX3Abstract then TJX3Abstract(LObj).JSONDeserialize(LInfoBlock, AInOutBlock)
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
            if LObj is TJX3Abstract then TJX3Abstract(LObj).SetDefaultValue( JX3Default(LAttr).Value )
            else raise Exception.Create(Format('Class: %s : "%s", only Primitives may have a default Value (%s)', [Self.ClassName, LName, JX3Default(LAttr).Value]));
            continue;
          end;

          if Assigned(JS3Required(TxRTTI.GetFieldAttribute(LField, JS3Required))) then
            raise Exception.Create(Format('Class: %s : "%s" is required but not defined', [Self.ClassName, LName]));

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
  LJObj:      TJSONObject;
begin
  LInfoBlock := Nil;
  LJObj := Nil;
  try
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
  end;
end;

class function TJX3Object.ToJSON(AObj: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): string;
var
  LInfoBlock: TJX3InfoBlock;
begin
  LInfoBlock := Nil;
  try
  try
    LInfoBlock := TJX3InfoBlock.Create('', nil, nil, AOptions);
    TxRTTI.CallMethodProc('JSONSerialize', AObj, [LInfoBlock, AInOutBlock]);
    if not LInfoBlock.IsEmpty then Result := LInfoBlock.Part;
  finally
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

class function TJX3Object.Clone<T>(AObj: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): T;
var
  LWatch: TStopWatch;
begin
  Result := Nil;
  try
  try
    if (joStats in AOptions) and Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
    Result := T.Create;
    if not (Result is TJX3Abstract) then TxRTTI.CallMethodProc('JSONCreate', Result, [True]);
    TxRTTI.CallMethodProc('JSONClone', AObj, [Result, TValue.From<TJX3Options>(AOptions), AInOutBlock])
  finally
  end;
  except
    on Ex: Exception do
    begin
      FreeAndNil(Result);
      if joRaiseException in AOptions then Raise;
    end;
  end;
end;

class function TJX3Object.Merge(AToBeMerged, AMergedWith: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): Boolean;
var
  LWatch: TStopWatch;
begin
  Result := False;
  try
  try
    if (joStats in AOptions) and Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
    TxRTTI.CallMethodProc('JSONMerge', AToBeMerged, [AMergedWith, TValue.From<TJX3Options>(AOptions), AInOutBlock]);
    Result := True;
  finally
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
        LNewObj := LDestField.GetValue(ADest).AsObject;
        if not Assigned(LNewObj) then // Unmanaged
        begin
          LNewObj := TxRTTI.CreateObject(LDestField.FieldType.AsInstance);
          if not (LNewObj is TJX3Abstract) then TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
          LDestField.SetValue(ADest, LNewObj);
        end;
        LObj := LSrcField.GetValue(Self).AsObject;
        if LObj is TJX3Abstract  then TJX3Abstract(LObj).JSONClone(LNewObj, AOptions, AInOutBlock)
        else TxRTTI.CallMethodProc('JSONClone', LObj, [LNewObj,  TValue.From<TJX3Options>(AOptions), AInOutBlock]);
        Continue;
      end;
    end;
  end;
end;

procedure TJX3Object.JSONMerge(AMergedWith: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  LField:     TRTTIField;
  LObj, LSrcObj: TObject;
  LSrcField:  TRTTIField;
begin
  for LField in TxRTTI.GetFields(Self) do
  begin
    for LSrcField in  TxRTTI.GetFields(AMergedWith) do
    begin
      if LField.Name = LSrcField.Name then
      begin
        LObj := LField.GetValue(Self).AsObject;
        LSrcObj := LSrcField.GetValue(AMergedWith).AsObject;
        if not Assigned(LSrcObj) then // Unmanaged
        begin
          LSrcObj := TxRTTI.CreateObject(LSrcField.FieldType.AsInstance);
          if not (LSrcObj is TJX3Abstract) then TxRTTI.CallMethodProc('JSONCreate', LSrcObj, [True]);
          LSrcField.SetValue(AMergedWith, LSrcObj);
        end;
        if not Assigned(LObj) then // Unmanaged
        begin
          LObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
          if not (LSrcObj is TJX3Abstract) then TxRTTI.CallMethodProc('JSONCreate', LSrcObj, [True]);
          LField.SetValue(Self, LObj);
        end;
        if LObj is TJX3Abstract then TJX3Abstract(LObj).JSONMerge(TJX3Boolean(LSrcObj), AOptions, AInOutBlock)
        else TxRTTI.CallMethodProc('JSONMerge', LObj, [ LSrcObj, TValue.From<TJX3Options>(AOptions), AInOutBlock]);
        continue;
      end;
    end;
  end;
end;

function  TJX3Object.GetIsNull: Boolean;
begin        
  Result := False;
end;

procedure TJX3Object.SetIsNull(ANull: Boolean);
var
  LField:       TRTTIField;
  LFields:      TArray<TRttiField>;
  LObj:         TOBject;
begin
  LFields := TxRTTI.GetFields(Self);
  for LField in LFields do
    if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic]) then
    begin
      LObj := LField.GetValue(Self).AsObject;
      if not Assigned(LObj) then Continue;
      if not Assigned(TxRTTI.GetFieldAttribute(LField, JX3Unmanaged)) then
      begin
        if (LObj is TJX3Abstract) then
            TJX3Abstract(LObj).Null := True;
      end else begin
        TxRTTI.CallMethodProc('JSONSetNull', LObj, [True]);
      end;
    end;
end;

procedure TJX3Object.SetDefaultValue(AVal: string);
begin

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

class function TJX3Object.LoadFromFile(const AFilename: string; var AStr: string; AEncoding: TEncoding): Int64;
var
  FS : TFileStream;
  SS: TStringStream;
begin
  FS := nil;
  SS := Nil;
  try
    FS := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
    SS := TStringStream.Create('', AEncoding, True);
    Result := SS.CopyFrom(FS, -1);
    AStr := SS.DataString;
  finally
    SS.Free;
    FS.Free;
  end;
end;

class function TJX3Object.SaveToFile(const Filename: string; const AStr: string; AEncoding: TEncoding): Int64;
var
  FS: TFileStream;
  SS: TStringStream;
begin
  FS := nil;
  SS := Nil;
  try
    FS := TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
    SS := TStringStream.Create(AStr, AEncoding);
    Result := FS.CopyFrom(SS, -1);
  finally
    SS.Free;
    FS.Free;
  end;
end;


initialization
  GJSX3PooledJSON := TJX3MiniPool.GetInstance<TJSONObject>(100);
finalization
  GJSX3PooledJSON.Free;
end.

