unit uJX3Rtti;

interface
uses
    System.Generics.Collections
  , SyncObjs
  , RTTI
  ;

{$DEFINE JX3RTTICACHE}

function  JX3GetFields(aObj: TObject): TArray<TRTTIField>;
function  JX3GetProps(aObj: TObject): TArray<TRTTIProperty>;
function  JX3GetMethods(aObj: TObject): TArray<TRTTIMethod>;
function  JX3GetMethod(aObj: TObject; const AName: string): TRTTIMethod; overload;
function  JX3GetMethod(AInstance: TRttiInstanceType; const AName: string): TRTTIMethod overload;
function  JX3GetFieldAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute;
function  JX3GetFieldInstance(Field: TRTTIField) : TRttiInstanceType;

{$IFDEF JX3RTTICACHE}
var
  _RTTIFieldsCacheDic: TDictionary<TClass, TArray<TRttiField>>;
  _RTTIPropsCacheDic: TDictionary<TClass, TArray<TRTTIProperty>>;
  _RTTIMethsCacheDic: TDictionary<TClass, TArray<TRTTIMethod>>;
  _RTTIInsMethsCacheDic: TDictionary<TRttiInstanceType, TRTTIMethod>;
  _RTTIObjMethsCacheDic: TDictionary<string, TRttiMEthod>;
  _RTTIAttrsCacheDic: TDictionary<NativeInt, TCustomAttribute>;
  _RTTIInstCacheDic: TDictionary<TRTTIField, TRttiInstanceType>;
  _RTTIctx: TRttiContext;

{$ELSE}
var
  _RTTIctx: TRttiContext;
{$ENDIF}

implementation

function JX3GetFields(aObj: TObject): TArray<TRTTIField>;
{$IFDEF JX3RTTICACHE}
var
  CType: TClass;
begin
  CType := aObj.ClassType;
  MonitorEnter(_RTTIFieldsCacheDic);
  if not _RTTIFieldsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetFields;
    _RTTIFieldsCacheDic.Add(CType, Result);
  end;
  MonitorExit(_RTTIFieldsCacheDic);
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetFields;
end;
{$ENDIF}

function JX3GetProps(aObj: TObject): TArray<TRTTIProperty>;
{$IFDEF JX3RTTICACHE}
var
  CType: TClass;
begin
  CType := aObj.ClassType;
  MonitorEnter(_RTTIPropsCacheDic);
  if not _RTTIPropsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetProperties;
    _RTTIPropsCacheDic.Add(CType, Result);
  end;
  MonitorExit(_RTTIPropsCacheDic);
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetProperties;
end;
{$ENDIF}

function JX3GetMethods(aObj: TObject): TArray<TRTTIMethod>;
{$IFDEF JX3RTTICACHE}
var
  CType: TClass;
begin
  CType := aObj.ClassType;
  MonitorEnter(_RTTIMethsCacheDic);
  if not _RTTIMethsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetMethods;
    _RTTIMethsCacheDic.Add(CType, Result);
  end;
  MonitorExit(_RTTIMethsCacheDic);
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetMethods;
end;
{$ENDIF}

function JX3GetMethod(AObj: TObject; const AName: string): TRTTIMethod;
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetMethod(AName);
end;

function JX3GetMethod(AInstance: TRttiInstanceType; const AName: string): TRTTIMethod;
{$IFDEF JX3RTTICACHE}
begin
  MonitorEnter(_RTTIInsMethsCacheDic);
  if not _RTTIInsMethsCacheDic.TryGetValue(AInstance, Result) then
  begin
    Result := AInstance.GetMethod(AName);
    _RTTIInsMethsCacheDic.Add(AInstance, Result);
  end;
  MonitorExit(_RTTIInsMethsCacheDic);
end;
{$ELSE}
begin
  Result :=  AInstance.GetMethod(AName);
end;
{$IFEND}

function JX3GetFieldAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute;

  function GetRTTIFieldAttribute(RTTIField: TRTTIField; AttrClass: TClass): TCustomAttribute; inline;
  begin
  {$IF CompilerVersion >= 35.0} // Alexandria 11.0
   Result := RTTIField.GetAttribute(TCustomAttributeClass(AttrClass));
  {$ELSE}
    Result := Nil;
    for var Attr in RTTIField.GetAttributes do
      if Attr.ClassType = AttrClass then
      begin
          Result := Attr;
          Break;
      end;
  {$IFEND}
  end;

begin
{$IFDEF JX3RTTICACHE}
  MonitorEnter(_RTTIAttrsCacheDic);
  if not _RTTIAttrsCacheDic.TryGetValue(NativeInt(Field) xor NativeInt(@AttrClass), Result) then
  begin
    Result := GetRTTIFieldAttribute(Field, AttrClass);
    _RTTIAttrsCacheDic.Add(NativeInt(Field) xor NativeInt(@AttrClass), Result);
  end;
  MonitorExit(_RTTIAttrsCacheDic);
{$ELSE}
    Result := GetRTTIFieldAttribute(Field, AttrClass);
{$ENDIF}
end;

function JX3GetFieldInstance(Field: TRTTIField) : TRttiInstanceType;
begin
{$IFDEF JX3RTTICACHE}
  MonitorEnter(_RTTIInstCacheDic);
  if not _RTTIInstCacheDic.TryGetValue(Field, Result) then
  begin
    Result := Field.FieldType.AsInstance;
    _RTTIInstCacheDic.Add(Field, Result);
  end;
  MonitorExit(_RTTIInstCacheDic);
{$ELSE}
  Result := Field.FieldType.AsInstance;
{$ENDIF}
end;

initialization
{$IFDEF JX3RTTICACHE}
  _RTTIFieldsCacheDic := TDictionary<TClass, TArray<TRttiField>>.Create;
  _RTTIPropsCacheDic := TDictionary<TClass, TArray<TRttiProperty>>.Create;
  _RTTIMethsCacheDic := TDictionary<TClass, TArray<TRttiMEthod>>.Create;
  _RTTIObjMethsCacheDic := TDictionary<string, TRttiMEthod>.Create;
  _RTTIAttrsCacheDic := TDictionary<NativeInt, TCustomAttribute>.Create;
  _RTTIInstCacheDic := TDictionary<TRTTIField, TRttiInstanceType>.Create;
  _RTTIInsMethsCacheDic := TDictionary<TRttiInstanceType, TRTTIMethod>.Create;

{$ENDIF}
finalization
{$IFDEF JX3RTTICACHE}
  _RTTIInsMethsCacheDic.Free;
  _RTTIInstCacheDic.Free;
  _RTTIAttrsCacheDic.Free;
  _RTTIMethsCacheDic.Free;
  _RTTIPropsCacheDic.Free;
  _RTTIFieldsCacheDic.Free;
  _RTTIObjMethsCacheDic.Free;
{$ENDIF}
end.
