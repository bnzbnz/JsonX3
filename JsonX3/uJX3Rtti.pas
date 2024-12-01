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
  _RTTIFieldLock: TCriticalSection;
  _RTTIPropLock: TCriticalSection;
  _RTTIPMethLock: TCriticalSection;
  _RTTIPMethsLock: TCriticalSection;
  _RTTIPInstLock:  TCriticalSection;
  _RTTIFieldsCacheDic: TDictionary<TClass, TArray<TRttiField>>;
  _RTTIPropsCacheDic: TDictionary<TClass, TArray<TRTTIProperty>>;
  _RTTIMethsCacheDic: TDictionary<TClass, TArray<TRTTIMethod>>;
  _RTTIInsMethsCacheDic: TDictionary<TRttiInstanceType, TRTTIMethod>;
  _RTTIInstCacheDic: TDictionary<TRTTIField, TRttiInstanceType>;
  _RTTIctx: TRttiContext;

{$ELSE}
var
  _RTTIctx: TRttiContext;
{$ENDIF}

implementation
uses
  StrUtils,
  Sysutils;

function JX3GetFields(aObj: TObject): TArray<TRTTIField>;
{$IFDEF JX3RTTICACHE}

var
  CType: TClass;
begin
  _RTTIFieldLock.Enter;
  CType := aObj.ClassType;
  if not _RTTIFieldsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetFields;
    _RTTIFieldsCacheDic.Add(CType, Result);
  end;
    _RTTIFieldLock.Leave;
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
  _RTTIPropLock.Enter;
  CType := aObj.ClassType;
  if not _RTTIPropsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetProperties;
    _RTTIPropsCacheDic.Add(CType, Result);
  end;
    _RTTIPropLock.Leave;
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
  _RTTIPMethsLock.Enter;
  CType := aObj.ClassType;
  if not _RTTIMethsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetMethods;
    _RTTIMethsCacheDic.Add(CType, Result);
  end;
    _RTTIPMethsLock.Leave;
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetMethods;
end;
{$ENDIF}

function JX3GetMethod(AObj: TObject; const AName: string): TRTTIMethod;
begin
  Result :=  _RTTIctx.GetType(aObj.ClassType).GetMethod(AName);
end;

function JX3GetMethod(AInstance: TRttiInstanceType; const AName: string): TRTTIMethod;
{$IFDEF JX3RTTICACHE}
begin
  _RTTIPMethLock.Enter;
  if not _RTTIInsMethsCacheDic.TryGetValue(AInstance, Result) then
  begin
    Result := AInstance.GetMethod(AName);
    _RTTIInsMethsCacheDic.Add(AInstance, Result);
  end;
    _RTTIPMethLock.Leave;
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
  Result := GetRTTIFieldAttribute(Field, AttrClass);
end;

function JX3GetFieldInstance(Field: TRTTIField) : TRttiInstanceType;
{$IFDEF JX3RTTICACHE}
begin
  _RTTIPInstLock.Enter;
  if not _RTTIInstCacheDic.TryGetValue(Field, Result) then
  begin
    Result := Field.FieldType.AsInstance;
    _RTTIInstCacheDic.Add(Field, Result);
  end;
  _RTTIPInstLock.Leave;
end;
{$ELSE}
begin
    Result := Field.FieldType.AsInstance;
end;
{$ENDIF}


initialization
{$IFDEF JX3RTTICACHE}
  _RTTIFieldsCacheDic := TDictionary<TClass, TArray<TRttiField>>.Create;
  _RTTIPropsCacheDic := TDictionary<TClass, TArray<TRttiProperty>>.Create;
  _RTTIMethsCacheDic := TDictionary<TClass, TArray<TRttiMEthod>>.Create;
  _RTTIInstCacheDic := TDictionary<TRTTIField, TRttiInstanceType>.Create;
  _RTTIInsMethsCacheDic := TDictionary<TRttiInstanceType, TRTTIMethod>.Create;
  _RTTIFieldLock := TCriticalSection.Create;
  _RTTIPropLock := TCriticalSection.Create;
  _RTTIPMethLock := TCriticalSection.Create;
  _RTTIPMethsLock := TCriticalSection.Create;
  _RTTIPInstLock := TCriticalSection.Create;

{$ENDIF}
finalization
{$IFDEF JX3RTTICACHE}
  _RTTIFieldLock.Free;
  _RTTIPropLock.Free;
  _RTTIPMethsLock.Free;
  _RTTIPInstLock.Free;
  _RTTIInsMethsCacheDic.Free;
  _RTTIInstCacheDic.Free;
  _RTTIMethsCacheDic.Free;
  _RTTIPropsCacheDic.Free;
  _RTTIFieldsCacheDic.Free;
  _RTTIPMethLock.Free;
{$ENDIF}
end.
