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
  _RTTIctx: TRttiContext;
  _RTTILock1: TCriticalSection;
  _RTTILock2: TCriticalSection;
  _RTTILock3: TCriticalSection;
  _RTTILock4: TCriticalSection;
  _RTTILock5: TCriticalSection;
  _RTTILock6: TCriticalSection;
  _RTTIFieldsCacheDic: TDictionary<TClass, TArray<TRttiField>>;
  _RTTIPropsCacheDic: TDictionary<TClass, TArray<TRTTIProperty>>;
  _RTTIMethsCacheDic: TDictionary<TClass, TArray<TRTTIMethod>>;
  _RTTIMethObjCacheDic: TDictionary<NativeInt, TRTTIMethod>;
  _RTTIInstMethsCacheDic: TDictionary<TRttiInstanceType, TRTTIMethod>;
  _RTTIInstCacheDic: TDictionary<TRTTIField, TRttiInstanceType>;

{$ELSE}
var
  _RTTIctx: TRttiContext;
{$ENDIF}
  function JX3CreateObject(AInstance: TRTTIInstanceType): TObject; overload; inline;
  function JX3CreateObject(AClass: TClass): TObject; overload; inline;
  procedure JX3CallMethodProc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue);
  function  JX3CallMethodFunc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue): TValue;
implementation
uses
    StrUtils
  , Sysutils
  ;

procedure JX3CallMethodProc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue);
var
  LMeth: TRttiMethod;
begin
  LMeth := JX3GetMethod(AObj, AMethod);
  if Assigned(LMeth) then LMeth.Invoke(AObj, AArgs);
end;

function JX3CallMethodFunc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue): TValue;
var
  LMeth: TRttiMethod;
begin
  LMeth := JX3GetMethod(AObj, AMethod);
  if not Assigned(LMeth) then Exit(TValue.Empty);
  Result := LMeth.Invoke(AObj, AArgs);
  if not Result.IsEmpty then Result := Result.AsType<TValue>(True);
end;

function JX3CreateObject(AInstance: TRTTIInstanceType): TObject;
var
  LMethod: TRTTIMethod;
begin
  Result := Nil;
  if not Assigned(AInstance) then Exit;
  LMethod := AInstance.GetMethod('Create');
  if not Assigned(LMethod) then Exit;
  Result := LMethod.Invoke(AInstance.MetaclassType,[]).AsObject
end;

function JX3CreateObject(AClass: TClass): TObject;
begin
   Result := JX3CreateObject(_RTTIctx.GetType(AClass).AsInstance);
end;

function JX3GetFields(aObj: TObject): TArray<TRTTIField>;
{$IFDEF JX3RTTICACHE}

var
  CType: TClass;
begin
  _RTTILock1.Enter;
  CType := aObj.ClassType;
  if not _RTTIFieldsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetFields;
    _RTTIFieldsCacheDic.Add(CType, Result);
  end;
    _RTTILock1.Leave;
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
  _RTTILock2.Enter;
  CType := aObj.ClassType;
  if not _RTTIPropsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetProperties;
    _RTTIPropsCacheDic.Add(CType, Result);
  end;
    _RTTILock2.Leave;
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
  _RTTILock3.Enter;
  CType := aObj.ClassType;
  if not _RTTIMethsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetMethods;
    _RTTIMethsCacheDic.Add(CType, Result);
  end;
    _RTTILock3.Leave;
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetMethods;
end;
{$ENDIF}

function JX3GetMethod(AObj: TObject; const AName: string): TRTTIMethod;
{$IFDEF JX3RTTICACHE}
var
  Lx: NativeInt;
begin
  _RTTILock4.Enter;
  Lx := NativeInt(AObj.ClassType) + AName.GetHashCode;
  if  not _RTTIMethObjCacheDic.TryGetValue(Lx, Result)  then
  begin
    Result :=  _RTTIctx.GetType(AObj.ClassType).GetMethod(AName);
    _RTTIMethObjCacheDic.Add(Lx, Result);
  end;
  _RTTILock4.Leave;
end;
{$ELSE}
begin
  Result :=  _RTTIctx.GetType(AObj.ClassType).GetMethod(AName);
end;
{$IFEND}


function JX3GetMethod(AInstance: TRttiInstanceType; const AName: string): TRTTIMethod;
{$IFDEF JX3RTTICACHE}
begin
  _RTTILock5.Enter;
  if not _RTTIInstMethsCacheDic.TryGetValue(AInstance, Result) then
  begin
    Result := AInstance.GetMethod(AName);
    _RTTIInstMethsCacheDic.Add(AInstance, Result);
  end;
  _RTTILock5.Leave;
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
  _RTTILock6.Enter;
  if not _RTTIInstCacheDic.TryGetValue(Field, Result) then
  begin
    Result := Field.FieldType.AsInstance;
    _RTTIInstCacheDic.Add(Field, Result);
  end;
  _RTTILock6.Leave;
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
  _RTTIInstMethsCacheDic := TDictionary<TRttiInstanceType, TRTTIMethod>.Create;
  _RTTIMethObjCacheDic := TDictionary<NativeInt, TRTTIMethod>.Create;
  _RTTILock1 := TCriticalSection.Create;
  _RTTILock2 := TCriticalSection.Create;
  _RTTILock3 := TCriticalSection.Create;
  _RTTILock4 := TCriticalSection.Create;
  _RTTILock5 := TCriticalSection.Create;
  _RTTILock6 := TCriticalSection.Create;
{$ENDIF}
finalization
{$IFDEF JX3RTTICACHE}
  _RTTILock6.Free;
  _RTTILock5.Free;
  _RTTILock4.Free;
  _RTTILock3.Free;
  _RTTILock2.Free;
  _RTTILock1.Free;
  _RTTIFieldsCacheDic.Free;
  _RTTIPropsCacheDic.Free;
  _RTTIMethsCacheDic.Free;
  _RTTIMethObjCacheDic.Free;
  _RTTIInstMethsCacheDic.Free;
  _RTTIInstCacheDic.Free;
{$ENDIF}
end.
