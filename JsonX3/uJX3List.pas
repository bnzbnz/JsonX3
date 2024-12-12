unit uJX3List;

interface
uses
  System.Generics.Collections
  , RTTI
  , uJX3Object
  , uJX3String
  , uJX3Boolean
  , uJX3Number
  ;

type

  TObjectList<T: class, constructor> = class(System.Generics.Collections.TObjectList<T>);
  TJX3List<T: class, constructor> = class(TObjectList<T>)
  protected
    function        GetIsNull: Boolean;
    procedure       SetIsNull(ANull: Boolean);
  public
    constructor     Create;
    destructor      Destroy; override;
    function        First:T;
    function        Last: T;

    function        JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock =Nil);
    procedure       JSONClone(ADest: TJX3List<T>; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONMerge(ASrc: TJX3List<T>; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock = Nil);

    class function  C: TJX3List<T>;
    class function  CAdd(AValue: T): TJX3List<T>;
    class function  CAddRange(const AValues: array of T): TJX3List<T>; overload;

    property        IsNull: Boolean read GetIsNull write SetIsNull;
    property        N: Boolean read GetIsNull write SetIsNull;
  end;

  TJX3Lst<V:class, constructor> = class(TJX3List<V>);

  // Aliases...

  TJX3StrList   = TJX3List<TJX3String>;
  TJX3StrLst    = TJX3List<TJX3String>;
  TJX3NumList   = TJX3List<TJX3Number>;
  TJX3NumLst    = TJX3List<TJX3Number>;
  TJX3BoolList  = TJX3List<TJX3Boolean>;
  TJX3BoolLst   = TJX3List<TJX3Boolean>;
  TJX3ListStr   = TJX3List<TJX3String>;
  TJX3LstStr    = TJX3List<TJX3String>;
  TJX3ListNum   = TJX3List<TJX3Number>;
  TJX3LstNum    = TJX3List<TJX3Number>;
  TJX3ListBool  = TJX3List<TJX3Boolean>;
  TJX3LstBool   = TJX3List<TJX3Boolean>;

implementation
uses
  Classes
  , JSON
  , SysUtils
  , StrUtils
  , System.Diagnostics
  , uJX3Rtti
  , TypInfo
  ;

function TJX3List<T>.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LParts:     TStringList;
  LPart:      TValue;
  LRes:       string;
  LEle:       T;
  LInfoBlock: TJX3InfoBlock;
  LName:      string;
  LNameAttr:  JX3Name;
   LJObj:     TJSONObject;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.ListCount);

  LName := AInfoBlock.FieldName;
  if Assigned(AInfoBlock) and Assigned(AInfoBlock.Field) then
  begin
    LNameAttr := JX3Name(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Name));
    if Assigned(LNameAttr) then LName := LNameAttr.Name;
  end;
  LName := TJX3Object.NameDecode(LName);

  if GetIsNull then
  begin

    if Assigned(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
      raise Exception.Create(Format('"%s" (TJX3List) : a value is required', [LName]));

    if joNullToEmpty in AInfoBlock.Options then Exit(TValue.Empty);
    if AInfoBlock.FieldName.IsEmpty then EXit('null');
    Exit(Format('"%s":null', [LName]));
  end;

  LParts := TStringList.Create(#0, cCommaDelimiter, [soStrictDelimiter]);
  LParts.Capacity := Self.Count;
  for LEle in Self do
  begin
    LJObj := Nil;
    LInfoBlock := TJX3InfoBlock.Create('', LJObj, Nil, AInfoBlock.Options);
    LPart := JX3CallMethodFunc('JSONSerialize', LEle, [ LInfoBlock, AInOutBlock ]);
    if not LPart.IsEmpty then LParts.Add(LPart.AsString);
    LInfoBlock.Free;
  end;
  LRes := LParts.DelimitedText.Replace(cCommaDelimiter, ',');

  if LName.IsEmpty then
    Result := '[' + LRes + ']'
  else
    Result := '"' + LName + '":[' + LRes + ']';
  LParts.Free;
end;

procedure TJX3List<T>.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LEle:       TJSONValue;
  LNewObj:    TObject;
  LInfoBlock: TJX3InfoBlock;
  LJObj:      TJSONObject;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.ListCount);
  if not Assigned(AInfoBlock.Obj) then begin SetIsNull(True); Exit end;;
  if AInfoBlock.Obj.Count = 0 then begin SetIsNull(True); Exit end;;
  if not Assigned(AInfoBlock.Obj.Pairs[0].JsonValue) then begin SetIsNull(True); Exit end;
  if AInfoBlock.Obj.Pairs[0].JsonValue.Null then begin SetIsNull(True); Exit end;;
  if TJSONArray(AInfoBlock.Obj.Pairs[0].JsonValue) is TJSONArray then
    for LEle in TJSONArray(AInfoBlock.Obj.Pairs[0].JsonValue) do
    begin
      if LELe is TJSONObject then
      begin
         LNewObj := T.Create;
         Add(LNewObj);
         LJObj :=  LEle as TJSONObject;
         LInfoBlock := TJX3InfoBlock.Create(AInfoBlock.FieldName, LJObj, AInfoBlock.Field, AInfoBlock.Options);
         JX3CallMethodProc( 'JSONDeserialize', LNewObj, [ TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock) ] );
         LInfoBlock.Free;
      end else begin
         LNewObj := T.Create;
         Add(LNewObj);
         LEle.Owned := False;
         LJObj :=  TJSONObject.Create(TJSONPair.Create('', LEle));
         LInfoBlock := TJX3InfoBlock.Create(AInfoBlock.FieldName, LJObj, AInfoBlock.Field, AInfoBlock.Options);
         JX3CallMethodProc( 'JSONDeserialize', LNewObj, [ TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock) ] );
         LJObj.Free;;
         LInfoBlock.Free;
         LEle.Owned := True;
      end;
    end;
end;

procedure TJX3List<T>.JSONClone(ADest: TJX3List<T>; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  LNewObj: TObject;
  LList: TObject;
begin
  if (joStats in AOptions) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.ListCount);
  if GetIsNull then
  begin
    ADest.SetIsNull(True);
    Exit;
  end;
  for LList in Self do
  begin
    LNewObj := JX3CreateObject(T);
    JX3CallMethodProc('JSONCreate', LNewObj, [True]);
    JX3CallMethodProc('JSONClone', LList, [LNewObj, TValue.From<TJX3Options>(AOptions), AInOutBlock]);
    ADest.Add(LNewObj);
  end;
end;

constructor TJX3List<T>.Create;
  var
  LFields:    TArray<TRttiField>;
  LField:     TRTTIField;
  LInstance:  TRttiInstanceType;
  LMethod:    TRTTIMEthod;
  LNewObj:    TObject;
begin
  inherited Create;
  Self.OwnsObjects := True;
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

destructor TJX3List<T>.Destroy;
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

class function TJX3List<T>.CAdd(AValue: T): TJX3List<T>;
begin
  Result := TJX3List<T>.Create;
  Result.Add(AValue);
end;

class function TJX3List<T>.CAddRange(const AValues: array of T): TJX3List<T>;
begin
  Result := TJX3List<T>.Create;
  Result.AddRange(AValues);
end;

function TJX3List<T>.GetIsNull: Boolean;
begin
  Result := Count = 0;
end;

procedure TJX3List<T>.SetIsNull(ANull: Boolean);
begin
  Clear;
end;

class function TJX3List<T>.C: TJX3List<T>;
begin
  Result := TJX3List<T>.Create;
end;

function TJX3List<T>.First: T;
begin
  Result := Self[0];
end;

function TJX3List<T>.Last: T;
begin
  Result := Self[Count - 1];
end;

procedure TJX3List<T>.JSONMerge(ASrc: TJX3List<T>; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  AList: T;
  LObj: T;
begin
  if ASrc.GetIsNull then
  begin
    Self.SetIsNull(True);
    Exit;
  end;
  for AList in ASrc do
  begin
    LObj := T.Create;
    JX3CallMethodProc('JSONCreate', LObj, [True]);
    JX3CallMethodProc('JSONMerge', LOBJ, [ AList, TValue.From<TJX3Options>(AMergeOpts), AInOutBlock]);
    Self.Add(LObj);
  end;
end;

end.
