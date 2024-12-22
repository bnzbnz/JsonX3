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

    procedure       JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
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

procedure TJX3List<T>.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LParts:     TList<string>;
  LRes:       string;
  LEle:       T;
  LInfoBlock: TJX3InfoBlock;
  LName:      string;
  LNameAttr:  JX3Name;
begin
  LName := AInfoBlock.FieldName;
  if Assigned(AInfoBlock) and Assigned(AInfoBlock.Field) then
  begin
    LNameAttr := JX3Name(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JX3Name));
    if Assigned(LNameAttr) then LName := LNameAttr.Name;
  end;
  LName := TJX3Object.NameDecode(LName);

  if GetIsNull then
  begin
    if Assigned(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
      raise Exception.Create(Format('"%s" (TJX3List) : a value is required', [LName]));
    if joNullToEmpty in AInfoBlock.Options then Exit;
    AInfoBlock.IsEmpty := False;
    if LName.IsEmpty then
      AInfoBlock.Part := 'null'
    else
      AInfoBlock.Part := '"' + LName + '":null';
    Exit;
  end;

  LParts := TList<string>.Create;
  LParts.Capacity := Self.Count;
  LInfoBlock := TJX3InfoBlock.Create;
  for LEle in Self do
  begin
    LInfoBlock.Init('', Nil, Nil, AInfoBlock.Options);
    TxRTTI.CallMethodFunc('JSONSerialize', LEle, [ LInfoBlock, AInOutBlock ]);
    if not LInfoBlock.IsEmpty then LParts.Add(LInfoBlock.Part);
  end;
  LInfoBlock.Free;
  LRes := TJX3Object.JsonListToJsonString(LParts);

  AInfoBlock.IsEmpty := False;
  if LName.IsEmpty then
    AInfoBlock.Part := '[' + LRes + ']'
  else
    AInfoBlock.Part := '"' + LName + '":[' + LRes + ']';
  LParts.Free;
end;

procedure TJX3List<T>.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LEle:       TJSONValue;
  LNewObj:    TObject;
  LInfoBlock: TJX3InfoBlock;
  LJObj:      TJSONObject;
begin
  if not Assigned(AInfoBlock.Obj) then begin SetIsNull(True); Exit end;;
  if AInfoBlock.Obj.Count = 0 then begin SetIsNull(True); Exit end;;
  if not Assigned(AInfoBlock.Obj.Pairs[0].JsonValue) then begin SetIsNull(True); Exit end;
  if AInfoBlock.Obj.Pairs[0].JsonValue.Null then begin SetIsNull(True); Exit end;;
  if TJSONArray(AInfoBlock.Obj.Pairs[0].JsonValue) is TJSONArray then
  begin
    LInfoBlock := TJX3InfoBlock.Create;
    for LEle in TJSONArray(AInfoBlock.Obj.Pairs[0].JsonValue) do
    begin
      if LELe is TJSONObject then
      begin
        LNewObj := T.Create;
        TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
        Add(LNewObj);
        LJObj :=  LEle as TJSONObject;
        LInfoBlock.Init(AInfoBlock.FieldName, LJObj, AInfoBlock.Field, AInfoBlock.Options);
        TxRTTI.CallMethodProc( 'JSONDeserialize', LNewObj, [ TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock) ] );
      end else begin
        LNewObj := T.Create;
        TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
        Add(LNewObj);
        LEle.Owned := False;
        LJObj :=  TJSONObject.Create(TJSONPair.Create('', LEle));
        LInfoBlock.Init(AInfoBlock.FieldName, LJObj, AInfoBlock.Field, AInfoBlock.Options);
        TxRTTI.CallMethodProc( 'JSONDeserialize', LNewObj, [ TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock) ] );
        LJObj.Free;;
        LEle.Owned := True;
      end;
    end;
    LInfoBlock.Free;
  end;
end;

procedure TJX3List<T>.JSONClone(ADest: TJX3List<T>; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  LNewObj: TObject;
  LList: TObject;
begin
  if GetIsNull then
  begin
    ADest.SetIsNull(True);
    Exit;
  end;
  for LList in Self do
  begin
    LNewObj := T.Create;
    TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
    TxRTTI.CallMethodProc('JSONClone', LList, [LNewObj, TValue.From<TJX3Options>(AOptions), AInOutBlock]);
    ADest.Add(LNewObj);
  end;
end;

constructor TJX3List<T>.Create;
  var
  LFields:    TArray<TRttiField>;
  LField:     TRTTIField;
  LNewObj:    TObject;
begin
  inherited Create;
  Self.OwnsObjects := True;
  LFields := TxRTTI.GetFields(Self);
  for LField in LFields do
  begin
    if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic]) then
    begin
      if not Assigned(TxRTTI.GetFieldAttribute(LField, JX3Unmanaged)) then
      begin
        LNewObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
        if not Assigned(LNewObj) then Continue;
        TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
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
  LFields := TxRTTI.GetFields(Self);
  for LField in LFields do
    if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic, mvPublished]) then
    begin
      LObj := LField.GetValue(Self).AsObject;
      if not Assigned(LObj) then Continue;
      if Assigned(TxRTTI.GetFieldAttribute(LField, JX3Unmanaged)) then
      begin
        if TxRTTI.CallMethodFunc('JSONDestroy', LObj, []).AsBoolean then
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
    TxRTTI.CallMethodProc('JSONCreate', LObj, [True]);
    TxRTTI.CallMethodProc('JSONMerge', LOBJ, [ AList, TValue.From<TJX3Options>(AMergeOpts), AInOutBlock]);
    Self.Add(LObj);
  end;
end;

end.
