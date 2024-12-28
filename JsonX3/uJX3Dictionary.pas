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
unit uJX3Dictionary;

interface
uses
  System.Generics.Collections
  , JSON
  , RTTI
  , uJX3Object
  ;

type

  TObjectDictionary<V> = class(System.Generics.Collections.TObjectDictionary<string,V>);
  TJX3Dic<V:class, constructor> = class(TObjectDictionary<V>)
  private

    FMergeAdded: TList<string>;
    FMergeModified: TList<string>;
    FMergeDeleted: TList<string>;

  protected

    function  GetIsNull: Boolean;
    procedure SetIsNull(ANull: Boolean);

  public
    constructor Create;
    destructor Destroy;

    procedure JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    procedure JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    procedure JSONClone(ADest: TJX3Dic<V>; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil);
    procedure JSONMerge(AMergedWith: TJX3Dic<V>; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock = Nil);

    class function New: TJX3Dic<V>;
    class function NewAdd(AKey: string; AValue: V): TJX3Dic<V>;
    class function NewAddRange(const AKeys: array of string; const AValues: array of V): TJX3Dic<V>;

    property IsNull:  Boolean write SetIsNull;
    property Null:    Boolean read  GetIsNull;

  end;
  TJX3Dictionary<V:class, constructor> = class(TJX3Dic<V>);
  TJX3Dict<V:class, constructor> = class(TJX3Dic<V>);

implementation
uses
  Classes
  , Sysutils
  , StrUTils
  , System.Diagnostics
  , uJX3String
  , uJX3Number
  , uJX3List
  , uJX3Boolean
  , uJX3Rtti
  ;

class function TJX3Dic<V>.NewAdd(AKey: string; AValue: V): TJX3Dic<V>;
begin
  Result := TJX3Dic<V>.Create;
  Result.Add(AKey, AValue);
end;

class function TJX3Dic<V>.NewAddRange(const AKeys: array of string; const AValues: array of V): TJX3Dic<V>;
var
  RCnt: Integer;
begin
  Result := TJX3Dic<V>.Create;
  for RCnt := 0  to Length(AKeys) -1 do
    Result.Add(AKeys[RCnt], AValues[RCnt]);
end;

constructor TJX3Dic<V>.Create;
begin
  inherited Create([doOwnsValues]);
  FMergeAdded := TList<string>.Create;
  FMergeModified := TList<string>.Create;
  FMergeDeleted := TList<string>.Create;
end;

destructor TJX3Dic<V>.Destroy;
begin
  FMergeAdded.Free;
  FMergeModified.Free;
  FMergeDeleted.Free;
end;

function TJX3Dic<V>.GetIsNull: Boolean;
begin
  Result := Count = 0;
end;

procedure TJX3Dic<V>.SetIsNull(ANull: Boolean);
begin
  Clear;
end;

class function TJX3Dic<V>.New: TJX3Dic<V>;
begin
  Result := TJX3Dic<V>.Create;
end;

procedure TJX3Dic<V>.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LParts:     TList<string>;
  LRes:       string;
  Lkp:        TPair<string,V>;
  LObj:       TObject;
  LInfoBlock: TJX3InfoBlock;
  LName:      string;
  LNameAttr:  JX3Name;
begin
  if Assigned(AInfoBlock.Field) then
  begin
    LName := AInfoBlock.Field.Name;
    LNameAttr := JX3Name(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JX3Name));
    if Assigned(LNameAttr) then LName := LNameAttr.Name;
  end else
    LName := AInfoBlock.FieldName;
  LName := TJX3Object.NameDecode(LName);

  if GetIsNull then
  begin
    if Assigned(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
      raise Exception.Create(Format('"%s" (TJX3Dic) : a value is required', [LName]));

    if joNullToEmpty in AInfoBlock.Options then Exit;
    AInfoBlock.IsEmpty := False;
    if AInfoBlock.FieldName.IsEmpty then
      AInfoBlock.Part := 'null'
    else
      AInfoBlock.Part := '"' + LName + '":null';
    Exit;
  end;

  LParts := TList<string>.Create;
  LParts.Capacity := Self.Count;
  LInfoBlock := TJX3InfoBlock.Create;
  for Lkp in Self do
  begin
    LObj := TValue.From<V>(Lkp.Value).AsObject;
    if Assigned(LObj) then
    begin
      LInfoBlock.Init(LKp.Key, Nil, Nil, AInfoBlock.Options);
      TxRTTI.CallMethodFunc('JSONSerialize', LObj, [TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock)]);
      if not LInfoBlock.IsEmpty then LParts.Add(LInfoBlock.Part);
    end;
  end;
  LInfoBlock.Free;
  LRes := TJX3Object.JsonListToJsonString(LParts);
  LParts.Free;

  AInfoBlock.IsEmpty := False;
  if AInfoBlock.FieldNAme.IsEmpty then
    AInfoBlock.Part := '{' + LRes + '}'
  else
    AInfoBlock.Part := '"' + LName + '":{'+ LRes + '}';
end;

procedure TJX3Dic<V>.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LPair: TJSONPair;
  LNewObj: TObject;
  LInfoBlock: TJX3InfoBlock;
  LJObj: TJSONObject;
  LJObjDestroy: Boolean;
begin
  if not Assigned(AInfoBlock.Obj) then begin SetIsNull(True); Exit end;;
  if AInfoBlock.Obj.Count = 0 then begin SetIsNull(True); Exit end;
  if not Assigned(AInfoBlock.Obj.Pairs[0].JsonValue) then begin SetIsNull(True); Exit end;
  if AInfoBlock.Obj.Pairs[0].JsonValue.Null then begin SetIsNull(True); Exit end;;

  LInfoBlock := TJX3InfoBlock.Create;
  for LPair in AInfoBlock.Obj do
  begin
    LNewObj := V.Create;
    Add(LPair.JsonString.value, LNewObj);

    LPair.JsonValue.Owned := False;
    LPair.Owned := False;
    LJObjDestroy := True;
    if LPair.JsonValue is TJSONObject then
    begin
       LJObjDestroy := False;
       LJObj := LPair.JsonValue as TJSONObject;
    end else
    if LPair.JsonValue is TJSONArray then
    begin
      LJObj := TJSONObject.Create(TJSONPAir.Create('', LPair.JsonValue));
    end else
      LJObj := TJSONObject.Create(LPair);

    LInfoBlock.Init(AInfoBlock.FieldName, LJObj, AInfoBlock.Field, AInfoBlock.Options);
    TxRTTI.CallMethodProc( 'JSONDeserialize', LNewObj, [ TValue.From<TJX3InfoBlock>(LInfoBlock), TValue.From<TJX3InOutBlock>(AInOutBlock) ]);

    if LJObjDestroy then FreeAndNil(LJObj);
    LPair.Owned := True;
    LPair.JsonValue.Owned := True;
  end;
  LInfoBlock.Free;
end;

procedure TJX3Dic<V>.JSONClone(ADest: TJX3Dic<V>; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  LNewObj: TObject;
  LPair:  TPair<string, V>;
begin
  if GetIsNull then
  begin
    ADest.SetIsNull(True);
    Exit;
  end;
  for LPair in Self do
  begin
    LNewObj := V.Create;
    TxRTTI.CallMethodProc('JSONClone', LPair.Value, [LNewObj, TValue.From<TJX3Options>(AOptions), AInOutBlock]);
    ADest.Add(LPair.Key, LNewObj);
  end;
end;

procedure TJX3Dic<V>.JSONMerge(AMergedWith: TJX3Dic<V>; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
var
  ADic: TPair<string, V>;
  LObj: V;
begin
  FMergeAdded.Clear;
  FMergeModified.Clear;
  FMergeDeleted.Clear;

  if jmoOverride in AOptions then
  begin

    for ADic in AMergedWith do
    begin
      if Self.ContainsKey(ADic.Key) then
      begin
        FMergeModified.Add(ADic.Key);
        TxRTTI.CallMethodProc('JSONMerge', Self.Items[ADic.Key], [ADic.Value, TValue.From<TJX3Options>(AOptions), AInOutBlock]);
      end else begin
        FMergeAdded.Add(ADic.Key);
        TxRTTI.CallMethodProc('JSONClone', ADic.Value, [LObj, TValue.From<TJX3Options>(AOptions), AInOutBlock]);
        Self.Add(ADic.Key, TObject(LObj));
      end;
    end;

    for ADic in Self do
    begin
      if not AMergedWith.ContainsKey(ADic.Key) then
      begin
        FMergeDeleted.Add(ADic.Key);
       TxRTTI.CallMethodProc('JSONSetNull', ADic.Value, [True, TValue.From<TJX3Options>(AOptions), AInOutBlock]);
      end;
    end;

  end else
    if (not AMergedWith.GetIsNull) and  (Self.GetIsNull) then
    begin
      for ADic in AMergedWith do
      begin
        if not Self.ContainsKey(ADic.Key) then
        begin
          LObj := V.Create;
          TxRTTI.CallMethodProc('JSONCreate', LObj, [True]);
          TxRTTI.CallMethodProc('JSONMerge', LObj, [ ADic.Value, TValue.From<TJX3Options>(AOptions), AInOutBlock]);
          Self.Add(ADic.Key, LObj)
        end;
      end
    end;
end;

end.
