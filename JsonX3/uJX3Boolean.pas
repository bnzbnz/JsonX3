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
unit uJX3Boolean;

interface
uses

  RTTI
  , uJX3Object
  ;

const
  cBoolToStr: array[Boolean] of string = ('false','true');

type

  TJX3Boolean = class(TJX3Abstract)
  private
    FIsNull:  Boolean;
    FValue: Boolean;
  protected
    function        GetIsNull: Boolean; override;
    procedure       SetIsNull(ANull: Boolean); override;
    function        GetValue: Boolean;
    procedure       SetValue(AValue: Boolean);
    procedure       SetDefaultValue(AVal: string); override;
  public
    constructor     Create;

    procedure       JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil); override;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil); override;
    procedure       JSONClone(ADest: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil); override;
    procedure       JSONMerge(AMergedWith: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock = Nil); override;

    class function  New: TJX3Boolean; overload;
    class function  New(AValue: Boolean): TJX3Boolean; overload;

    property        Value:  Boolean read GetValue write Setvalue;
    property        Val:    Boolean read GetValue write Setvalue;
    property        V:      Boolean read GetValue write Setvalue;
  end;
  TJX3Bool = TJX3Boolean;

implementation
uses
    JSON
  , SysUtils
  , System.Generics.Collections
  , System.Diagnostics
  , uJX3Rtti
  ;

constructor TJX3Boolean.Create;
begin
  FIsNull := True;
  FValue := False;
end;

procedure TJX3Boolean.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LName: string;
  LValue: Boolean;
  LAttr:  TCustomAttribute;
begin

  if Assigned(AInfoBlock.Field) then
  begin
    LName := AInfoBlock.Field.Name;
    LAttr := JX3Name(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JX3Name));
    if Assigned(LAttr) then LName := JX3Name(LAttr).Name;
  end else
    LName := AInfoBlock.FieldName;
  LName := TJX3Object.NameDecode(LName);

  LValue := FValue;
  if GetIsNull then
  begin
    LAttr := Nil;
    if Assigned(AInfoBlock.Field) then
    begin
      LAttr := JX3Default(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JX3Default));
      if Assigned(LAttr) then LValue := JX3Default(LAttr).Value.ToBoolean();
    end;
    if not Assigned(LAttr) then
    begin
      if Assigned(AInfoBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
        raise Exception.Create(Format('"%s" (TJX3Boolean) : a value is required', [LName]));

      if joNullToEmpty in AInfoBlock.Options then Exit;
      AInfoBlock.IsEmpty := False;
      if LName.IsEmpty then
        AInfoBlock.Part := 'null'
      else
        AInfoBlock.Part := '"' + LName + '":null';
      Exit;
    end;
  end;

  AInfoBlock.IsEmpty := False;
  if Assigned(AInfoBlock) and AInfoBlock.FieldName.IsEmpty then
    AInfoBlock.Part := cBoolToStr[LValue]
  else
    AInfoBlock.Part := '"' + LName + '":' + cBoolToStr[LValue];
end;

procedure TJX3Boolean.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LJPair:  TJSONPair;
  LAttr:   JX3Default;
begin
  LJPair := AInfoBlock.Obj.Pairs[0];
  if (Assigned(LJPair)) and (not LJPair.null) and (not (LJPair.JsonValue is TJSONNull)) then
  begin
    SetValue(LJPair.JsonValue.Value.ToBoolean);
    Exit;
  end else begin
  	LAttr := Nil;
    if  Assigned(AInfoBlock.Field) then LAttr := JX3Default(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JX3Default));
    if Assigned(LAttr) then
      SetValue(JX3Default(LAttr).Value.ToBoolean())
    else
      SetIsNull(True);
  end;
end;

procedure TJX3Boolean.JSONClone(ADest: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
begin
  if FIsNull then
  begin
    TJX3Boolean(ADest).SetIsNull(True);
    Exit;
  end;
  TJX3Boolean(ADest).SetValue(Self.FValue);
end;

procedure TJX3Boolean.JSONMerge(AMergedWith: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
begin
  if (not TJX3Boolean(AMergedWith).GetIsNull) and  (Self.GetIsNull) then
    Self.SetValue(TJX3Boolean(AMergedWith).GetValue);
end;

class function TJX3Boolean.New: TJX3Boolean;
begin
  Result := TJX3Boolean.Create;
end;

class function TJX3Boolean.New(AValue: Boolean): TJX3Boolean;
begin
  Result := New;
  Result.SetValue(AValue);
end;

function TJX3Boolean.GetIsNull: Boolean;
begin
  Result := FIsNull;
end;

procedure TJX3Boolean.SetIsNull(ANull: Boolean);
begin
  FIsNull := ANull;
  if ANull then FValue := False;
end;

procedure TJX3Boolean.SetValue(AValue: Boolean);
begin
  FIsNull := False;
  FValue := AValue;
end;

procedure TJX3Boolean.SetDefaultValue(AVal: string);
begin
  SetValue(AVal.ToBoolean);
end;

function TJX3Boolean.GetValue: Boolean;
begin
  Result := FValue;
end;

end.
