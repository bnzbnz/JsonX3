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
unit uJX3String;

interface
uses
    RTTI
  , JSON
  , uJX3Object
  ;

type

  TJX3String = class(TJX3Primitive)
  private
    FValue: string;
    FIsNull:  Boolean;
  protected
    function        GetIsNull: Boolean; override;
    procedure       SetIsNull(ANull: Boolean); override;
    function        GetValue: string;
    procedure       SetValue(AValue: string);
    function        GetIso8601: TDateTime;
    procedure       SetIso8601(AValue: TDateTime);
    function        GetIso8601UTC: TDateTime;
    procedure       SetIso8601UTC(AValue: TDateTime);
  public
    constructor     Create;

    procedure       JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONClone(ADest: TJX3String; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONMerge(AMergedWith: TJX3String; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil);

    class function  C(AValue: string): TJX3String; overload;
    class function  C: TJX3String; overload;

    property N:         Boolean read GetIsNull write SetIsNull;
    property Value:     string read GetValue write Setvalue;
    property Val:       string read GetValue write Setvalue;
    property V:         string read GetValue write Setvalue;
    property Iso8601:   TDateTime read GetIso8601 write SetIso8601;
    property Iso8601UTC:TDateTime read GetIso8601 write SetIso8601;
  end;

  TJX3Str = TJX3String;

implementation
uses
    SysUtils
  , StrUtils
  , DateUtils
  , System.Generics.Collections
  , System.Diagnostics
  , uJX3Rtti
  ;

constructor TJX3String.Create;
begin
  SetIsNull(True);
end;

procedure TJX3String.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LName: string;
  LValue: string;
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
      if Assigned(LAttr) then LValue := JX3Default(LAttr).Value;
    end;
    if not Assigned(LAttr) then
    begin
      if Assigned(AInfoBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
        raise Exception.Create(Format('"%s" (TJX3String) : a value is required', [LName]));
      if joNullToEmpty in AInfoBlock.Options then Exit;
      AInfoBlock.IsEmpty := False;
      if LName.IsEmpty then
        AInfoBlock.Part := 'null'
      else
        AInfoBlock.Part := '"' + LName + '":null';
      Exit;
    end;
  end;

  TJX3Object.VarEscapeJSONStr(LValue);
  AInfoBlock.IsEmpty := False;
  if Assigned(AInfoBlock) and AInfoBlock.FieldName.IsEmpty then
    AInfoBlock.Part := '"' + LValue + '"'
  else
    AInfoBlock.Part := '"' + LName + '":"' + LValue  +'"';
end;

procedure TJX3String.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LJPair:         TJSONPair;
  LDefaultAttr:   JX3Default;
begin
  LJPair := AInfoBlock.Obj.Pairs[0];
  if (Assigned(LJPair)) and (not LJPair.null) and (not (LJPair.JsonValue is TJSONNull)) then
  begin
    SetValue(LJPair.JsonValue.Value);
    Exit;
  end else begin
    LDefaultAttr := JX3Default(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JX3Default));
    if Assigned(LDefaultAttr) then
      SetValue(LDefaultAttr.Value)
    else
      SetIsNull(True);
  end;
end;

procedure TJX3String.JSONClone(ADest: TJX3String; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
begin
  if FIsNull then
  begin
    ADest.IsNull := True;
    Exit;
  end;
  ADest.SetValue(Self.FValue);
end;

procedure TJX3String.JSONMerge(AMergedWith: TJX3String; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
begin
  if (not AMergedWith.GetIsNull) and  (Self.GetIsNull) then
    Self.SetValue(AMergedWith.GetValue);
end;

class function TJX3String.C: TJX3String;
begin
  Result := TJX3String.Create;
end;

class function TJX3String.C(AValue: string): TJX3String;
begin
  Result := TJX3String.C;
  REsult.SetValue(AValue);
end;

procedure TJX3String.SetValue(AValue: string);
begin
  FIsNull := False;
  FValue := AValue;
end;

procedure TJX3String.SetIsNull(ANull: Boolean);
begin
  if not FIsNull and ANull then FValue := '';
  FIsNull := ANull;
end;

function TJX3String.GetIsNull: Boolean;
begin
  Result := FIsNull;
end;

function TJX3String.GetValue: string;
begin
  Result := FValue;
end;

function  TJX3String.GetIso8601: TDateTime;
begin
  Result := ISO8601ToDate(FValue, False);
end;

procedure TJX3String.SetIso8601(AValue: TDateTime);
begin
  SetValue(DateToISO8601(AValue, False));
end;

function  TJX3String.GetIso8601UTC: TDateTime;
begin
  Result := ISO8601ToDate(FValue, True);
end;

procedure TJX3String.SetIso8601UTC(AValue: TDateTime);
begin
  SetValue(DateToISO8601(AValue, True));
end;

end.
