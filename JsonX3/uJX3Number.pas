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
unit uJX3Number;

interface
uses
    RTTI
  , JSON
  , uJX3Object
  ;

type

  TJX3Number    = class(TJX3Abstract)
  private
    FIsNull:  Boolean;
    FValue: string;
  protected
    function    GetIsNull: Boolean; override;
    procedure   SetIsNull(ANull: Boolean); override;
    function    GetInt: Integer;
    procedure   SetInt(AValue: Integer);
    function    GetInt64: Int64;
    procedure   SetInt64(AValue: Int64);
    function    GetUInt: Cardinal;
    procedure   SetUInt(AValue: Cardinal);
    function    GetUInt64: UInt64;
    procedure   SetUInt64(AValue: UInt64);
    function    GetDouble: Double;
    procedure   SetDouble(AValue: Double);
    function    GetCurrency: Currency;
    procedure   SetCurrency(AValue: Currency);
    function    GetValue: string;
    procedure   SetValue(AValue: string);
    procedure   SetDefaultValue(AVal: string); override;
  public
    constructor Create;

    procedure   JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil); override;
    procedure   JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);  override;
    procedure   JSONClone(ADest: TObject; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil); override;
    procedure   JSONMerge(AMergedWith: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock = Nil); override;

    class function  New: TJX3Number; overload;
    class function  New(AValue: string): TJX3Number; overload;
    class function  NewInt(AValue: Integer):TJX3Number;
    class function  NewUInt(AValue: Cardinal):TJX3Number;
    class function  NewInt64(AValue: Int64):TJX3Number;
    class function  NewUInt64(AValue:UInt64):TJX3Number;
    class function  NewDouble(AValue: Double):TJX3Number;
    class function  NewCurrency(AValue: Currency):TJX3Number;
    class function  NewValue(AValue: string):TJX3Number;

    property Int:       Integer read GetInt write SetInt;
    property UInt:      Cardinal read GetUInt write SetUInt;
    property Int64:     Int64 read GetInt64 write SetInt64;
    property UInt64:    UInt64 read GetUInt64 write SetUInt64;
    property Double:    Double read GetDouble write SetDouble;
    property Currency:  Currency read GetCurrency write SetCurrency;
    property Value:     string read GetValue write Setvalue;
    property Val:       string read GetValue write Setvalue;
    property V:         string read GetValue write Setvalue;
  end;

  TJX3Num = TJX3Number;

implementation
uses
    SysUTils
  , StrUtils
  , System.Generics.Collections
  , System.Diagnostics
  , uJX3Rtti
  ;

constructor TJX3Number.Create;
begin
  FIsNull := True;
  FValue := '0';
end;

procedure TJX3Number.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LName:    string;
  LValue:   string;
  LAttr:    TCustomAttribute;
  LJValue:  TJSONNumber;
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
        raise Exception.Create(Format('"%s" (TJX3Number) : a value is required', [LName]));

      if joNullToEmpty in AInfoBlock.Options then Exit;
      AInfoBlock.IsEmpty := False;
      if LName.IsEmpty then
        AInfoBlock.Part := 'null'
      else
        AInfoBlock.Part := '"' + LName + '":null';
      Exit;
    end;
  end;

  LJValue := TJSONNumber.Create(LValue);
  try
    AInfoBlock.IsEmpty := False;
    if AInfoBlock.FieldName.IsEmpty then
      AInfoBlock.Part := LJValue.ToString
    else if LName.IsEmpty then
      AInfoBlock.Part := LJValue.Value
    else
      AInfoBlock.Part := '"' + LName + '":' + LJValue.Value;
  finally
    LJValue.Free;
  end;

end;

class function TJX3Number.New: TJX3Number;
begin
  Result := TJX3Number.Create;
end;

class function TJX3Number.New(AValue: string): TJX3Number;
begin
  Result := New;
  Result.SetValue(AValue);
end;

procedure TJX3Number.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LJPair: TJSONPair;
  LDefaultAttr:   JX3Default;
begin

  LJPair := AInfoBlock.Obj.Pairs[0];
  if (Assigned(LJPair)) and (not LJPair.null) and (not (LJPair.JsonValue is TJSONNull)) then
  begin
    SetValue(LJPair.JsonValue.Value);
    Exit;
  end else begin
  	LDefaultAttr := Nil;
    if  Assigned(AInfoBlock.Field) then LDefaultAttr := JX3Default(TxRTTI.GetFieldAttribute(AInfoBlock.Field, JX3Default));
    if Assigned(LDefaultAttr) then
      SetValue(LDefaultAttr.Value)
    else
      SetIsNull(True);
  end;
end;

procedure TJX3Number.JSONClone(ADest: TObject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
begin
  if Self.FIsNull then
  begin
    TJX3Number(ADest).IsNull := True;
    Exit;
  end;
  TJX3Number(ADest).SetValue(Self.FValue);
end;

class function TJX3Number.NewInt(AValue:Integer): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetInt(AVAlue);
end;

class function TJX3Number.NewInt64(AValue: Int64): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetInt64(AVAlue);
end;

class function TJX3Number.NewUInt(AValue: Cardinal): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetUInt(AVAlue);
end;

class function TJX3Number.NewUInt64(AValue: UInt64): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetUInt64(AVAlue);
end;

class function TJX3Number.NewDouble(AValue: Double): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetDouble(AValue);
end;

class function TJX3Number.NewCurrency(AValue: Currency): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetCurrency(AValue);
end;

class function TJX3Number.NewValue(AValue: string): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetValue(AValue);
end;

function TJX3Number.GetISNull: Boolean;
begin
  Result := FIsNull;
end;

procedure TJX3Number.SetIsNull(ANull: Boolean);
begin
  FIsNull := ANull;
  if ANull then FValue := '0';
end;

procedure TJX3Number.SetValue(AValue: string);
begin
  FIsNull := False;
  FValue := AValue;
end;

procedure TJX3Number.SetDefaultValue;
begin
  SetValue(AVal);
end;

function TJX3Number.GetInt: Integer;
begin
  Result := FVAlue.ToInteger;
end;

procedure TJX3Number.SetInt(AValue: Integer);
begin
  SetValue(AValue.ToString);
end;

function TJX3Number.GetInt64: Int64;
begin
  Result := FValue.ToInt64;
end;

procedure TJX3Number.SetInt64(AValue: Int64);
begin
  SetValue(AValue.ToString);
end;

function TJX3Number.GetValue: string;
begin
  Result := FValue;
end;

function TJX3Number.GetUInt: Cardinal;
begin
  Result := StrToUInt(FValue);
end;

procedure TJX3Number.SetUInt(AValue: Cardinal);
begin
  SetValue(AValue.ToString);
end;

function TJX3Number.GetUInt64: UInt64;
begin
  Result := StrToUInt64(FValue);
end;

procedure TJX3Number.SetUInt64(AValue: UInt64);
begin
  SetValue(AValue.ToString);
end;

function TJX3Number.GetDouble: Double;
begin
  Result := FValue.ToDouble;
end;

procedure TJX3Number.SetDouble(AValue: Double);
begin
  SetValue(AValue.ToString);
end;

function TJX3Number.GetCurrency: Currency;
begin
  Result := StrToCurr(FValue);
end;

procedure TJX3Number.SetCurrency(AValue: Currency);
begin
  SetValue(CurrToStr(AValue));
end;

procedure TJX3Number.JSONMerge(AMergedWith: TOBject; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
begin
  if jmoOverride in AOptions then
  begin
    if not(TJX3Number(AMergedWith).GetIsNull) then
      Self.Value := TJX3Number(AMergedWith).Value;
  end else
    // null only
    if (not TJX3Number(AMergedWith).GetIsNull) and  (Self.GetIsNull) then
      Self.SetValue(TJX3Number(AMergedWith).GetValue);
end;

end.
