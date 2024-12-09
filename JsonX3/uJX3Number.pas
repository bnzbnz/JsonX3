unit uJX3Number;

interface
uses
    RTTI
  , JSON
  , uJX3Object
  ;

type

  TJX3NumKind   = (nkNull, nkInt, nkUInt, nkInt64, nkUInt64, nkDouble, nkCurrency);

  TJX3Number    = class(TObject)
  private
    FIsNull:  Boolean;
    FValue: string;
    FKind: TJX3NumKind;
  protected
    function    GetIsNull: Boolean;
    procedure   SetIsNull(ANull: Boolean);
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
  public
    constructor Create;

    function    JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure   JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    procedure   JSONClone(ADest: TJX3Number; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil);
    procedure   JSONMerge(ASrc: TJX3Number; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock = Nil);

    class function C: TJX3Number;
    class function CInt(AValue: Integer):TJX3Number;
    class function CUInt(AValue: Cardinal):TJX3Number;
    class function CInt64(AValue: Int64):TJX3Number;
    class function CUInt64(AValue:UInt64):TJX3Number;
    class function CDouble(AValue: Double):TJX3Number;
    class function CCurrency(AValue: Currency):TJX3Number;
    class function CValue(AValue: string):TJX3Number;

    property IsNull:    Boolean read GetIsNull write SetIsNull;
    property N:         Boolean read GetIsNull write SetIsNull;
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
  inherited;
  FKind := nkNull;
  FIsNull := True;
  FValue := '0';
end;

function TJX3Number.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LName:    string;
  LValue:   string;
  LAttr:    TCustomAttribute;
  LJValue:  TJSONNumber;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.PrimitiveCount);

  if Assigned(AInfoBlock.Field) then
  begin
    LName := AInfoBlock.Field.Name;
    LAttr := JX3Name(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Name));
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
      LAttr := JX3Default(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Default));
      if Assigned(LAttr) then LValue := JX3Default(LAttr).Value;
    end;
    if not Assigned(LAttr) then
    begin

      if Assigned(AInfoBlock.Field) and Assigned(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
        raise Exception.Create(Format('"%s" (TJX3Number) : a value is required', [LName]));

      if joNullToEmpty in AInfoBlock.Options then Exit(TValue.Empty);
      if LName.IsEmpty then Exit('null');
      Exit(Format('"%s":null', [LName]))
    end;
  end;

  LJValue := TJSONNumber.Create(LValue);
  try
    if AInfoBlock.FieldName.IsEmpty then Exit(LJValue.Value);
    if LName.IsEmpty then Exit(Format('%s', [LName, LJValue.Value]));
    Result := Format('"%s":%s', [LName, LJValue.Value]);
  finally
    LJValue.Free;
  end;

end;

procedure TJX3Number.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LJPair: TJSONPair;
  LDefaultAttr:   JX3Default;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.PrimitiveCount);
  LJPair := AInfoBlock.Obj.Pairs[0];
  if (Assigned(LJPair)) and (not LJPair.null) and (not (LJPair.JsonValue is TJSONNull)) then
  begin
    SetValue(LJPair.JsonValue.Value);
    Exit;
  end else begin
  	LDefaultAttr := Nil;
    if  Assigned(AInfoBlock.Field) then LDefaultAttr := JX3Default(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Default));
    if Assigned(LDefaultAttr) then
      SetValue(LDefaultAttr.Value)
    else
      SetIsNull(True);
  end;
end;

procedure TJX3Number.JSONClone(ADest: TJX3Number; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
begin
  if Assigned(AInOutBlock) and (joStats in AOptions) then Inc(AInOutBlock.Stats.PrimitiveCount);
  if Self.FIsNull then
  begin
    ADest.IsNull := True;
    Exit;
  end;
  ADest.SetValue(Self.FValue);
end;

class function TJX3Number.C: TJX3Number;
begin
  Result := TJX3Number.Create;
end;

class function TJX3Number.CInt(AValue:Integer): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetInt(AVAlue);
end;

class function TJX3Number.CInt64(AValue: Int64): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetInt64(AVAlue);
end;

class function TJX3Number.CUInt(AValue: Cardinal): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetUInt(AVAlue);
end;

class function TJX3Number.CUInt64(AValue: UInt64): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetUInt64(AVAlue);
end;

class function TJX3Number.CDouble(AValue: Double): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetDouble(AValue);
end;

class function TJX3Number.CCurrency(AValue: Currency): TJX3Number;
begin
  Result := TJX3Number.Create;
  Result.SetCurrency(AValue);
end;

class function TJX3Number.CValue(AValue: string): TJX3Number;
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

function TJX3Number.GetInt: Integer;
begin
  Result := FVAlue.ToInteger;
end;

procedure TJX3Number.SetInt(AValue: Integer);
begin
  FKind := nkInt;
  SetValue(AValue.ToString);
end;

function TJX3Number.GetInt64: Int64;
begin
  Result := FValue.ToInt64;
end;

procedure TJX3Number.SetInt64(AValue: Int64);
begin
  FKind := nkInt64;
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
   FKind := nkUInt;
   SetValue(AValue.ToString);
end;

function TJX3Number.GetUInt64: UInt64;
begin
  Result := StrToUInt64(FValue);
end;

procedure TJX3Number.SetUInt64(AValue: UInt64);
begin
   FKind := nkUInt64;
   SetValue(AValue.ToString);
end;

function TJX3Number.GetDouble: Double;
begin
  Result := FValue.ToDouble;
end;

procedure TJX3Number.SetDouble(AValue: Double);
begin
   FKind := nkDouble;
   SetValue(AValue.ToString);
end;

function TJX3Number.GetCurrency: Currency;
begin
   FKind := nkCurrency;
   Result := StrToCurr(FValue);
end;

procedure TJX3Number.SetCurrency(AValue: Currency);
begin
   SetValue(AValue.ToString);
end;

procedure TJX3Number.JSONMerge(ASrc: TJX3Number; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock);
begin
  if ASrc.GetIsNull then
  begin
    Self.SetIsNull(True);
    Exit;
  end;
  if Self.GetIsNull then
    Self.SetValue(ASrc.GetValue);
end;

end.
