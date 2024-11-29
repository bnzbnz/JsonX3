unit uJX3Number;

interface
uses RTTI, JSON, uJX3Tools;

type

  TJX3Number = class(TObject)
  private
    FNull: Boolean;
    FValue: string;
  public
    constructor Create;
    destructor Destroy; override;
    function JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    function GetNull: Boolean;
    procedure SetNull(ANull: Boolean);
    function GetInt: Integer;
    procedure SetInt(AValue: Integer);
    function GetInt64: Int64;
    procedure SetInt64(AValue: Int64);
    function GetUInt: Cardinal;
    procedure SetUInt(AValue: Cardinal);
    function GetUInt64: UInt64;
    procedure SetUInt64(AValue: UInt64);
    function GetDouble: Double;
    procedure SetDouble(AValue: Double);
    function GetCurrency: Currency;
    procedure SetCurrency(AValue: Currency);
    function GetValue: string;
    procedure SetValue(AValue: string);

    class function C: TJX3Number;
    class function CInt(AValue: Integer):TJX3Number;
    class function CUInt(AValue: Cardinal):TJX3Number;
    class function CInt64(AValue: Int64):TJX3Number;
    class function CUInt64(AValue:UInt64):TJX3Number;
    class function CDouble(AValue: Double):TJX3Number;
    class function CCurrency(AValue: Currency):TJX3Number;
    class function CValue(AValue: string):TJX3Number;

    property IsNull: Boolean read GetNull write SetNull;
    property Int: Integer read GetInt write SetInt;
    property UInt: Cardinal read GetUInt write SetUInt;
    property Int64: Int64 read GetInt64 write SetInt64;
    property UInt64: UInt64 read GetUInt64 write SetUInt64;
    property Double: Double read GetDouble write SetDouble;
    property Currency: Currency read GetCurrency write SetCurrency;
    property Value: string read GetValue write Setvalue;
    property Val: string read GetValue write Setvalue;
    property V: string read GetValue write Setvalue;
  end;
  TJX3Num = TJX3Number;

implementation
uses SysUTils, StrUtils, System.Generics.Collections;

function TJX3Number.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LJValue:  TJSONString;
  Value: string;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.NumCount);
  Value := FValue;
  if FNull then
  begin
    if Assigned(AInfoBlock.AttrDefault) then
      Value := AInfoBlock.AttrDefault.Value
    else begin
      if joNullToEmpty in AInfoBlock.Options then Exit(TValue.Empty);
      if AInfoBlock.FieldName.IsEmpty then Exit('null');
      Exit(Format('"%s":null', [AInfoBlock.FieldName]))
    end;
  end;
  LJValue := TJSONString.Create(Value);
  try
    if AInfoBlock.FieldName.IsEmpty then Exit(LJValue.Value);
    Exit(Format('"%s":%s', [AInfoBlock.FieldName, LJValue.Value]));
  finally
    LJValue.Free;
  end;
end;

procedure TJX3Number.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LJPair: TJSONPair;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.NumCount);
  LJPair := AInfoBlock.Obj.Pairs[0];
  if (Assigned(LJPair) and (not LJPair.null)) and (not (LJPair.JsonValue is TJSONNull)) then
    SetValue(LJPair.JsonValue.Value)
  else begin
    if Assigned(AInfoBlock.AttrDefault)  then
    begin
      SetValue(AInfoBlock.AttrDefault.Value)
    end else
      if Assigned(AInfoBlock.AttrDefault)  then
        SetValue(AInfoBlock.AttrDefault.Value)
      else
        SetNull(True);
  end;
end;

constructor TJX3Number.Create;
begin
  inherited;
  FNull := True;
  FValue := '0';
end;

destructor TJX3Number.Destroy;
begin
  inherited;
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

function TJX3Number.GetNull: Boolean;
begin
  Result := FNull;
end;

procedure TJX3Number.SetNull(ANull: Boolean);
begin
  FNull := ANull;
  if ANull then FValue := '0';
end;

procedure TJX3Number.SetValue(AValue: string);
begin
  FNull := False;
  FValue := AValue;
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
   SetValue(AValue.ToString);
end;

end.
