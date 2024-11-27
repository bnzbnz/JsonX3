unit uJX3Boolean;

interface
uses
    JSON
  , RTTI
  , uJX3Tools
  ;

type

  TJX3Boolean = class(TObject)
  private
    FNull:  Boolean;
    FValue: Boolean;
  public
    constructor Create;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    function        JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    function        GetNull: Boolean;
    procedure       SetNull(ANull: Boolean);
    function        GetValue: Boolean;
    procedure       SetValue(Value: Boolean);
    class function  C(AValue: Boolean = False): TJX3Boolean;
    property        IsNull: Boolean read GetNull write SetNull;
    property        N: Boolean read GetNull write SetNull;
    property        Value: Boolean read GetValue write Setvalue;
    property        V: Boolean read GetValue write Setvalue;
  end;
  TJX3Bool = TJX3Boolean;

implementation
uses
    SysUtils
  , System.Generics.Collections
  ;

procedure TJX3Boolean.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LJPair: TJSONPair;
begin
  if Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.BooleanCount);
  LJPair := AInfoBlock.Obj.Pairs[0];
  if (Assigned(LJPair)) and (not LJPair.null) and (not (LJPair.JsonValue is TJSONNull))  then
    SetValue(LJPair.JsonValue.toString.ToBoolean())
  else
    SetNull(True);
end;

function TJX3Boolean.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
const
  BStrL: array[Boolean] of string = ('false','true');
begin
  if Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.BooleanCount);
  if FNull then
  begin
    if joNullToEmpty in AInfoBlock.Options then Exit(TValue.Empty);
    if AInfoBlock.FieldName.IsEmpty then Exit('null');
    Exit(Format('"%s":null', [AInfoBlock.FieldName]))
  end;
  if AInfoBlock.FieldName.IsEmpty then Exit(BStrL[FValue]);
  Result := Format('"%s":%s', [AInfoBlock.FieldName, BStrL[FValue]])
end;

constructor TJX3Boolean.Create;
begin
  inherited;
  FNull := True;
  FValue := False;
end;

class function TJX3Boolean.C(AValue: Boolean = False): TJX3Boolean;
begin
  Result := TJX3Boolean.Create;
  Result.SetValue(AValue);
end;

function TJX3Boolean.GetNull: Boolean;
begin
  Result := FNull;
end;

procedure TJX3Boolean.SetNull(ANull: Boolean);
begin
  FNull := ANull;
  if ANull then FValue := False;
end;

procedure TJX3Boolean.SetValue(Value: Boolean);
begin
  FNull := False;
  FValue := Value;
end;

function TJX3Boolean.GetValue: Boolean;
begin
  Result := FValue;
end;

end.
