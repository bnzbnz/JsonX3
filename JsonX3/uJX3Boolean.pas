unit uJX3Boolean;

interface
uses
    JSON
  , RTTI
  , uJX3Tools
  ;

const
  cBoolToStr: array[Boolean] of string = ('false','true');

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
    procedure       SetValue(AValue: Boolean);
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
  , uJX3Rtti
  ;

procedure TJX3Boolean.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LJPair: TJSONPair;
  LDefaultAttr:   JX3Default;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.PrimitivesCount);
  LJPair := AInfoBlock.Obj.Pairs[0];
  if (Assigned(LJPair)) and (not LJPair.null) and (not (LJPair.JsonValue is TJSONNull)) then
  begin
    SetValue(LJPair.JsonValue.Value.ToBoolean);
    Exit;
  end else begin
    LDefaultAttr := JX3Default(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Default));
    if Assigned(LDefaultAttr) then
      SetValue(LDefaultAttr.Value.ToBoolean)
    else
      SetNull(True);
  end;
end;

function TJX3Boolean.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LValue: Boolean;
  LDefaultAttr: JX3Default;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.BooleanCount);
  LValue := FValue;
  if FNull then
  begin
    LDefaultAttr := JX3Default(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Default));
    if Assigned(LDefaultAttr) then
      LValue := LDefaultAttr.Value.ToBoolean
    else begin
      if joNullToEmpty in AInfoBlock.Options then Exit(TValue.Empty);
      if AInfoBlock.FieldName.IsEmpty then Exit('null');
      Exit(Format('"%s":null', [AInfoBlock.FieldName]))
    end;
  end;;
  if AInfoBlock.FieldName.IsEmpty then Exit(cBoolToStr[LValue]);
  Result := Format('"%s":%s', [AInfoBlock.FieldName, cBoolToStr[LValue]])
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

procedure TJX3Boolean.SetValue(AValue: Boolean);
begin
  FNull := False;
  FValue := AValue;
end;

function TJX3Boolean.GetValue: Boolean;
begin
  Result := FValue;
end;

end.
