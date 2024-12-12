unit uJX3Boolean;

interface
uses

  RTTI
  , uJX3Object
  ;

const
  cBoolToStr: array[Boolean] of string = ('false','true');

type

  TJX3Boolean = class(TObject)
  private
    FIsNull:  Boolean;
    FValue: Boolean;
  protected
    function        GetIsNull: Boolean;
    procedure       SetIsNull(ANull: Boolean);
    function        GetValue: Boolean;
    procedure       SetValue(AValue: Boolean);
  public
    constructor     Create;

    function        JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONClone(ADest: TJX3Boolean; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONMerge(ASrc: TJX3Boolean; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock = Nil);

    class function  C(AValue: Boolean = False): TJX3Boolean;

    property        IsNull: Boolean read GetIsNull write SetIsNull;
    property        N: Boolean read GetIsNull write SetIsNull;
    property        Value: Boolean read GetValue write Setvalue;
    property        Val: Boolean read GetValue write Setvalue;
    property        V: Boolean read GetValue write Setvalue;
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
  inherited;
  FIsNull := True;
  FValue := False;
end;

function TJX3Boolean.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LName: string;
  LValue: Boolean;
  LAttr:  TCustomAttribute;
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
      if Assigned(LAttr) then LValue := JX3Default(LAttr).Value.ToBoolean();
    end;
    if not Assigned(LAttr) then
    begin
      if Assigned(AInfoBlock.Field) and Assigned(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
        raise Exception.Create(Format('"%s" (TJX3Boolean) : a value is required', [LName]));

      if joNullToEmpty in AInfoBlock.Options then Exit(TValue.Empty);
      if LName.IsEmpty then Exit('null');
      Exit(Format('"%s":null', [LName]))
    end;
  end;

  if AInfoBlock.FieldName.IsEmpty then Exit(cBoolToStr[LValue]);
  Result := Format('"%s":%s', [LName, cBoolToStr[LValue]])
end;

procedure TJX3Boolean.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LJPair:  TJSONPair;
  LAttr:   JX3Default;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.PrimitiveCount);
  LJPair := AInfoBlock.Obj.Pairs[0];
  if (Assigned(LJPair)) and (not LJPair.null) and (not (LJPair.JsonValue is TJSONNull)) then
  begin
    SetValue(LJPair.JsonValue.Value.ToBoolean);
    Exit;
  end else begin
  	LAttr := Nil;
    if  Assigned(AInfoBlock.Field) then LAttr := JX3Default(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Default));
    if Assigned(LAttr) then
      SetValue(JX3Default(LAttr).Value.ToBoolean())
    else
      SetIsNull(True);
  end;
end;

procedure TJX3Boolean.JSONClone(ADest: TJX3Boolean; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
begin
  if Assigned(AInOutBlock) and (joStats in AOptions) then Inc(AInOutBlock.Stats.PrimitiveCount);
  if FIsNull then
  begin
    ADest.IsNull := True;
    Exit;
  end;
  ADest.SetValue(Self.FValue);
end;

procedure TJX3Boolean.JSONMerge(ASrc: TJX3Boolean; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock);
begin
  if ASrc.GetIsNull then
  begin
    Self.SetIsNull(True);
    Exit;
  end;
  if Self.GetIsNull then
    Self.SetValue(ASrc.GetValue);
end;

class function TJX3Boolean.C(AValue: Boolean = False): TJX3Boolean;
begin
  Result := TJX3Boolean.Create;
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

function TJX3Boolean.GetValue: Boolean;
begin
  Result := FValue;
end;

end.
