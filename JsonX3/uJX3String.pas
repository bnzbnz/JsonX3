unit uJX3String;

interface
uses
    RTTI
  , JSON
  , uJX3Object
  ;

type

  TJX3String = class(TJX3Object)
  private
    FValue: string;
    FIsNull:  Boolean;
  protected
    function        GetIsNull: Boolean;
    procedure       SetIsNull(ANull: Boolean);
    function        GetValue: string;
    procedure       SetValue(AValue: string);
    function        GetIso8601: TDateTime;
    procedure       SetIso8601(AValue: TDateTime);
    function        GetIso8601UTC: TDateTime;
    procedure       SetIso8601UTC(AValue: TDateTime);
  public
    constructor     Create; override;

    function        JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    procedure       JSONClone(ADest: TJX3String; AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil);
    function        JSONMerge(ASrc: TJX3String; AMergeOpts: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): TValue;

    class function  C(AValue: string): TJX3String; overload;
    class function  C: TJX3String; overload;

    property IsNull:    Boolean read GetIsNull write SetIsNull;
    property Null:      Boolean read GetIsNull write SetIsNull;
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
  inherited;
  SetIsNull(True);
end;

function TJX3String.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LName: string;
  LValue: string;
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
      if Assigned(LAttr) then LValue := JX3Default(LAttr).Value;
    end;
    if not Assigned(LAttr) then
    begin
      if Assigned(AInfoBlock.Field) and Assigned(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
        raise Exception.Create(Format('"%s" (TJX3String) : a value is required', [LName]));
      if joNullToEmpty in AInfoBlock.Options then Exit(TValue.Empty);
      if LName.IsEmpty then Exit('null');
      Exit('"' + LName + '":null');
    end;
  end;

  if Assigned(AInfoBlock) and AInfoBlock.FieldName.IsEmpty then Exit( '"' + TJX3Object.EscapeJSONStr(LValue) + '"');
  Result := '"' + LName + '":"' + TJX3Object.EscapeJSONStr(LValue)  +'"';
end;

procedure TJX3String.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LJPair:         TJSONPair;
  LDefaultAttr:   JX3Default;
begin
  if Assigned(AInOutBlock) and (joStats in AInfoBlock.Options) then Inc(AInOutBlock.Stats.PrimitiveCount);
  LJPair := AInfoBlock.Obj.Pairs[0];
  if (Assigned(LJPair)) and (not LJPair.null) and (not (LJPair.JsonValue is TJSONNull)) then
  begin
    SetValue(LJPair.JsonValue.Value);
    Exit;
  end else begin
    LDefaultAttr := JX3Default(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Default));
    if Assigned(LDefaultAttr) then
      SetValue(LDefaultAttr.Value)
    else
      SetIsNull(True);
  end;
end;

procedure TJX3String.JSONClone(ADest: TJX3String; AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock);
begin
  if Assigned(AInOutBlock) and (joStats in AOptions) then Inc(AInOutBlock.Stats.PrimitiveCount);
  if FIsNull then
  begin
    ADest.IsNull := True;
    Exit;
  end;
  ADest.SetValue(Self.FValue);
end;

function TJX3String.JSONMerge(ASrc: TJX3String; AMergeOpts: TJX3Options; AInOutBlock: TJX3InOutBlock): TValue;
begin
  Result := False;

  if ASrc.GetIsNull then
  begin
    Self.SetIsNull(True);
    Result := True;
    Exit;
  end;

  if Self.GetIsNull then
    Self.SetValue(ASrc.GetValue);
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
  FIsNull := ANull;
  if FIsNull then FValue := '';
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
