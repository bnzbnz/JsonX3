unit uJX3String;

interface
uses
    RTTI
  , JSON
  , uJX3Tools
  ;

type

  TJX3String = class(TObject)
  private
    FValue: string;
    FNull:  Boolean;
  public
    constructor     Create;
    function        JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);
    function        GetNull: Boolean;
    procedure       SetNull(ANull: Boolean);
    function        GetValue: string;
    procedure       SetValue(AValue: string);

    class function  C(AValue: string): TJX3String; overload;
    class function  C: TJX3String; overload;

    property IsNull: Boolean read GetNull write SetNull;
    property Null: Boolean read GetNull write SetNull;
    property N: Boolean read GetNull write SetNull;
    property Value: string read GetValue write Setvalue;
    property Val: string read GetValue write Setvalue;
    property V: string read GetValue write Setvalue;
  end;
  TJX3Str = TJX3String;

implementation
uses
    SysUtils
  , StrUtils
  , System.Generics.Collections
  , uJX3Rtti
  ;

constructor TJX3String.Create;
begin
  inherited;
  FValue := '';
  FNull := True;
end;

function TJX3String.JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue;
var
  LName: string;
  LValue: string;
  LNameAttr:      JX3Name;
  LDefaultAttr:   JX3Default;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.PrimitivesCount);

  if Assigned(AInfoBlock.Field) then
  begin
    LName := AInfoBlock.Field.Name;
    LNameAttr := JX3Name(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Name));
    if Assigned(LNameAttr) then LName := LNameAttr.Name;
  end else
    LName := AInfoBlock.FieldName;
  LName := TJX3Tools.NameDecode(LName);

  LValue := FValue;
  if GetNull then
  begin
    LDefaultAttr := Nil;
    if Assigned(AInfoBlock.Field) then
    begin
      LDefaultAttr := JX3Default(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JX3Default));
      if Assigned(LDefaultAttr) then LValue := LDefaultAttr.Value;
    end;
    if not Assigned(LDefaultAttr) then
    begin
      if joNullToEmpty in AInfoBlock.Options then Exit(TValue.Empty);
      if LName.IsEmpty then Exit('null');
      Exit(Format('"%s":null', [LName]))
    end;
  end;

  if AInfoBlock.FieldName.IsEmpty then Exit( '"' + TJX3Tools.EscapeJSONStr(LValue) + '"');
  Result := Format('"%s":%s', [LName,  '"' + TJX3Tools.EscapeJSONStr(LValue) + '"']);
end;

procedure TJX3String.JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock);
var
  LJPair:         TJSONPair;
  LDefaultAttr:   JX3Default;
begin
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.PrimitivesCount);
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
      SetNull(True);
  end;
end;

class function TJX3String.C: TJX3String;
begin
  Result := TJX3String.Create;
  TJX3Tools.CallMethodProc( 'JSONInit', Result, []);
end;

class function TJX3String.C(AValue: string): TJX3String;
begin
  Result := TJX3String.C;
  REsult.SetValue(AValue);
end;

procedure TJX3String.SetNull(ANull: Boolean);
begin
  FNull := ANull;
  if FNull then FValue := '';
end;

procedure TJX3String.SetValue(AValue: string);
begin
  FNull := False;
  FValue := AValue;
end;

function TJX3String.GetNull: Boolean;
begin
  Result := FNull;
end;

function TJX3String.GetValue: string;
begin
  Result := FValue;
end;



end.
