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
    FIsNull:  Boolean;
  protected
    function        GetIsNull: Boolean;
    procedure       SetIsNull(ANull: Boolean);
    function        GetValue: string;
    procedure       SetValue(AValue: string);
  public
    constructor     Create;
    function        JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil): TValue;
    procedure       JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock = Nil);

    class function  C(AValue: string): TJX3String; overload;
    class function  C: TJX3String; overload;
    function        Clone(AOptions: TJX3Options = []; AInOutBlock: TJX3InOutBlock = Nil): TJX3String;

    property IsNull:  Boolean read GetIsNull write SetIsNull;
    property Null:    Boolean read GetIsNull write SetIsNull;
    property N:       Boolean read GetIsNull write SetIsNull;
    property Value:   string read GetValue write Setvalue;
    property Val:     string read GetValue write Setvalue;
    property V:       string read GetValue write Setvalue;
  end;

  TJX3Str = TJX3String;

implementation
uses
    SysUtils
  , StrUtils
  , System.Generics.Collections
  , System.Diagnostics
  , uJX3Rtti
  ;

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
  LName := TJX3Tools.NameDecode(LName);

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
      if Assigned(uJX3Rtti.JX3GetFieldAttribute(AInfoBlock.Field, JS3Required)) then
        TJX3Tools.RaiseException(Format('"%s" (TJX3String) : a value is required', [LName]));
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
  if (joStats in AInfoBlock.Options) and Assigned(AInOutBlock) then Inc(AInOutBlock.Stats.PrimitiveCount);
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

constructor TJX3String.Create;
begin
  inherited;
  FValue := '';
  FIsNull := True;
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

function TJX3String.Clone(AOptions: TJX3Options; AInOutBlock: TJX3InOutBlock): TJX3String;
var
  LWatch: TStopWatch;
  LJObj: TJSONObject;
  LInfoBlock: TJX3InfoBlock;
  LJson: string;
begin
  if (joStats in AOptions) and Assigned(AInOutBlock) then LWatch := TStopWatch.StartNew;
  LInfoBlock := Nil;
  LJObj := Nil;
  try
    Result := TJX3String.Create;
    LInfoBlock := TJX3InfoBlock.Create('T', LJObj, Nil, AOptions);
    LJson := JSONSerialize(LInfoBlock, AInOutBlock).AsString;
    LInfoBlock.Free;
    LJObj := TJSONObject.ParseJSONValue('{'  + LJson + '}', True, joRaiseException in AOptions) as TJSONObject;
    LInfoBlock := TJX3InfoBlock.Create( '', LJObj, Nil, AOptions);
    JSONDeserialize(LInfoBlock, AInOutBlock);
    TJX3Tools.CallMethodProc('JSONDeserialize', Result, [LInfoBlock, AInOutBlock]);
  finally
    LJObj.Free;
    LInfoBlock.Free;
  end;
  if (joStats in AOptions) and Assigned(AInOutBlock) then AInOutBlock.Stats.ProcessingTimeMS := LWatch.ElapsedMilliseconds;
end;



end.
