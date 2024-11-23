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
    function        JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
    procedure       JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
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
  ;

constructor TJX3String.Create;
begin
  inherited;
  FValue := '';
  FNull := True;
end;

function TJX3String.JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
begin
  if FNull then
  begin
    if joNullToEmpty in AOptions then Exit(TValue.Empty);
    if AFieldName.IsEmpty then Exit('null');
    Exit(Format('"%s":null', [AFieldName]))
  end;
  if AFieldName.IsEmpty then Exit( '"' + TJX3Tools.EscapeJSONStr(FValue) + '"');
  Exit(Format('"%s":%s', [AFieldName,  '"' + TJX3Tools.EscapeJSONStr(FValue) + '"']));
end;

procedure TJX3String.JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
var
  LJPair: TJSONPair;
begin
  LJPair := AJObj.Pairs[0];
  if not Assigned(LJPair) then
  begin
    SetNull(True);
    Exit;
  end else
  if LJPair.null then
  begin
    SetNull(True);
  end else
  if LJPair.JsonValue is TJSONNull then
  begin
    SetNull(True);
  end else
  begin
    SetValue(LJPair.JsonValue.Value);
  end;
end;

class function TJX3String.C: TJX3String;
begin
  Result := TJX3String.Create;
  TJX3Tools.CallMethod( 'JSONInit', Result, []);
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
