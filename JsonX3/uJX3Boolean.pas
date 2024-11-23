unit uJX3Boolean;

interface
uses RTTI, JSON, uJX3Tools;

type

  TJX3Boolean = class(TObject)
  private
    FNull: Boolean;
    FValue: Boolean;
  public
    constructor Create;
    procedure JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
    function JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
    function GetNull: Boolean;
    procedure SetNull(ANull: Boolean);
    function GetValue: Boolean;
    procedure SetValue(Value: Boolean);
    class function C: TJX3Boolean;
    property IsNull: Boolean read GetNull write SetNull;
    property N: Boolean read GetNull write SetNull;
    property Value: Boolean read GetValue write Setvalue;
    property V: Boolean read GetValue write Setvalue;
  end;
  TJX3Bool = class(TJX3Boolean);

implementation
uses SysUtils, System.Generics.Collections;

procedure TJX3Boolean.JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
var
  LJPair: TJSONPair;
begin
  LJPair := AJObj.Pairs[0];
  if not Assigned(LJPair) then
  begin
    SetNull(True);
    Exit;
  end else
  if LJPair.JSOnValue.null then
  begin
    SetNull(True);
  end else
    SetValue(LJPair.JsonValue.toString.ToBoolean());
end;

function TJX3Boolean.JSONSerialize(AFieldName: string = ''; AField: TRttiField = nil; AOptions: TJX3Options = []): TValue;
const
  BStrL: array[Boolean] of string = ('false','true');
begin
  if FNull then
  begin
    if joNullToEmpty in AOptions then Exit(TValue.Empty);
    if AFieldName.IsEmpty then Exit('null');
    Exit(Format('"%s":null', [AFieldName]))
  end;
  if AFieldName.IsEmpty then Exit(BStrL[FValue]);
  Result := Format('"%s":%s', [AFieldName, BStrL[FValue]])
end;

constructor TJX3Boolean.Create;
begin
  inherited;
  FNull := True;
  FValue := False;
end;

class function TJX3Boolean.C: TJX3Boolean;
begin
  Result := TJX3Boolean.Create;
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
