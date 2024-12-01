unit uJX3Tools;

interface
uses
    RTTI
  , Classes
  , TypInfo
  , JSON
  ;

const
  cCommaDelimiter = #0;

type

  TJS3Option  = (
        joNullToEmpty
      , joRaiseException
      , joRaiseOnMissingField
      , joStats
  );
  TJX3Options = set of TJS3Option;

  JX3Name = class(TCustomAttribute)
  public
    Name: string;
    constructor Create(const AName: string);
  end;

  JX3Default = class(TCustomAttribute)
  public
    Value: string;
    constructor Create(const AValue: string);
  end;

  JS3Required  = class(TCustomAttribute);

  TJX3Tools = record
    class procedure CallMethodProc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue); static;
    class function  CallMethodFunc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue): TValue; static;
    class procedure BreakPoint(AMsg: string = ''; ABreak: Boolean= True); static;
    class procedure RaiseException(const AMsg: string); static;
    class function  FormatJSON(const json: string; Indentation: Integer = 4): string; static;
    class function  EscapeJSONStr(const AStr: string): string; static;
    class function  NameDecode(const ToDecode: string): string; static;
    class function  NameEncode(const ToEncode: string): string; static;
  end;

  TJX3InfoBlock = class
    Obj: TJSONObject;
    FieldName: string;
    Field: TRttiField;
    Options: TJX3Options;
    constructor Create( AFieldName: string; AObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
  end;

  TJX3InOutStats = class
    ProcessingTimeMS: Int64;
    PrimitivesCount: Int64;
    ListsCount: Int64;
    DicsCount: Int64;
    BooleanCount: Int64;
    NumCount: Int64;
    procedure Clear;
  end;

  TJX3InOutBlock = class
    Stats: TJX3InOutStats;
    User1: TValue;
    User2: TValue;
    constructor Create;
    destructor Destroy; override;
  end;

implementation
uses
    SysUtils
  , System.Character
  , REST.Json
  , uJX3Rtti
  {$IF defined(DEBUG) and defined(MSWINDOWS)}
  , Windows
  {$ENDIF}
  ;

{$IF defined(DEBUG) and defined(MSWINDOWS)}
procedure X64BRK ; assembler;
asm
  int 3; // Press F7 to access the error message
end;
{$ENDIF}

class procedure TJX3Tools.BreakPoint(AMsg: string = ''; ABreak: Boolean= True);
begin
  {$IF defined(DEBUG) and defined(MSWINDOWS)}
    {$IF defined(CPUX86)}
      OutputDebugString(PChar(AMsg));
      if ABreak then
        asm int 3; end;
      AMsg := AMsg; // << Error Message Here
    {$ENDIF}
    {$IF defined(CPUX64)}
      OutputDebugString(PChar(AMsg));
      if ABreak then
      X64BRK;
      AMsg := AMsg; // << Error Message Here
    {$ENDIF}
  {$ELSE}
     raise Exception.Create(AMsg);
  {$ENDIF}
end;

class procedure TJX3Tools.RaiseException(const AMsg: string);
begin
  {$IF defined(DEBUG) and defined(MSWINDOWS)}
    BreakPoint(AMsg, False);
 {$ELSE}
    Raise Exception.Create(AMsg) at ReturnAddress;
 {$ENDIF}
end;

class procedure TJX3Tools.CallMethodProc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue);
var
  LMeth: TRttiMethod;
begin
  LMeth := JX3GetMethod(AObj, AMethod);
  if Assigned(LMeth) then LMeth.Invoke(AObj, AArgs);
end;

class function TJX3Tools.CallMethodFunc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue): TValue;
var
  LMeth: TRttiMethod;
begin
  LMeth := JX3GetMethod(AObj, AMethod);
  if not Assigned(LMeth) then Exit(TValue.Empty);
  Result := LMeth.Invoke(AObj, AArgs);
  if not Result.IsEmpty then Result := Result.AsType<TValue>(True);
end;


class function TJX3Tools.FormatJSON(const json: string; Indentation: Integer): string;
var
  TmpJson: TJsonObject;
begin
  TmpJson := TJSONObject.ParseJSONValue(json) as TJSONObject;
  Result := TJSONAncestor(TmpJson).Format(Indentation);
  FreeAndNil(TmpJson);
end;

class function TJX3Tools.EscapeJSONStr(const AStr: string): string;
var
  LP: PChar;
  LEndP: PChar;
begin
  Result := '';
  LP := PChar(Pointer(AStr));
  LEndP := LP + Length(AStr);
  while LP < LendP do
  begin
    case LP^ of
      #0 .. #31, '\', '"':
        Result := Result + '\' + LP^;
      else
        Result := Result + LP^;
    end;
    Inc(LP);
  end;
end;

class function TJX3Tools.NameDecode(const ToDecode: string): string;
var
  Index: Integer;
  CharCode: Integer;
begin;
  if Pos('_', ToDecode) <> 1 then Exit(ToDecode);
  Result := ''; Index := 2;
  while (Index <= Length(ToDecode)) do
    begin
      if (ToDecode[Index] = '_') and TryStrToInt('$' + Copy(ToDecode, Index + 1, 2), CharCode) then
      begin
        Result := Result + Chr(CharCode);
        Inc(Index, 3);
      end
        else
      begin
        Result := Result + ToDecode[Index];
        Inc(Index, 1);
      end;
    end;
end;

class function TJX3Tools.NameEncode(const ToEncode: string): string;
var
  Encoded: Boolean;
begin
  Result := ''; Encoded := False;
  for var i := 1 to Length(ToEncode) do
    if ToEncode[i].IsLetterOrDigit then
      Result := Result + ToEncode[i]
    else begin
      Encoded := True;
      Result := Result + '_' + Format('%2x', [Ord(ToEncode[i])]);
    end;
  if Encoded then Result := '_' + Result;
end;

constructor JX3Name.Create(const AName: string);
begin
  Name := AName;
end;

constructor JX3Default.Create(const AValue: string);
begin
  Value := AValue;
end;

constructor TJX3InfoBlock.Create( AFieldName: string; AObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
begin
  Obj := AObj;
  FieldName := AFieldName;
  Field := AField;
  Options := AOptions;
end;

{ TJX3StatBlock }

procedure TJX3InOutStats.Clear;
begin
  ProcessingTimeMS  := 0;
  PrimitivesCount   := 0;
  ListsCount        := 0;
  DicsCount         := 0;
  BooleanCount      := 0;
  NumCount          := 0;
end;

constructor TJX3InOutBlock.Create;
begin
  inherited;
  Stats := TJX3InOutStats.Create;
  Stats.Clear;
  User1 := TValue.Empty;
  User2 := TValue.Empty;
end;

destructor TJX3InOutBlock.Destroy;
begin
  Stats.Free;
  inherited;
end;

{ JX2Default }


end.
