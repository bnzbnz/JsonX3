unit uJSONableStringList;

interface
uses
  Classes
  , JSON
  , RTTI
  , uJX3Tools
  , SysUtils
  ;

type

  TJSONableStringList = class(TStringList)
  public
    procedure   JSONInit;
    function    JSONSerialize(AFieldName: string; AField: TRttiField; AOptions: TJX3Options): TValue;
    procedure   JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
    procedure   JSONExit;
  end;

implementation

procedure TJSONableStringList.JSONInit;
begin
end;

procedure TJSONableStringList.JSONExit;
begin

end;

function TJSONableStringList.JSONSerialize(AFieldName: string; AField: TRttiField; AOptions: TJX3Options): TValue;
begin
end;

procedure TJSONableStringList.JSONDeserialize(AJObj: TJSONObject; AField: TRttiField; AOptions: TJX3Options);
begin
end;


end.
