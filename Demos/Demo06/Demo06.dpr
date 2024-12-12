program Demo06;

{mode delphi}
uses
  FMX.Forms,
  uDemo06 in 'uDemo06.pas' {Form4},
  uJX3String in '..\..\JsonX3\uJX3String.pas',
  uJX3Boolean in '..\..\JsonX3\uJX3Boolean.pas',
  uJX3List in '..\..\JsonX3\uJX3List.pas',
  uJX3Object in '..\..\JsonX3\uJX3Object.pas',
  uJX3Number in '..\..\JsonX3\uJX3Number.pas',
  uJX3Dictionary in '..\..\JsonX3\uJX3Dictionary.pas',
  uJX3Rtti in '..\..\JsonX3\uJX3Rtti.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
