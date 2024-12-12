unit uDemo01;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , uJX3Number
  , uJX3Boolean
  , uJX3String
  , uJX3Object
  ;

type

  TForm4 = class(TForm)
    Memo1: TMemo;
    Button: TButton;
    procedure ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TPrimitives = class(TJX3Object)
    Str:     TJX3Str;
    Bool:    TJX3Bool;
    Num1:    TJX3Num; // as Int
    Num2:    TJX3Num; // as UInt
    Num3:    TJX3Num; // as Int64
    Num4:    TJX3Num; // as UInt64
    NullStr: TJX3Str;
    // ...
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Primitives, NewPrimitives: TPrimitives;
  Json: string;
begin
  Memo1.Lines.Clear;

  Primitives := TPrimitives.Create;
  Primitives.Str.Value := 'testing 😜';
  Primitives.Bool.V := True;        // V being a shortcut for "Value"

  Primitives.Num1.Int := -999;
  Primitives.Num2.UInt64 := 999;
  Primitives.Num3.Double := 2.2;
  Primitives.Num4.Currency := 22.22;
  // Primitives.NullStr << Null

  // Raw Json
  Json := TJX3Object.ToJson(Primitives, []);
  Memo1.lines.add('Raw Original Object:');
  Memo1.lines.add(Json);

  //Optimized Json
  Memo1.lines.add('');
  Json := TJX3Object.ToJson(Primitives, [joNullToEmpty]);
  Memo1.lines.add('Optimized Original Object:');
  Memo1.lines.add(Json);

  // Converting back to a Primitives Object;
  NewPrimitives := TJX3Object.FromJSON<TPrimitives>(Json, []);

  // Serializing the New Object
  Memo1.lines.add('');
  Json := TJX3Object.ToJson(NewPrimitives, [joNullToEmpty]);
  Memo1.lines.add('New Cloned Object:');
  Memo1.lines.add(Json);

  // Checking Values
  Memo1.lines.add('');
  Memo1.lines.add('Checking the New Object Values:');
  Memo1.lines.add('Str: ' + NewPrimitives.Str.V);
  Memo1.lines.add('UInt64: ' + NewPrimitives.Num2.UInt64.ToString);
  Memo1.lines.add('Currency as double: ' + NewPrimitives.Num4.Double.ToString);

  // Formatted Json
  Memo1.lines.add('');
  Memo1.lines.add('Formatted:');
  Memo1.lines.add(TJX3Object.FormatJSON(Json));

  NewPrimitives.Free;
  Primitives.Free;

end;

end.
