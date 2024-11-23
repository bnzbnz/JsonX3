unit uDemo01;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , System.Generics.Defaults
  , System.Generics.Collections
  , uJX3Number
  , uJX3Boolean
  , uJX3String
  , uJX3List
  , uJX3Dictionary
  , uJX3Object
  , uJX3Tools
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
    Str: TJX3Str;
    Bool: TJX3Bool;
    b: TJX3Num; // as Int
    c: TJX3Num; // as UInt
    d: TJX3Num; // as Int64
    e: TJX3Num; // as UInt64
    Null: TJX3Str;
    // ...
  end;

var
  Form4: TForm4;

implementation
uses System.Diagnostics;

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Primitives, NewPrimitives: TPrimitives;
  Json: string;
begin
  Memo1.Lines.Clear;

  Primitives := TPrimitives.Create;
  Primitives.Str.Value := 'testing 😜';  // or Primitives.Str.V :=
  Primitives.Bool.V := True;             // <--^
  Primitives.b.Int := -999;
  Primitives.c.UInt64 := 999;
  Primitives.d.Double := 2.2;
  Primitives.e.Currency := 22.22;

  // Raw Json
  Json := Primitives.ToJson;
  Memo1.lines.add('Raw Original Object:');
  Memo1.lines.add(Json);

  //Optimized Json
  Memo1.lines.add('');
  Json := Primitives.ToJson([joNullToEmpty]);
  Memo1.lines.add('Optimized Original Object:');
  Memo1.lines.add(Json);

  // Converting back to a Primitives Object;
  NewPrimitives := TJX3Object.FromJSON<TPrimitives>(Json);

  // Serializing the New Object
  Memo1.lines.add('');
  Json := NewPrimitives.ToJson([joNullToEmpty]);
  Memo1.lines.add('New Optimized Object:');
  Memo1.lines.add(Json);

  // Checking Values
  Memo1.lines.add('');
  Memo1.lines.add('Checking the New Object Values:');
  Memo1.lines.add('Str: ' + NewPrimitives.Str.V);
  Memo1.lines.add('UInt64: ' + NewPrimitives.c.UInt64.ToString);
  Memo1.lines.add('Currency: ' + NewPrimitives.e.Currency.ToString);

  // Formatted Json
  Memo1.lines.add('');
  Memo1.lines.add('Formatted:');
  Memo1.lines.add(TJX3Tools.FormatJSON(Json));

  NewPrimitives.Free;
  Primitives.Free;

end;

end.
