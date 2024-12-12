unit uDemo02;

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
    Str: TJX3Str;
    Bool: TJX3Bool;
    b: TJX3Num; // as Int
    c: TJX3Num; // as UInt
    d: TJX3Num; // as Int64
    e: TJX3Num; // as UInt64
    Null: TJX3Str;
    // ...
  end;
  TSubClassDemo = class(TJX3Object)
    X: TJX3Num;
    PClass: TPrimitives
  end;
  TInnerObjectDemo = class(TJX3Object)
    S: TJX3Str;
    SubClass: TSubClassDemo; // a class
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Demo, NewDemo, CloneDemo: TInnerObjectDemo;
  Json: string;
begin
  Memo1.Lines.Clear;

  // PLease note that JSX3 owns all objects !
  // It handles construction and destruction of them for you...
  // You may add any number of inner/classes.

  Demo := TInnerObjectDemo.Create;
  Demo.S.V := '~~😃~~'; // UTF8 Support
  Demo.SubClass.X.Double := 222;
  Demo.SubClass.PClass.Bool.V := True;
  Demo.SubClass.PClass.b.Int := 1234;
  Demo.SubClass.PClass.d.Double := 2.22;

  Demo.SubClass.PClass.Str.V:= 'ABC';

  // Raw Json
  Json := TJX3Object.ToJson(Demo, [joRaiseException]);
  Memo1.lines.add('Raw Original Object:');
  Memo1.lines.add(Json);

  // Optimized Json

  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add('Optimized Original Object:');
  Memo1.lines.add(Json);

  // Converting back to a Primitives Object;
  NewDemo := TJX3Object.FromJSON<TInnerObjectDemo>(Json, [joRaiseException]);

  // Serializing the New Object
  Memo1.lines.add('');
  Json := TJX3Object.ToJson(NewDemo, []);
  Memo1.lines.add('New Optimized Object:');
  Memo1.lines.add(Json);

  // Formatted Json
  Memo1.lines.add('');
  Memo1.lines.add('Formatted:');
  Memo1.lines.add(TJX3Object.FormatJSON(Json));

  // You may also cloned any JSX3 Objects.
  CloneDemo := TJX3Object.Clone<TInnerObjectDemo>(Demo);

  CloneDemo.Free;
  NewDemo.Free;
  Demo.Free;

end;

end.
