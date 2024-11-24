unit uDemo03;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , uJX3Number
  , uJX3Boolean
  , uJX3String
  , uJX3Object
  , uJX3List
  , uJX3Dictionary
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

  TObjectDemo = class(TJX3Object)
    S: TJX3Str;
    X: TJX3List<TJX3Str>;                         // an array(List) of strings : TArray<string>
    Y: TJX3Dic<TJX3Num>;                          // An dictionary of Numbers (<string, number>)  *JSON allows only strings as key
    Z: TJX3List<TPrimitives>;                     // A list of TPrimitives
    Q: TJX3List<TJX3List<TJX3Str>>;               // A list of string Lists
    W: TJX3List<TJX3Dic<TJX3List<TPrimitives>>>;  // ouch ! A List of dictionaries of TPrimitives Objects Lists !!!
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Demo, NewDemo, CloneDemo: TObjectDemo;
  Json: string;
  s: TJX3List<TJX3Str>;
  p: TJX3List<TPrimitives>;
begin
  Memo1.Lines.Clear;

  Demo := TObjectDemo.Create;
  Demo.S.V := '~~😃~~'; // UTF8 Support

  // TJX3List<TJX3Str> : Array<string>
  Demo.X.Add(TJX3Str.C('@@@@'));  // "C" to Create with a Value
  Demo.X.Add(TJX3Str.C('EEZZ'));
  Demo.X.Add(TJX3Str.C('OOOO'));

  // TJX3Dic<TJX3Num> :  Array<number>
  Demo.Y.Add('Value1', TJX3Num.CInt(10111));
  Demo.Y.Add('Value2', TJX3Num.CInt64(12222));

  // TJX3List<TPrimitives>  : Array<TPrimitives)
  Demo.Z.Add(TPrimitives.Create);
  Demo.Z.Last.Bool.V := True;
  Demo.Z.Last.e.Int64 := 111;
  Demo.Z.Add(TPrimitives.Create);
  Demo.Z.Last.Bool.V := False;
  Demo.Z.Last.e.Int64 := 333;

  // TJX3List<TJX3List<TJX3Str>> : Array<Array<string>>>
  S := TJX3List<TJX3Str>.Create;
  S.Add(TJX3Str.C('TTT'));  // C for create
  S.Add(TJX3Str.C('OOO'));  // C for create
  Demo.Q.Add(S);

    // 1 Liner :
    // Demo.Q.Add(TJX3List<TJX3Str>.CAddRange([TJX3Str.C('TTT'), TJX3Str.C('OOO')]));

  // TJX3List<TJX3Dic<TJX3List<TPrimitives>>>  : Array<Dictionary<string, Array<TPrimitives>>> :)
  var p1 := TJX3List<TPrimitives>.CAdd(TPrimitives.Create);              // Create a 2 elements Primitives array
  p1.Last.Str.v := 'Boolean1';                                           // Acdess the Last item (which is the first also in this case)
  p1.Last.Bool.v := True;
  var p2 := TJX3List<TPrimitives>.CAdd(TPrimitives.Create);              // Create a 2 elements Primitives array
  p2.Last.Str.v := 'Boolean2';
  p2.Last.Bool.v := True;

  var d1 := TJX3Dic<TJX3List<TPrimitives>>.Create;                        // Create the dictionary ownning the 2 lists
  d1.Add('DicVal1', p1);
  d1.Add('DicVal2', p2);

  Demo.w.Add(d1);                                                         // Adding the Dict and its clone to the main list
  Demo.w.Add(d1.Clone);

  // An other way (remove curly brackets)
  {
      Demo.w.Free; Demo.w := TJX3List<TJX3Dic<TJX3List<TPrimitives>>>.Create;

  p := TJX3List<TPrimitives>.CAddRange([TPrimitives.Create, TPrimitives.Create]);
  p.First.Str.V := 'string 1';
  p.First.b.Int := 111;
  p.Last.Str.V := 'string 2';
  p.Last.b.Int64 := 222;

  Demo.w.AddRange([TJX3Dic<TJX3List<TPrimitives>>.Create, TJX3Dic<TJX3List<TPrimitives>>.Create]);
  Demo.w.First.Add('Dic1', p);
  Demo.w.Last.Add('Dic2', p.Clone());
  }

  // Raw Json
  Json := Demo.ToJson;
  Memo1.lines.add('Raw Original Object:');
  Memo1.lines.add(Json);

  // Optimized Json
  Memo1.lines.add('');
  Json := Demo.ToJson([joNullToEmpty]);
  Memo1.lines.add('Optimized Original Object:');
  Memo1.lines.add(Json);

  // Converting back to a Primitives Object;
  NewDemo := TJX3Object.FromJSON<TObjectDemo>(Json);

  // Serializing the New Object
  Memo1.lines.add('');
  Json := NewDemo.ToJson([joNullToEmpty]);
  Memo1.lines.add('New Optimized Object:');
  Memo1.lines.add(Json);

  // You may also cloned any JSX3 Objects.
  CloneDemo := Demo.Clone<TObjectDemo>;

  // Formatted Json
  Memo1.lines.add('');
  Memo1.lines.add('Formatted:');
  Memo1.lines.add(TJX3Tools.FormatJSON(CloneDemo.ToJSON(), 8));

  CloneDemo.Free;
  NewDemo.Free;
  Demo.Free;

end;

end.
