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
    NullStr: TJX3Str;
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
  Demo: TObjectDemo;
  Json: string;
  s: TJX3List<TJX3Str>;
begin
  Memo1.Lines.Clear;

  Demo := TObjectDemo.Create;
  Demo.S.V := '~~😃~~'; // UTF8 Support

  // TJX3List<TJX3Str> : Array<string>
  Memo1.lines.add('TJX3List<TJX3Str> : Array<string> :');
  Demo.X.Add(TJX3Str.C('@@@@'));  // "C" to Create with a Value
  Demo.X.Add(TJX3Str.C('EEZZ'));
  Demo.X.Add(TJX3Str.C('OOOO'));
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  //  + TJX3Dic<TJX3Num> : Dictionary<string, number> (JSON only allows strings as keys)
  Memo1.lines.add('');
  Memo1.lines.add('TJX3Dic<TJX3Num> : Dictionary<string, number> :');
  Demo.Y.Add('Value1', TJX3Num.CInt(1111));
  Demo.Y.Add('Value2', TJX3Num.CInt64(2222));
  Demo.Y.Add('Value3', TJX3Num.CInt64(3333));
  Demo.Y.Add('Value4', TJX3Num.CInt64(4444));
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

   //  + TJX3List<TPrimitives>  : Array<TPrimitives)
  Memo1.lines.add('');
  Memo1.lines.add('TJX3List<TPrimitives>  : Array<TPrimitives) :');
  Demo.Z.Add(TPrimitives.Create);
  Demo.Z.Last.Bool.V := True;
  Demo.Z.Last.e.Int64 := 111;
  Demo.Z.Add(TPrimitives.Create);
  Demo.Z.Last.Bool.V := False;
  Demo.Z.Last.e.Int64 := 333;
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  //  + TJX3List<TJX3List<TJX3Str>> : Array<Array<string>>>
  Memo1.lines.add('');
  Memo1.lines.add('TJX3List<TJX3List<TJX3Str>> : Array<Array<string>>> :');
  S := TJX3List<TJX3Str>.Create;
  S.Add(TJX3Str.C('TTT'));
  S.Add(TJX3Str.C('OOO'));
  Demo.Q.Add(S);
  S := TJX3List<TJX3Str>.C;
  S.AddRange([TJX3Str.C('UUU'), TJX3Str.C('III')]);
  Demo.Q.Add(S);
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);
  Memo1.lines.add('');
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  // +  TJX3List<TJX3Dic<TJX3List<TPrimitives>>>  : Array<Dictionary<string, Array<TPrimitives>>> :)
  Memo1.lines.add('');
  Memo1.lines.add('TJX3List<TJX3Dic<TJX3List<TPrimitives>>>  : Array<Dictionary<string, Array<TPrimitives>>>');
  var p1 := TJX3List<TPrimitives>.CAdd(TPrimitives.Create);              // Create a 2 elements Primitives array
  p1.First.Str.v := 'Boolean1';                                          // Acdess the Last item (which is the first also in this case)
  p1.First.Bool.v := True;
  var p2 := TJX3List<TPrimitives>.CAdd(TPrimitives.Create);              // Create a 2 elements Primitives array
  p2[0].Str.V := 'Boolean3';
  p2[0].Bool.v := True;
  var d1 := TJX3Dic<TJX3List<TPrimitives>>.Create;                        // Create the dictionary ownning the 2 lists
  d1.Add('DicVal1', p1);
  d1.Add('DicVal2', p2);
  Demo.w.Add(d1);                                                         // Adding the Dict and its clone to the main list
  Demo.w.Add( TJX3.Clone< TJX3Dic<TJX3List<TPrimitives>> >(d1));
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  Memo1.lines.add('');                                                    // Format
  Memo1.lines.add('Formatted:');
  Memo1.lines.add(TJX3Object.FormatJSON(Json));

  Demo.Free;
end;

end.
