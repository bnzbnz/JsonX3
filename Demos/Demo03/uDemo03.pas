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

  TPrimitive = class(TJX3Object)
    Str: TJX3Str;
    Bool: TJX3Bool;
    I: TJX3Num; // as Int
    UI: TJX3Num; // as UInt
    Dble: TJX3Num; // as Double
    Curr: TJX3Num; // as Vurrency
    NullStr: TJX3Str;
    // ...
  end;

  TObjectDemo = class(TJX3Object)
    Str:  TJX3Str;
    Keys: TJX3List<TJX3Str>;                            // an array(List) of strings : TArray<string>
    Nums: TJX3Dic<TJX3Num>;                             // An dictionary of Numbers (<string, number>)  *JSON allows only strings as key
    Primitives: TJX3List<TPrimitive>;                   // A list of TPrimitives
    SLists: TJX3List<TJX3List<TJX3Str>>;                // A list of string Lists
    PDicList: TJX3List<TJX3Dic<TJX3List<TPrimitive>>>;  // ouch ! A List of dictionaries of TPrimitives Objects Lists !!!
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
  Demo.Str.V := '~~😃~~'; // UTF8 Support

  // TJX3List<TJX3Str> : Array<string>
  Memo1.lines.add('TJX3List<TJX3Str> : Array<string> :');
  Demo.Keys.Add(TJX3Str.New('Q W E R T Y'));
  Demo.Keys.Add(TJX3Str.New('A Z E R T Y'));
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  // + TJX3Dic<TJX3Num> : Dictionary<string, number> (JSON only allows strings as keys)
  Memo1.lines.add('');
  Memo1.lines.add('TJX3Dic<TJX3Num> : Dictionary<string, number> :');
  Demo.Nums.Add('Int', TJX3Num.NewInt(1111));
  Demo.Nums.Add('Int64', TJX3Num.NewInt64(2222));
  Demo.Nums.Add('Double', TJX3Num.NewDouble(33.33));
  Demo.Nums.Add('Currency', TJX3Num.NewCurrency(44.44));
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  // TJX3List<TPrimitives> : Array<TPrimitives)
  Memo1.lines.add('');
  Memo1.lines.add('TJX3List<TPrimitives>  : Array<TPrimitives) :');
  Demo.Primitives.Add(TPrimitive.Create);
  Demo.Primitives.Last.Bool.Value := True;
  Demo.Primitives.Last.I.Int64 := 111;
  Demo.Primitives.Add(TPrimitive.Create);
  Demo.Primitives.Last.Bool.Value := False;
  Demo.Primitives.Last.Dble.Int64 := 333;
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  // TJX3List<TJX3List<TJX3Str>> : Array<Array<string>>>
  Memo1.lines.add('');
  Memo1.lines.add('TJX3List<TJX3List<TJX3Str>> : Array<Array<string>>> :');
  S := TJX3List<TJX3Str>.Create;
  S.Add(TJX3Str.New('TTT'));
  S.Add(TJX3Str.New('OOO'));
  Demo.SLists.Add(S);
  S := TJX3List<TJX3Str>.New;
  S.AddRange([TJX3Str.New('XXX'), TJX3Str.New('YYY'), TJX3Str.New('ZZZ')]);
  Demo.SLists.Add(S);
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);
  Memo1.lines.add('');
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  // TJX3List<TJX3Dic<TJX3List<TPrimitives>>> : Array<Dictionary<string, Array<TPrimitives>>> :)
  Memo1.lines.add('');
  Memo1.lines.add('TJX3List<TJX3Dic<TJX3List<TPrimitives>>>  : Array<Dictionary<string, Array<TPrimitives>>>');
  var p1 := TJX3List<TPrimitive>.NewAdd(TPrimitive.Create);              // Create a 2 elements Primitives array
  p1.First.Str.v := 'Boolean1';                                            // Acdess the Last item (which is the first also in this case)
  p1.First.Bool.v := True;
  var p2 := TJX3List<TPrimitive>.NewAdd(TPrimitive.Create);              // Create a 2 elements Primitives array
  p2[0].Str.V := 'Boolean2';
  p2[0].Bool.v := True;
  var d1 := TJX3Dic<TJX3List<TPrimitive>>.Create;                        // Create the dictionary ownning the 2 lists
  d1.Add('DicVal1', p1);
  d1.Add('DicVal2', p2);
  Demo.PDicList.Add(d1);                                                         // Adding the Dict and its clone to the main list
  Demo.PDicList.Add( TJX3.Clone< TJX3Dic<TJX3List<TPrimitive>> >(d1));
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  Memo1.lines.add('');                                                    // Format
  Memo1.lines.add('Formatted:');
  Memo1.lines.add(TJX3Object.FormatJSON(Json));

  Demo.Free;
end;

end.
