unit uDemo05;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , uJX3Number
  , uJX3Boolean
  , uJX3String
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

  TDemo = class(TJX3Object)
    [JS3Required]
    Str:     TJX3Str;                   // A value is required when serializing (Exception)
    [JX3Name('#href')]                  // a name of the json value whiw
    HrefVar: TJX3Str;                   // a name of the json value mapped to "Bool"
    [JX3Default('22')]                  // a defualt value to be used at deserialization, if the field is null
    Num1:    TJX3Num;
    __23href2: TJX3Str;                 // name encoding :  __23href = #hef    ('_'+'_'+Hex('#')+'href')
    [JX3Default('true')]                //                                       ^-- Header
    [JX3Name('NewMix')]
    Mix: TJX3Bool;
  end;
         //

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Demo, Demo3, Demo4, JDemo: TDemo;
  JsonStr: string;
begin
  Demo := Nil;
  JDemo := Nil;
  Demo3 := Nil;
  Memo1.Lines.Clear;
  try
    Demo := TDemo.Create;
    Demo.Str.Value := 'Need a Value';
    Demo.HrefVar.V := 'http://';
    //Demo.Mix.V := True;
    Memo1.Lines.Add('JX3Default Attribute : Num1 default to 22 :');
    Memo1.Lines.Add(Demo.ToJSON([joNulltoEmpty]));

    Memo1.Lines.Add('');
    Memo1.Lines.Add('JX3Name Attribute, Name conversion :');
    JsonStr := '{"Str":"Need a Value","#href":"http","Num1":22}';
    JDemo := TJX3Object.FromJSON<TDemo>(JsonStr);
    Memo1.Lines.Add('Deserialization: #href value is : ' + JDemo.HrefVar.Value);
    JDemo.HrefVar.Value :='ftp';
    Memo1.Lines.Add('Serialization: ' + JDemo.ToJSON([joNulltoEmpty]));

    Memo1.Lines.Add('');
    Demo.__23href2.Value := 'auto enc/dec oding';       // Name encoding: start by '_' and special characters: '_'+Hex Value : # => _23
    Memo1.Lines.Add('Name encoding : ' + Demo.ToJSON([joNulltoEmpty]));

    //Cloning :
    Memo1.Lines.Add('');
    Demo3 := Demo.Clone<TDemo>;
    Memo1.Lines.Add('Clone : ' + Demo3.ToJSON([joNulltoEmpty]));

    //Cloning RTTI:
    Memo1.Lines.Add('');
    Demo4 := Demo.CloneRTTI<TDemo>;
    Memo1.Lines.Add('Clone RTTI: ' + Demo4.ToJSON([joNulltoEmpty]));

    // Options flags:
    //  joNullToEmpty         : Remove null fields
    //  joRaiseException      : Re-raise ecxceptions
    //  joRaiseOnMissingField : Raise an exception when json field is missing in the delphi object; (Debug)
    //  joStats               : Calc. stats (see Lage demo)

    Memo1.Lines.Add('');
    Memo1.Lines.Add('JX3Required exception');
    Demo3.Str.IsNull := True;
    //Demo.Str is null but required >> Exception;
    JsonStr := Demo3.ToJSON([joRaiseException]);        // This flag will re-raise internal exceptions

  finally
    Demo4.Free;
    Demo3.Free;
    JDemo.Free;
    Demo.Free;
  end;

end;

end.
