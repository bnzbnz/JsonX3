unit uDemo07;

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

  TBatter = class(TJX3Object)
    id: TJX3Str;
    [JX3Name('type')]
    atype: TJX3Str;
  end;

  TTopping = TBatter;

  TBatters = class(TJX3Object)
    batter: TJX3List<TBatter>;
  end;

  TDonut = class(TJX3Object)
    id: TJX3Str;
    [JX3Name('type')]
    atype: TJX3Str;
    name: TJX3Str;
    ppu: TJX3Num;
    batters: TBatters;
    topping: TJX3List<TTopping>;
  end;

  TEx7 = class(TJX3Object)
    container: TJX3List<TDonut>;
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
begin
  var Json := '{"container":' + Memo1.Lines.Text + '}';     // << because the provided json is an array, we enclose it with a TJXObject container
  var Ex7 := TJX3Object.FromJSON<TEx7>(Json);

  var Ex7Clone := TJX3.Clone<TEx7>(Ex7);                        // for the fun we clone Ex7... :)
  Ex7.Free;

  Memo1.Text := TJX3.FormatJSON( TJX3.ToJSON(Ex7Clone, [joNullToEmpty]) );

  Ex7Clone.Free;
end;

end.
