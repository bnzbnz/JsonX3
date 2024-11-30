unit uDemo04;

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
  , uJSONableStringList
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

  TJSObject = class(TJX3Object)
  public
    StringList : TJSONableStringList;
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Json: string;
  Obj, NewObj: TJSObject;
begin
  Memo1.Lines.Clear;

  Obj := TJSObject.Create;      // Again, we dont have to take care of the stringlist creation, TJX3Object handles it for us!
                                // It will call Create() and then JSONInit
  Obj.StringList.Add('A');
  Obj.StringList.Add('B');
  Obj.StringList.Add('C');
  Obj.StringList.Add('D');

  // Raw Json
  Json := Obj.ToJson([]);

  // Formatted Json
  Memo1.lines.add('List Raw:');
  Memo1.lines.add(Json);

  Obj.StringList.Strings[0] := '>>';
  Obj.StringList.Strings[2] := '<<';
  Json := Obj.ToJson([]);

  // Updated and Formatted Json
  Memo1.lines.add('');
  Memo1.lines.add('Updated and Formatted:');
  Memo1.lines.add(Json);

  NewObj := Obj.Clone<TJSObject>;

  // Cloned, updated and Formatted Json
  Memo1.lines.add('');
  Memo1.lines.add('Cloned and Formatted:');
  Memo1.lines.add(Json);

  NewObj.Free;
  Obj.Free;
end;

end.
