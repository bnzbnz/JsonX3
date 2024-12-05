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

  TDemoContainer = class(TJX3Object)
  public
    StringList : TJSONableStringList; // Creation and destruction will handle automatically

    [JX3Unmanaged]
    StringListNotManaged : TJSONableStringList; // NOT MANAGED : You have to take care of the Creation/Destruction of this Object;
  end;                                          // It will still be serialized/deserialized

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Json: string;
  Obj, NewObj: TDemoContainer;
  MyList:  TJSONableStringList;
begin
  Memo1.Lines.Clear;

  MyList :=  TJSONableStringList.Create;      // we create a "jsonable" object
  MyList.Add('Not');


  Obj := TDemoContainer.Create;         // will also create "Obj.StringList", but not "StringListNotManaged" as it it Not Managed

  Obj.StringList.Add('A');              // Again, we dont have to take care of the "StringList" creation, TJX3Object handles it for us!
  Obj.StringList.Add('B');
  Obj.StringList.Add('C');
  Obj.StringList.Add('D');

  Obj.StringListNotManaged := MyList;   // This external field is nil, we assign it

  // Raw Json
  Json := Obj.ToJson([]);

  // Formatted Json
  Memo1.lines.add('List Raw:');
  Memo1.lines.add(Json);

  Obj.StringList.Strings[0] := '>>';      // we update the lists
  Obj.StringList.Strings[2] := '<<';
  MyList.Add('Managed');

  // Updated Json
  Json := Obj.ToJson([]);
  Memo1.lines.add('');
  Memo1.lines.add('Updated:');
  Memo1.lines.add(Json);

  // Cloned Json
  NewObj := Obj.Clone<TDemoContainer>;    // we clone the json object as NewObj
  Json := NewObj.ToJson([]);              // Serialize the new object;
  Memo1.lines.add('');
  Memo1.lines.add('Cloned and Formatted:');
  Memo1.lines.add(Json);

  NewObj.Free;
  Obj.Free;

  MyList.Free; // we destroy the list...
end;

end.
