unit uLarge;

interface



uses
    System.SysUtils
  , System.Types
  , System.UITypes
  , System.Classes
  , System.Variants
  , FMX.Types
  , FMX.Controls
  , FMX.Forms
  , FMX.Graphics
  , FMX.Dialogs
  , FMX.Memo.Types
  , FMX.Controls.Presentation
  , FMX.ScrollBox
  , FMX.Memo
  , FMX.StdCtrls
  , System.Generics.Defaults
  , System.Generics.Collections
  , uJX3Number
  , uJX3Boolean
  , uJX3String
  , uJX3List
  , uJX3Dictionary
  , uJX3Object
  ;

type

  TForm4 = class( TForm )
    Button : TButton;
    Memo1 : TMemo;
    procedure ButtonClick( Sender : TObject );
    private
      { Private declarations }
    public
      { Public declarations }
  end;

  TvalueConst = class( TJX3Object )
    applicableForLocalizedAspectName : TJX3String;
    applicableForLocalizedAspectValues : TJX3List< TJX3String >;
  end;

  TvalueConstraint = class( TJX3Object )
    localizedValue : TJX3String;
    valueConstraints : TJX3List< TvalueConst >;
    applicableForLocalizedAspectName : TJX3String;
    applicableForLocalizedAspectValues : TJX3List< TJX3String >;
  end;

  TaspectValues = class( TJX3Object )
    localizedValue : TJX3String;
    valueConstraints : TJX3List< TvalueConstraint >;
  end;

  TaspectConstraint = class( TJX3Object )
    aspectDataType : TJX3String;
    itemToAspectCardinality : TJX3String;
    aspectMode : TJX3String;
    aspectRequired : TJX3Boolean;
    aspectUsage : TJX3String;
    aspectEnabledForVariations : TJX3Boolean;
    aspectApplicableTo : TJX3List< TJX3String >;
    aspectMaxLength : TJX3Number;
    expectedRequiredByDate : TJX3Number;
    aspectFormat : TJX3String;
  end;

  TcategoryAspectName = class( TJX3Object )
    categoryId : TJX3String;
    categoryName : TJX3String;
  end;

  TcategoryAspect = class( TJX3Object )
    localizedAspectName : TJX3String;
    aspectConstraint : TaspectConstraint;
    aspectValues : TJX3List< TaspectValues >;
  end;

  TcategoryAspects = class( TJX3Object )
    category : TcategoryAspectName;
    aspects : TJX3List< TcategoryAspect >;
  end;

  TfetchItemAspectsContentType = class( TJX3Object )
  public
    categoryTreeId : TJX3String;
    categoryTreeVersion : TJX3String;
    categoryAspects : TJX3List< TcategoryAspects >;
  end;

var
  Form4 : TForm4;

implementation

uses
    System.Diagnostics
  ;

{$R *.fmx}

procedure TForm4.ButtonClick( Sender : TObject );
  var
    LJObj, LJObjClone, LJObjMerge: TfetchItemAspectsContentType;
    LStream : TStringStream;
    LJsonStr : string;
    LWatch : TStopWatch;
    LJSize: Int64;
begin
    Memo1.Lines.Clear;

    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Loading ebay''s Aspects json file :' );
  LJSize := TJX3Object.LoadFromFile('aspects100.json', LJsonStr, TEncoding.UTF8);
    Memo1.Lines.add( Format( '  Stream size: %n KB', [ (LJSize / 1024) ] ));
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Convert Json String to JSX3 Objects (Deserialize):' );
    LWatch := TStopWatch.StartNew;
  LJObj := TJX3Object.FromJSON<TfetchItemAspectsContentType>(LJsonStr, [ joRaiseException] );
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n KB/s', [(LJSize / 1024) / (LWatch.ElapsedMilliseconds / 1000)]));


    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'JSX3 Object Cloning (RTTI):' );
    LWatch := TStopWatch.StartNew;
  LJObjClone := TJX3Object.Clone<TfetchItemAspectsContentType>(LJObj);
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n KB/s', [(LJSize / 1024) / (LWatch.ElapsedMilliseconds / 1000)]));

    Memo1.Lines.add( '' );

    Memo1.Lines.add( 'JSX3 Object Cloning (Merging):' );
    LJObjMerge := TfetchItemAspectsContentType.Create;
    LWatch := TStopWatch.StartNew;
  LJObjClone.Merge(LJObj, LJObjMerge);
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n KB/s', [(LJSize / 1024) / (LWatch.ElapsedMilliseconds / 1000)]));

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Revert JSX3 Objects to Json String (Serialize)):' );
    LWatch := TStopWatch.StartNew;
  LJsonStr := TJX3Object.ToJson( LJObjMerge, [ joNullToEmpty ] );
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n KB/s', [(LJSize / 1024) / (LWatch.ElapsedMilliseconds / 1000)]));

    Memo1.Lines.add( '' );
    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Free Json Objects :' );
  LJObj.Free;
  LJObjClone.Free;
  LJObjMerge.Free;
    Memo1.Lines.add( Format( '  Freed in %d ms', [ LWatch.ElapsedMilliseconds ] ) );

    Memo1.Lines.add( '' );
    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Saving Cloned Json file (jsx3.json) :' );
  LJSize := TJX3Object.SaveToFile( 'jsx3.json', LJsonStr, TEncoding.UTF8);
    Memo1.Lines.add( Format( '  Stream size: %n KB', [ (LJSize / 1024) ] ));
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
  end;

end.
