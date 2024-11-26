unit uLarge;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Memo.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  System.Generics.Defaults,
  System.Generics.Collections,
  uJX3Number,
  uJX3Boolean,
  uJX3String,
  uJX3List,
  uJX3Dictionary,
  uJX3Object,
  uJX3Tools;

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

  TFvalueConst = class( TJX3Object )
    applicableForLocalizedAspectName : TJX3String;
    applicableForLocalizedAspectValues : TJX3List< TJX3String >;
  end;

  TFvalueConstraint = class( TJX3Object )
    localizedValue : TJX3String;
    valueConstraints : TJX3List< TFvalueConst >;
    applicableForLocalizedAspectName : TJX3String;
    applicableForLocalizedAspectValues : TJX3List< TJX3String >;
  end;

  TaspectValues = class( TJX3Object )
    localizedValue : TJX3String;
    valueConstraints : TJX3List< TFvalueConstraint >;
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
    categoryId : TJX3Number;
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
      categoryTreeVersion : TJX3Number;
      categoryAspects : TJX3List< TcategoryAspects >;
  end;

var
  Form4 : TForm4;

implementation

uses
  System.Diagnostics;

{$R *.fmx}

procedure TForm4.ButtonClick( Sender : TObject );
  var
    LJObj : TfetchItemAspectsContentType;
    LStream : TStringStream;
    LJsonStr : string;
    LWatch : TStopWatch;
    Count : Integer;
    LStats : TJX3StatBlock;
  begin
    Memo1.Lines.Clear;

    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Loading ebay''s Aspects json file :' );
    LStream := TStringStream.Create( '', TEncoding.UTF8, True );
    LStream.LoadFromFile( 'aspects100.json' );
    LJsonStr := LStream.DataString;
    Memo1.Lines.add( Format( '  Stream size: %s KB',
      [ ( LStream.Size div 1024 ).toString ] ) );
    LStream.Free;

    Memo1.Lines.add( '' );
    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Convert Json String to JSX3 Objects :' );
    LJObj := TJX3Object.FromJSON< TfetchItemAspectsContentType >( LJsonStr,
      [ joNullToEmpty, joDisableNameEncoding ] );
    Memo1.Lines.add( Format( '  Processing duration %d ms',
      [ LWatch.ElapsedMilliseconds ] ) );

    Count := 0;
    for var LLoop1 in LJObj.categoryAspects do
      for var LLoop2 in LLoop1.aspects do
        for var LLoop3 in LLoop2.aspectValues do
          Inc( Count );
    Memo1.Lines.add( '' );
    Memo1.Lines.add( '==>>' + Count.toString + ' Aspect Values !!!' );

    // Using Stats :
    LStats := TJX3StatBlock.Create;
    Memo1.Lines.add( '' );
    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Revert JSX3 Objects to Json String (using Stats) :' );
    LJsonStr := LJObj.ToJson( [ joNullToEmpty, joDisableNameEncoding ],
      LStats );
    Memo1.Lines.add( Format( '  Processing duration %d ms',
      [ LStats.ProcessingTimeMS ] ) );
    LStats.Free;

    Memo1.Lines.add( '' );
    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Free Json Object :' );
    LJObj.Free;
    Memo1.Lines.add( Format( '  Processing duration %d ms',
      [ LWatch.ElapsedMilliseconds ] ) );

    Memo1.Lines.add( '' );
    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Saving ebay''s Aspects Json file (jsx3.json) :' );
    LStream := TStringStream.Create( LJsonStr, TEncoding.UTF8, True );
    LStream.SaveToFile( ( 'jsx3.json' ) );
    Memo1.Lines.add( Format( '  Stream size: %s KB',
      [ ( LStream.Size div 1024 ).toString ] ) );
    LStream.Free;
  end;

end.
