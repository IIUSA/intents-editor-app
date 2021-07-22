unit IntentsEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TmainForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    newButton: TButton;
    openButton: TButton;
    helpButton: TButton;
    saveButton: TButton;
    tagLabel: TLabel;
    fileLael: TLabel;
    tagSelectPopupBox: TPopupBox;
    fileNameLabel: TLabel;
    patternsLabel: TLabel;
    patternsMemo: TMemo;
    responsesLabel: TLabel;
    responsesMemo: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mainForm: TmainForm;

implementation

{$R *.fmx}

end.
