unit HeaderFooterTemplate;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.DialogService,
  FMX.Memo.Types;

type
  TmainForm = class(TForm)
    Footer: TToolBar;
    newButton: TButton;
    openButton: TButton;
    helpButton: TButton;
    saveButton: TButton;
    tagSelector: TPopupBox;
    patternsMemo: TMemo;
    responsesMemo: TMemo;
    fileLabel: TLabel;
    tagLabel: TLabel;
    patternsLabel: TLabel;
    responsesLabel: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure openButtonClick(Sender: TObject);
    procedure saveButtonClick(Sender: TObject);
    procedure newButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mainForm: TmainForm;

implementation

{$R *.fmx}

procedure TmainForm.newButtonClick(Sender: TObject);
begin
TDialogService.MessageDialog('Do you really want to clear the workspace?',
  TMsgDlgType.mtWarning, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure (const AResult: TModalResult) begin
       if AResult = mrYes then
       begin
          // Clear it
       end;
    end);
end;

procedure TmainForm.openButtonClick(Sender: TObject);
begin
    OpenDialog1.Filter := 'Intent files (*.json)|*.json';
    OpenDialog1.Execute;
end;

procedure TmainForm.saveButtonClick(Sender: TObject);
begin
    SaveDialog1.Filter := 'Intent files (*.json)|*.json';
    SaveDialog1.Execute;

end;

end.
