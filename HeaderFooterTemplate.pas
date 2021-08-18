unit HeaderFooterTemplate;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.DialogService,
  FMX.DialogService.Sync, FMX.Memo.Types, FMX.Platform, System.JSON, System.IOUtils,
  WinJson;

type
  TmainForm = class(TForm)
    Footer: TToolBar;
    resetButton: TButton;
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
    AddTagBtn: TButton;
    DelTagBtn: TButton;
    AboutBtn: TButton;
    EditTagBtn: TButton;
    procedure openButtonClick(Sender: TObject);
    procedure saveButtonClick(Sender: TObject);
    procedure resetButtonClick(Sender: TObject);
    procedure OpenDialog1Close(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure DelTagBtnClick(Sender: TObject);
    procedure AddTagBtnClick(Sender: TObject);
    procedure tagSelectorChange(Sender: TObject);
  private
    { Private declarations }
    procedure Populate;
    procedure WalkIt(theJsonValues: TJson; memoSection: string);
  public
    { Public declarations }
  end;

var
  mainForm: TmainForm;
  JsonValues: TJson;

implementation

{$R *.fmx}

procedure TmainForm.WalkIt(theJsonValues: TJson; memoSection: string);
var
  JsonArray: TJsonArray;
  JsonItem: TJsonItem;
  JsonObject: TJsonObject;
  i: integer;
begin
  if JsonValues is TJsonNull then begin end
  else if JsonValues is TJsonFalse then begin end
  else if JsonValues is TJsonTrue then begin end
  else if JsonValues is TJsonNumber then begin end
  else if JsonValues is TJsonString then
 //    AddChild(Parent, Prefix + '"' + TJsonString(JsonItem).Value + '"')
  else if JsonValues is TJsonArray then
  begin
    JsonArray := TJsonArray(JsonValues);
    for i := 0 to JsonArray.ElementCount - 1 do
  //    Show(Child, '[' + IntToStr(i) + '] ', JsonArray.Elements[i]);
  end
  else if JsonValues is TJsonObject then
  begin
  //  Child := AddChild(Parent, Prefix + '{}');
    JsonObject := TJsonObject(JsonValues);
    for i := 0 to JsonObject.MemberCount - 1 do
  //    Show(Child, JsonObject.MemberName[i] + ': ', JsonObject.MemberValue[i]);
  end;
end;

procedure TmainForm.Populate;
begin

  fileLabel.Text := 'File: ' + OpenDialog1.FileName;
  resetButton.Enabled := true;
  WalkIt(JsonValues,'');
end;

procedure TmainForm.AboutBtnClick(Sender: TObject);
begin
  TDialogService.ShowMessage('Intents Editor v0.0.1');
end;

procedure TmainForm.AddTagBtnClick(Sender: TObject);
begin
  TDialogService.InputQuery('New tag',['Tag name:'],[''],nil);
end;

procedure TmainForm.DelTagBtnClick(Sender: TObject);
begin
  TDialogService.MessageDialog('Delete this tag and its contents?',
    TMsgDlgType.mtWarning, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure (const AResult: TModalResult) begin
       if AResult = mrYes then
       begin
          // Clear it
       end;
    end);
end;

procedure TmainForm.resetButtonClick(Sender: TObject);
begin
  TDialogService.MessageDialog('Do you really want to clear the workspace?',
    TMsgDlgType.mtWarning, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure (const AResult: TModalResult) begin
       if AResult = mrYes then
       begin
          fileLabel.Text := 'File:';
          JsonValues.Free;
          JsonValues := nil;
          resetButton.Enabled := false;
          patternsMemo.Lines.Clear;
          responsesMemo.Lines.Clear;
          tagSelector.Items.Clear;
       end;
    end);
end;

procedure TmainForm.openButtonClick(Sender: TObject);
begin
    OpenDialog1.Filter := 'Intent files (*.json)|*.json';
    OpenDialog1.Execute;
end;

procedure TmainForm.OpenDialog1Close(Sender: TObject);
begin
  with TJsonParser.Create do
    begin
      JsonValues := ParseUtf8File(OpenDialog1.FileName);
    end;
  if (JsonValues <> nil) then // and first tag is "intents"
    Populate;
end;

procedure TmainForm.saveButtonClick(Sender: TObject);
begin
    SaveDialog1.Filter := 'Intent files (*.json)|*.json';
    SaveDialog1.Execute;
end;

procedure TmainForm.tagSelectorChange(Sender: TObject);
begin
  if tagSelector.ItemIndex > -1 then
    begin

    end;
end;

end.
