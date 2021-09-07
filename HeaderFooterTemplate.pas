unit HeaderFooterTemplate;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.DialogService,
  FMX.DialogService.Sync, FMX.Memo.Types, FMX.Platform, System.JSON, System.IOUtils,
  WinJson, Generics.Collections, System.JSON.Readers, System.JSON.Types, u_urlOpen;

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
    procedure AboutBtnClick(Sender: TObject);
    procedure DelTagBtnClick(Sender: TObject);
    procedure AddTagBtnClick(Sender: TObject);
    procedure tagSelectorChange(Sender: TObject);
    procedure EditTagBtnClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure onResponsesMemoChangeTracking(Sender: TObject);
    procedure patternsMemoChangeTracking(Sender: TObject);
  private
    { Private declarations }
    procedure Populate;
    procedure WalkIt;
  public
    { Public declarations }
  end;

type
  TIntent = record
    patterns: TStringList;
    responses: TStringList
  end;

var
  mainForm: TmainForm;
  JsonValues: TJson;
  myDict: TDictionary<string, TIntent>;
  anIntent: TIntent;

implementation

{$R *.fmx}

// Walkit opens the json file, checks that it is an intents file and
// reads the various pattern and response groups into a dictionary, with
// the tag as the dictionary key

procedure TmainForm.WalkIt;
var
  theFile: TextFile;
  myStringBuilder: TStringBuilder;
  aLine,currentTag: string;
  JSONReader: TJsonTextReader;
  tr: TStringReader;
begin
  AssignFile(theFile,OpenDialog1.Filename);
  Reset(theFile);
  myStringBuilder := TStringBuilder.Create(FileSize(theFile));
  while not Eof(theFile) do
  begin
    ReadLn(theFile,aLine);
    myStringBuilder.Append(aLine)
  end;
  CloseFile(theFile);
  tr := TStringReader.Create(myStringBuilder.Text);
  JSONReader := TJsonTextReader.Create(tr);
  with JSONReader do
  try
    repeat
      Read;
    until TokenType = TJSONToken.PropertyName;
    if Value.AsString <> 'intents' then // Look for 'intents'
      ShowMessage('This file is not an intents definition.')
    else
    begin
      myDict := TDictionary<string,TIntent>.Create(1);
      while Read do
        case JSONReader.TokenType of
          TJSONToken.PropertyName:
            begin
              if Value.ToString = 'tag' then
              begin
                Read;
                currentTag := Value.ToString;
              end
              else if Value.ToString = 'patterns' then
              begin
                anIntent.patterns := TStringlist.Create;
                Read;
                while TokenType <> TJSONToken.EndArray do  // read the patterns
                  begin
                    Read;
                    anIntent.patterns.Add(Value.AsString);
                end
              end
              else
              begin
                anIntent.responses := TStringlist.Create;
                Read;
                while TokenType <> TJSONToken.EndArray do  // read the responses
                begin
                  Read;
                  anIntent.responses.Add(Value.AsString);
                end;
                myDict.Add(currentTag,anIntent); // add the patterns and responses to the dict
              end
            end;
        end;
      end
  finally  // The only object that remains is the dictionary, rest are freed
    JSONreader.free;
    tr.free;
    myStringBuilder.free;
  end;
end;

// From the selected tag, populates the various fields and edit areas

procedure TmainForm.Populate;
var
  Key: string;
begin
  fileLabel.Text := 'File: ' + OpenDialog1.Filename;
  WalkIt;
  for Key in myDict.Keys do
    tagSelector.Items.Append(Key);
  tagSelector.ItemIndex := 0;
  patternsMemo.Lines.Assign(myDict[tagSelector.Items[tagSelector.ItemIndex]].patterns);
  responsesMemo.Lines.Assign(myDict[tagSelector.Items[tagSelector.ItemIndex]].responses);
  tagSelector.Enabled := true;
  tagSelector.TextAlign := TTextAlign.Leading;
  resetButton.Enabled := true;
  EditTagBtn.enabled := true;
  DelTagBtn.Enabled := true;
  saveButton.Enabled := true;
  patternsMemo.Enabled := true;
  responsesMemo.Enabled := true;
end;

// Simple about button

procedure TmainForm.AboutBtnClick(Sender: TObject);
begin
  TDialogService.ShowMessage('Intents Editor v0.0.1');
end;

// Handles request to add a new tag. Note that the patterns and responses
// are created with one dummy tag required for the implementation

procedure TmainForm.AddTagBtnClick(Sender: TObject);
begin
  TDialogService.InputQuery('New tag',['Tag name:'],[''],nil);
end;

// Handles a request to delete the currently selected tag.

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

// Handles a request to change the currently selected tag.

procedure TmainForm.EditTagBtnClick(Sender: TObject);
begin
  TDialogService.InputQuery('Edit tag name',['Tag name:'],[''],nil);
end;

// The help button points to a URL on the iiusatechai web site
// that contains the help content

procedure TmainForm.helpButtonClick(Sender: TObject);
begin
  tUrlOpen.Open('https://www.iiusatechai.com/intent-editor-app.html');
end;

// Handler for the reset button

procedure TmainForm.resetButtonClick(Sender: TObject);
begin
  TDialogService.MessageDialog('Do you really want to clear the workspace?',
    TMsgDlgType.mtWarning, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure (const AResult: TModalResult) begin
       if AResult = mrYes then
       begin
          // Put code here to reset the dictionaries
          fileLabel.Text := 'File:';
          resetButton.Enabled := false;
          tagSelector.Enabled := false;
          EditTagBtn.enabled := false;
          DelTagBtn.Enabled := false;
          saveButton.Enabled := false;
          patternsMemo.Lines.Clear;
          responsesMemo.Lines.Clear;
          patternsMemo.Enabled := false;
          responsesMemo.Enabled := false;
          tagSelector.Items.Clear;
          myDict.free;
       end;
    end);
end;

// On open, try to initially parse the json file. Continue to populate
// onlyi if the parsing is successful

procedure TmainForm.openButtonClick(Sender: TObject);
begin
    OpenDialog1.Filter := 'Intent files (*.json)|*.json';
    if OpenDialog1.Execute then
    begin
      with TJsonParser.Create do
      begin
        JsonValues := ParseUtf8File(OpenDialog1.FileName);
      end;
      if (JsonValues <> nil) then
        Populate;
    end;
end;

procedure TmainForm.patternsMemoChangeTracking(Sender: TObject);
begin
  if (myDict <> nil) then
       myDict[tagSelector.Items[tagSelector.ItemIndex]].patterns.assign(patternsMemo.Lines);
end;

// On any change to a memo field, the entire memo's
// contents are saved to the dictionary

procedure TmainForm.onResponsesMemoChangeTracking(Sender: TObject);
begin
  if (myDict <> nil) then
        myDict[tagSelector.Items[tagSelector.ItemIndex]].responses.assign(responsesMemo.Lines);
end;

// Handle the request to save the intent to an existing or new filename

procedure TmainForm.saveButtonClick(Sender: TObject);
begin
    SaveDialog1.Filter := 'Intent files (*.json)|*.json';
    SaveDialog1.Execute;
end;

// Triggered when the user selects a tag from the dropdown menu

procedure TmainForm.tagSelectorChange(Sender: TObject);
begin
  if tagSelector.ItemIndex > -1 then
    begin
      if myDict <> nil then
      begin
        myDict.TryGetValue(tagSelector.Items[tagSelector.ItemIndex],anIntent);
        patternsMemo.Lines.Assign(anIntent.patterns);
        responsesMemo.Lines.Assign(anIntent.responses);
      end;
    end;
end;

end.
