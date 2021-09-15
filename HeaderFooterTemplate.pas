unit HeaderFooterTemplate;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.DialogService,
  FMX.Memo.Types, FMX.Platform, System.JSON, System.IOUtils, Generics.Collections,
  System.JSON.Readers, System.JSON.Writers, System.JSON.Types, u_urlOpen;

type
  TmainForm = class(TForm)
    Footer: TToolBar;
    resetBtn: TButton;
    openBtn: TButton;
    helpBtn: TButton;
    saveBtn: TButton;
    tagSelector: TPopupBox;
    patternsMemo: TMemo;
    responsesMemo: TMemo;
    fileLabel: TLabel;
    tagLabel: TLabel;
    patternsLabel: TLabel;
    responsesLabel: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    addTagBtn: TButton;
    delTagBtn: TButton;
    aboutBtn: TButton;
    editTagBtn: TButton;
    procedure openBtnClick(Sender: TObject);
    procedure saveBtnClick(Sender: TObject);
    procedure resetBtnClick(Sender: TObject);
    procedure aboutBtnClick(Sender: TObject);
    procedure delTagBtnClick(Sender: TObject);
    procedure addTagBtnClick(Sender: TObject);
    procedure tagSelectorChange(Sender: TObject);
    procedure editTagBtnClick(Sender: TObject);
    procedure helpBtnClick(Sender: TObject);
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
  JsonValues: TJsonObject;
  myDict: TObjectDictionary<string, TIntent>;
  anIntent: TIntent;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.Windows.fmx MSWINDOWS}
{$R *.Macintosh.fmx MACOS}

// Walkit opens the json file, checks that it is an intents file and
// reads the various pattern and response groups into a dictionary, with
// the tag as the dictionary key.

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
  tr := TStringReader.Create(myStringBuilder.ToString);
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
      myDict := TObjectDictionary<string,TIntent>.Create(1);
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
  fileLabel.Text := 'File: ' + ExtractFileName(OpenDialog1.Filename);
  tagSelector.Items.Clear;
  for Key in myDict.Keys do
    tagSelector.Items.Append(Key);
  tagSelector.ItemIndex := 0;
  patternsMemo.Lines.Assign(myDict[tagSelector.Items[tagSelector.ItemIndex]].patterns);
  responsesMemo.Lines.Assign(myDict[tagSelector.Items[tagSelector.ItemIndex]].responses);
  tagSelector.Enabled := true;
  tagSelector.TextAlign := TTextAlign.Leading;
  resetBtn.Enabled := true;
  EditTagBtn.enabled := true;
  DelTagBtn.Enabled := true;
  saveBtn.Enabled := true;
  patternsMemo.Enabled := true;
  responsesMemo.Enabled := true;
end;

// Simple about Btn

procedure TmainForm.aboutBtnClick(Sender: TObject);
begin
  TDialogService.ShowMessage('Intents Editor v0.0.1');
end;

// Handles request to add a new tag. Note that the patterns and responses
// are created with one dummy tag required for the implementation
// If there are no intents loaded the dictionary is created

procedure TmainForm.addTagBtnClick(Sender: TObject);
begin
  TDialogService.InputQuery('New tag',['Tag name:'],[''],
    procedure (const AResult: TModalResult; const AValues: array of string) begin
       if AResult = mrOK then
       begin
        if AValues[0] = '' then
          TDialogService.ShowMessage('Tag name cannot be blank.')
        else if tagSelector.Items.IndexOf(AValues[0]) > -1 then
          TDialogService.ShowMessage('Tag already exists.')
        else begin
          anIntent.responses := TStringlist.Create;
          anIntent.patterns := TStringlist.Create;
          anIntent.responses.Add('<Dummy response, please edit me.>');
          anIntent.patterns.Add('<Dummy pattern, please edit me.>');
          if myDict = nil then
            myDict := TObjectDictionary<string,TIntent>.Create(1);
          myDict.Add(AValues[0],anIntent);
  //        OpenDialog1.FileName := 'New intent.json';
          Populate;
        end;
       end;
    end);
end;

// Handles a request to delete the currently selected tag by deleting it from
// the dictionary and updating everything.

procedure TmainForm.delTagBtnClick(Sender: TObject);
begin
  TDialogService.MessageDialog('Delete this tag and its contents?',
    TMsgDlgType.mtWarning, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure (const AResult: TModalResult) begin
       if AResult = mrYes then
       begin
          myDict.Remove(tagSelector.Items[tagSelector.ItemIndex]);
          Populate;
       end;
    end);
end;

// Handles a request to change the currently selected tag by using
// Extract to get the data and remove it from the dictionary,
// then adding it back to the dictionary with the new tag name as the key

procedure TmainForm.editTagBtnClick(Sender: TObject);
type
  TMyKey = string;
  TMyValue = TIntent;
var
  Pair: TPair<TMyKey,TMyValue>;
begin
  TDialogService.InputQuery('Rename tag',['New name:'],[''],
  procedure (const AResult: TModalResult; const AValues: array of string) begin
       if AResult = mrOK then
       begin
         Pair := myDict.ExtractPair(tagSelector.Items[tagSelector.ItemIndex]);
         myDict.Add(AValues[0],Pair.Value);
         Populate;
       end;
  end);
end;

// The help Btn points to a URL on the iiusatechai web site
// that contains the help content

procedure TmainForm.helpBtnClick(Sender: TObject);
begin
  tUrlOpen.Open('https://www.iiusatechai.com/intent-editor-app.html');
end;

// Handler for the reset button, clears everything to the default state
// as if you had just opened the app.

procedure TmainForm.resetBtnClick(Sender: TObject);
begin
  TDialogService.MessageDialog('Do you really want to clear the workspace?',
    TMsgDlgType.mtWarning, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure (const AResult: TModalResult) begin
       if AResult = mrYes then
       begin
          // Put code here to reset the dictionaries
          fileLabel.Text := 'File:';
          OpenDialog1.FileName := '';
          resetBtn.Enabled := false;
          tagSelector.Enabled := false;
          EditTagBtn.enabled := false;
          DelTagBtn.Enabled := false;
          saveBtn.Enabled := false;
          patternsMemo.Lines.Clear;
          responsesMemo.Lines.Clear;
          patternsMemo.Enabled := false;
          responsesMemo.Enabled := false;
          tagSelector.Items.Clear;
          FreeAndNil(myDict);
       end;
    end);
end;

// On open, try to initially parse the json file. Continue to walkit and
// populate only if the parsing is successful.

procedure TmainForm.openBtnClick(Sender: TObject);
begin
    OpenDialog1.Filter := 'Intent files (*.json)|*.json';
    if OpenDialog1.Execute then
      begin
        WalkIt;
        Populate;
      end
end;

// On any change to a memo field, the entire memo's contents are
// saved to the dictionary (one handler for each memo)

procedure TmainForm.patternsMemoChangeTracking(Sender: TObject);
begin
  if (myDict <> nil) then
       myDict[tagSelector.Items[tagSelector.ItemIndex]].patterns.assign(patternsMemo.Lines);
end;

procedure TmainForm.onResponsesMemoChangeTracking(Sender: TObject);
begin
  if (myDict <> nil) then
        myDict[tagSelector.Items[tagSelector.ItemIndex]].responses.assign(responsesMemo.Lines);
end;

// Handle the request to save the intent to an existing or new filename
// Dialog will prompt for overwrite if the file exists

procedure TmainForm.saveBtnClick(Sender: TObject);
var
  stringWriter: TStringWriter;
  jsonWriter: TJsonTextWriter;
  key: string;
  i: integer;
  patterns, responses: TStringList;
  theFile: TextFile;
begin
    SaveDialog1.Filter := 'Intent files (*.json)|*.json';
    if OpenDialog1.FileName > '' then
    begin
      SaveDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
      SaveDialog1.FileName := ExtractFileName(OpenDialog1.FileName);
    end
    else
      SaveDialog1.InitialDir := GetCurrentDir;
    if SaveDialog1.Execute then
    begin
      stringWriter := TStringWriter.Create;
      jsonWriter := TJsonTextWriter.Create(stringWriter);
      patterns := TStringList.Create;
      responses := TStringList.Create;
      jsonWriter.Formatting := TJsonFormatting.Indented;
      jsonWriter.WriteStartObject;
      jsonWriter.WritePropertyName('intents');
      jsonWriter.WriteStartArray;
      for key in myDict.Keys do
      begin
        jsonWriter.WriteStartObject;
         jsonWriter.WritePropertyName('tag');
         jsonWriter.WriteValue(key);
         patterns.Assign(myDict[key].patterns);
         responses.Assign(myDict[key].responses);
         jsonWriter.WritePropertyName('patterns');
           jsonWriter.WriteStartArray;
           for i := 0 to patterns.Count-1 do
             if patterns[i] > '' then jsonWriter.WriteValue(patterns[i]);
           jsonWriter.WriteEndArray;
         jsonWriter.WritePropertyName('responses');
           jsonWriter.WriteStartArray;
           for i := 0 to responses.Count-1 do
             if responses[i] > '' then jsonWriter.WriteValue(responses[i]);
           jsonWriter.WriteEndArray;
        jsonWriter.WriteEndObject;
      end;
      jsonWriter.WriteEndArray;
      jsonWriter.WriteEndObject;
      AssignFile(theFile,SaveDialog1.Filename);
      Rewrite(theFile);
      WriteLn(theFile,stringWriter.ToString);
      CloseFile(theFile);
      stringWriter.Free;
      jsonWriter.Free;
      patterns.Free;
      responses.Free;
    end;
end;

// Triggered when the user selects a tag from the dropdown menu
// If the dictionary has been populated it gets the intent patterns and responses
// and populates the two editor controls.

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
