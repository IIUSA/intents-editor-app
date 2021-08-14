// ----------------------------------------------------------------------------
// JSON 4.8
// Copyright (c) 2013-2021 WINSOFT
// ----------------------------------------------------------------------------

//{$define TRIAL}

unit WinJson;

{$ifdef CONDITIONALEXPRESSIONS}
  {$if CompilerVersion >= 24} // Delphi XE3 or higher
    {$LEGACYIFEND ON}
  {$ifend}

  {$if CompilerVersion >= 14}
    {$define D6PLUS} // Delphi 6 or higher
  {$ifend}

  {$if CompilerVersion >= 20}
    {$define D2009PLUS} // Delphi 2009 or higher
  {$ifend}

  {$if CompilerVersion >= 22}
    {$define DXEPLUS} // Delphi XE or higher
  {$ifend}

  {$if CompilerVersion >= 34}
    {$define D104PLUS} // Delphi 10.4 or higher
  {$ifend}
{$else}
  {$define MSWINDOWS}
{$endif}

{$ifdef FPC}
{$mode DELPHI}
{$endif FPC}

interface

{$ifdef NEXTGEN}
uses System.Classes, System.SysUtils, System.Types;
{$else}
  {$ifdef D104PLUS}
    uses System.Classes, System.SysUtils, System.Types;
  {$else}
    uses Classes, SysUtils;
  {$endif D104PLUS}
{$endif NEXTGEN}

const
  JsonMIME = 'application/json';
  JsonFileExtension = '.json';

type
{$ifdef D2009PLUS}
  WideString = string;
  WideChar = Char;
{$else}
  TBytes = array of Byte;
{$endif D2009PLUS}

  EJsonError = class(Exception)
  end;

{$ifndef NEXTGEN}
  TStringBuilder = class;
{$endif NEXTGEN}

  TJsonNull = class;
  TJsonFalse = class;
  TJsonTrue = class;
  TJsonObject = class;
  TJsonString = class;
  TJsonArray = class;
  TJsonNumber = class;

  TJson = class
  private
    function Clone: TJson; virtual; abstract;
    function GetAt(Index: Integer): TJson; virtual;
    function GetNullAt(Index: Integer): TJsonNull; virtual;
    function GetFalseAt(Index: Integer): TJsonFalse; virtual;
    function GetTrueAt(Index: Integer): TJsonTrue; virtual;
    function GetObjectAt(Index: Integer): TJsonObject; virtual;
    function GetStringAt(Index: Integer): TJsonString; virtual;
    function GetArrayAt(Index: Integer): TJsonArray; virtual;
    function GetNumberAt(Index: Integer): TJsonNumber; virtual;

    function GetItem(const Name: WideString): TJson; virtual;
    function GetNullItem(const Name: WideString): TJsonNull; virtual;
    function GetFalseItem(const Name: WideString): TJsonFalse; virtual;
    function GetTrueItem(const Name: WideString): TJsonTrue; virtual;
    function GetObjectItem(const Name: WideString): TJsonObject; virtual;
    function GetStringItem(const Name: WideString): TJsonString; virtual;
    function GetArrayItem(const Name: WideString): TJsonArray; virtual;
    function GetNumberItem(const Name: WideString): TJsonNumber; virtual;

    procedure SetAt(Index: Integer; Value: TJson); virtual;
    procedure SetItem(const Name: WideString; Value: TJson); virtual;
    procedure Write(StringBuilder: TStringBuilder); overload; virtual; abstract;
    procedure Write(StringBuilder: TStringBuilder; TabChar: WideChar; TabSize, Indentation: Integer); overload; virtual;
  public
    function AsBoolean: Boolean; virtual;
    function AsDateTime: TDateTime; virtual;
    function AsDateTimeOffset: Integer; virtual;
    function AsNumber: Double; virtual;
    function AsString: WideString; virtual;

    property At[Index: Integer]: TJson read GetAt write SetAt;

    property NullAt[Index: Integer]: TJsonNull read GetNullAt;
    property FalseAt[Index: Integer]: TJsonFalse read GetFalseAt;
    property TrueAt[Index: Integer]: TJsonTrue read GetTrueAt;
    property ObjectAt[Index: Integer]: TJsonObject read GetObjectAt;
    property StringAt[Index: Integer]: TJsonString read GetStringAt;
    property ArrayAt[Index: Integer]: TJsonArray read GetArrayAt;
    property NumberAt[Index: Integer]: TJsonNumber read GetNumberAt;

    property Items[const Name: WideString]: TJson read GetItem write SetItem; default;

    property NullItem[const Name: WideString]: TJsonNull read GetNullItem;
    property FalseItem[const Name: WideString]: TJsonFalse read GetFalseItem;
    property TrueItem[const Name: WideString]: TJsonTrue read GetTrueItem;
    property ObjectItem[const Name: WideString]: TJsonObject read GetObjectItem;
    property StringItem[const Name: WideString]: TJsonString read GetStringItem;
    property ArrayItem[const Name: WideString]: TJsonArray read GetArrayItem;
    property NumberItem[const Name: WideString]: TJsonNumber read GetNumberItem;

    function IsArray: Boolean; virtual;
    function IsBoolean: Boolean; virtual;
    function IsDateTime: Boolean; virtual;
    function IsLiteral: Boolean; virtual;
    function IsNull: Boolean; virtual;
    function IsNumber: Boolean; virtual;
    function IsObject: Boolean; virtual;
    function IsString: Boolean; virtual;

    function ToString(PrettyPrint: Boolean = True; UseTabChar: Boolean = False; TabSize: Integer = 2): WideString; reintroduce;
{$ifdef NEXTGEN}
    function ToUtf8(PrettyPrint: Boolean = True; UseTabChar: Boolean = False; TabSize: Integer = 2): TBytes;
{$else}
    function ToUtf8(PrettyPrint: Boolean = True; UseTabChar: Boolean = False; TabSize: Integer = 2): AnsiString;
{$endif NEXTGEN}
    procedure ToUtf8File(const FileName: string; PrettyPrint: Boolean = True; UseTabChar: Boolean = False; TabSize: Integer = 2);
  end;

  TJsonLiteral = class(TJson)
  private
    FFreeInstance: Boolean;
  public
    function IsLiteral: Boolean; override;
    procedure FreeInstance; override;
  end;

  TJsonNull = class(TJsonLiteral)
  private
    function Clone: TJson; override;
    procedure Write(StringBuilder: TStringBuilder); override;
  public
    function IsNull: Boolean; override;
  end;

  TJsonFalse = class(TJsonLiteral)
  private
    function Clone: TJson; override;
    procedure Write(StringBuilder: TStringBuilder); override;
  public
    function AsBoolean: Boolean; override;
    function IsBoolean: Boolean; override;
  end;

  TJsonTrue = class(TJsonLiteral)
  private
    function Clone: TJson; override;
    procedure Write(StringBuilder: TStringBuilder); override;
  public
    function AsBoolean: Boolean; override;
    function IsBoolean: Boolean; override;
  end;

  TJsonNumber = class(TJson)
  private
    FValue: Double;
    constructor Create(Value: Double);
    function Clone: TJson; override;
    procedure Write(StringBuilder: TStringBuilder); override;
  public
    function AsNumber: Double; override;
    function IsNumber: Boolean; override;
    property Value: Double read FValue;
  end;

  TJsonString = class(TJson)
  private
    FValue: WideString;
    constructor Create(const Value: WideString);
    function Clone: TJson; override;
    procedure Write(StringBuilder: TStringBuilder); override;
    function GetDateTime: TDateTime;
    function GetDateTimeOffset: Integer;
  public
    function AsString: WideString; override;
    function AsDateTime: TDateTime; override;
    function AsDateTimeOffset: Integer; override;

    function IsDateTime: Boolean; override;
    function IsString: Boolean; override;

    property Value: WideString read FValue;
    property DateTime: TDateTime read GetDateTime;
    property DateTimeOffset: Integer read GetDateTimeOffset;
  end;

  TJsonArray = class(TJson)
  private
    FElements: array of TJson;
    procedure CheckIndex(Index: Integer);
    function GetAt(Index: Integer): TJson; override;
    procedure SetAt(Index: Integer; Value: TJson); override;
    function GetElementCount: Integer;
    procedure SetElementCount(Value: Integer);
    function GetElement(Index: Integer): TJson;
    procedure SetElement(Index: Integer; Element: TJSon);
    procedure SetRawElement(Index: Integer; Element: TJSon);
    function Clone: TJson; override;
    function GetNullAt(Index: Integer): TJsonNull; override;
    function GetFalseAt(Index: Integer): TJsonFalse; override;
    function GetTrueAt(Index: Integer): TJsonTrue; override;
    function GetObjectAt(Index: Integer): TJsonObject; override;
    function GetStringAt(Index: Integer): TJsonString; override;
    function GetArrayAt(Index: Integer): TJsonArray; override;
    function GetNumberAt(Index: Integer): TJsonNumber; override;
    procedure Write(StringBuilder: TStringBuilder); override;
    procedure Write(StringBuilder: TStringBuilder; TabChar: WideChar; TabSize, Indentation: Integer); override;
  public
    constructor Create(ElementCount: Integer = 0);
    destructor Destroy; override;
    property Elements[Index: Integer]: TJson read GetElement write SetElement; default;
    property ElementCount: Integer read GetElementCount write SetElementCount;

    property NullAt[Index: Integer]: TJsonNull read GetNullAt;
    property FalseAt[Index: Integer]: TJsonFalse read GetFalseAt;
    property TrueAt[Index: Integer]: TJsonTrue read GetTrueAt;
    property ObjectAt[Index: Integer]: TJsonObject read GetObjectAt;
    property StringAt[Index: Integer]: TJsonString read GetStringAt;
    property ArrayAt[Index: Integer]: TJsonArray read GetArrayAt;
    property NumberAt[Index: Integer]: TJsonNumber read GetNumberAt;

    function IsArray: Boolean; override;

    procedure AppendNull;
    procedure AppendFalse;
    procedure AppendTrue;
    procedure AppendNumber(Value: Double);
    procedure AppendString(const Value: WideString);
    procedure AppendDateTime(const Value: TDateTime; Offset: Integer = 0);
    function AppendArray(ElementCount: Integer): TJsonArray;
    function AppendObject: TJsonObject;
    function AppendJson(Value: TJSon): TJson;

    procedure InsertNull(Index: Integer);
    procedure InsertFalse(Index: Integer);
    procedure InsertTrue(Index: Integer);
    procedure InsertNumber(Index: Integer; Value: Double);
    procedure InsertString(Index: Integer; const Value: WideString);
    procedure InsertDateTime(Index: Integer; const Value: TDateTime; Offset: Integer = 0);
    function InsertArray(Index: Integer; ElementCount: Integer): TJsonArray;
    function InsertObject(Index: Integer): TJsonObject;
    function InsertJson(Index: Integer; Value: TJSon): TJson;

    procedure SetNull(Index: Integer);
    procedure SetFalse(Index: Integer);
    procedure SetTrue(Index: Integer);
    procedure SetNumber(Index: Integer; Value: Double);
    procedure SetString(Index: Integer; const Value: WideString);
    procedure SetDateTime(Index: Integer; const Value: TDateTime; Offset: Integer = 0);
    function SetArray(Index: Integer; ElementCount: Integer): TJsonArray;
    function SetObject(Index: Integer): TJsonObject;
    function SetJson(Index: Integer; Value: TJson): TJson;

    procedure Delete(Index: Integer);
    procedure Swap(Index1, Index2: Integer);
  end;

  TJsonMember = record
    FName: WideString;
    FValue: TJson;
  end;

  TJsonObject = class(TJson)
  private
    FMembers: array of TJsonMember;
    FMemberCount: Integer;
    function GetMemberCapacity: Integer;
    procedure CheckIndex(Index: Integer);
    procedure ClearMember(Index: Integer);
    function GetItem(const Name: WideString): TJson; override;
    procedure SetItem(const Name: WideString; Value: TJson); override;

    function GetNullItem(const Name: WideString): TJsonNull; override;
    function GetFalseItem(const Name: WideString): TJsonFalse; override;
    function GetTrueItem(const Name: WideString): TJsonTrue; override;
    function GetObjectItem(const Name: WideString): TJsonObject; override;
    function GetStringItem(const Name: WideString): TJsonString; override;
    function GetArrayItem(const Name: WideString): TJsonArray; override;
    function GetNumberItem(const Name: WideString): TJsonNumber; override;

    function GetMemberName(Index: Integer): WideString;
    procedure SetMemberName(Index: Integer; const Value: WideString);
    function GetMemberValue(Index: Integer): TJson;
    procedure SetMemberValue(Index: Integer; Value: TJson);
    function Clone: TJson; override;
    procedure Write(StringBuilder: TStringBuilder); override;
    procedure Write(StringBuilder: TStringBuilder; TabChar: WideChar; TabSize, Indentation: Integer); override;
    function GetMember(const Name: WideString): TJson;
    procedure SetMember(const Name: WideString; Value: TJSon);
    procedure SetRawMember(Index: Integer; const Name: WideString; Value: TJSon);
    property MemberCapacity: Integer read GetMemberCapacity;
  public
    constructor Create;
    destructor Destroy; override;
    property MemberCount: Integer read FMemberCount;
    property MemberName[Index: Integer]: WideString read GetMemberName write SetMemberName;
    property MemberValue[Index: Integer]: TJson read GetMemberValue write SetMemberValue;
    property Members[const Name: WideString]: TJson read GetMember write SetMember; default;

    property NullItem[const Name: WideString]: TJsonNull read GetNullItem;
    property FalseItem[const Name: WideString]: TJsonFalse read GetFalseItem;
    property TrueItem[const Name: WideString]: TJsonTrue read GetTrueItem;
    property ObjectItem[const Name: WideString]: TJsonObject read GetObjectItem;
    property StringItem[const Name: WideString]: TJsonString read GetStringItem;
    property ArrayItem[const Name: WideString]: TJsonArray read GetArrayItem;
    property NumberItem[const Name: WideString]: TJsonNumber read GetNumberItem;

    function Find(const Name: WideString): Integer;
    procedure DeleteAll;
    procedure Delete(Index: Integer); overload;
    procedure Delete(const Name: WideString); overload;
    function IsObject: Boolean; override;
    procedure SetNull(const Name: WideString);
    procedure SetFalse(const Name: WideString);
    procedure SetTrue(const Name: WideString);
    procedure SetNumber(const Name: WideString; Value: Double);
    procedure SetString(const Name, Value: WideString);
    procedure SetDateTime(const Name: WideString; const Value: TDateTime; Offset: Integer = 0);
    function SetArray(const Name: WideString; ElementCount: Integer): TJsonArray;
    function SetObject(const Name: WideString): TJsonObject;
    function SetJson(const Name: WideString; Value: TJson): TJson;
  end;

{$ifndef NEXTGEN}
  TStringBuilder = class
  private
    FText: WideString;
    FLength: Integer;
    function GetText: WideString;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    procedure EnsureCapacity(Value: Integer);
  public
    constructor Create(Capacity: Integer);
    procedure Clear;
    procedure Append(Value: WideChar); overload;
    procedure Append(const Value: WideString); overload;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Text: WideString read GetText;
  end;
{$endif NEXTGEN} 

  TJsonCustomParser = class
  private
    FText: WideString;
    FLastColumn: Integer; // for optimized Row and Column calculation
    FLastPosition: Integer; // for optimized Row and Column calculation
    FLastRow: Integer; // for optimized Row and Column calculation
    FPosition: Integer;
    FStringBuilder: TStringBuilder;
    function IsEnd: Boolean;
    function Current: WideChar;
    procedure Next;
    function IsWhiteSpace: Boolean;
    procedure SkipWhiteSpace;
    function IsStructuralCharacter: Boolean;
    function IsDigit: Boolean;
    procedure PositionToRowColumn(Position: Integer; var Row, Column: Integer);
    procedure Error(Position: Integer; const ErrorMessage: string);
    procedure Read(C: WideChar);
    function ReadDigit: Integer;
    function ReadHexaDigit: Integer;
    function ParseNumber: Double;
    function ParseString: WideString;
  protected
    constructor Create;
    function Parse(const Text: WideString): TJson; virtual;
  public
    destructor Destroy; override;
    function ParseUtf8(const Utf8: TBytes): TJson; overload;
    function ParseAnsi(const Ansi: TBytes): TJson; overload;
{$ifndef NEXTGEN}
    function ParseUtf8(const Utf8: AnsiString): TJson; overload;
    function ParseAnsi(const Ansi: AnsiString): TJson; overload;
{$endif NEXTGEN}
    function ParseUtf8(Stream: TStream): TJson; overload;
    function ParseAnsi(Stream: TStream): TJson; overload;
    function ParseUtf8File(const FileName: string): TJson;
    function ParseAnsiFile(const FileName: string): TJson;
  end;

  TJsonParser = class(TJsonCustomParser)
  private
    function ParseValue: TJson;
    function ParseLiteral: TJson;
    function ParseObject: TJsonObject;
    function ParseArray: TJsonArray;
  public
    constructor Create;
    function Parse(const Text: WideString): TJson; override;
  end;

  TJsonWriter = class
  private
    FNesting: array of Boolean;
    FNestedLevel: Integer;
    FDestroyStream: Boolean;
    FFirstItem: Boolean;
    FPrettyPrint: Boolean;
    FStream: TStream;
    FTabSize: Integer;
    FUseTabChar: Boolean;
    FUtf8: Boolean;
    procedure AddNesting(IsObject: Boolean);
    function InArray: Boolean;
    function InObject: Boolean;
    procedure WriteIndentation;
    procedure WriteSeparator;
    procedure WriteNamePart(const Name: WideString);
    procedure RequireObject;
    procedure RequireArray;
    function IsEmpty: Boolean;
  public
    constructor Create(Stream: TStream; PrettyPrint: Boolean = True; UseTabChar: Boolean = False; TabSize: Integer = 2; Utf8: Boolean = True); overload;
    constructor Create(const FileName: string; PrettyPrint: Boolean = True; UseTabChar: Boolean = False; TabSize: Integer = 2); overload;
    destructor Destroy; override;

    procedure Check;

    procedure BeginArray; overload;
    procedure BeginObject; overload;

    procedure BeginArray(const Name: WideString); overload;
    procedure BeginObject(const Name: WideString); overload;

    procedure EndArray;
    procedure EndObject;

    procedure WriteNull; overload;
    procedure Write(Value: Boolean); overload;
    procedure Write(Value: Double); overload;
    procedure Write(const Value: WideString); overload;
    procedure Write(const Value: TDateTime; Offset: Integer); overload;
    procedure WriteNull(const Name: WideString); overload;
    procedure Write(const Name: WideString; Value: Boolean); overload;
    procedure Write(const Name: WideString; Value: Double); overload;
    procedure Write(const Name: WideString; const Value: WideString); overload;
    procedure Write(const Name: WideString; const Value: TDateTime; Offset: Integer); overload;
  end;

  TJsonItem = (itEof, itNull, itFalse, itTrue, itNumber, itString, itBeginArray, itEndArray, itBeginObject, itEndObject);

  TJsonReader = class(TJsonCustomParser)
  private
    FFirstItem: Boolean;
    FIsMember: Boolean;
    FItemPosition: Integer;
    FMemberName: WideString;
    FNesting: array of Boolean;
    FNestedLevel: Integer;
    FNumberValue: Double;
    FStringValue: WideString;
    FWasJsonItem: Boolean;
    procedure AddNesting(IsObject: Boolean);
    function GetColumn: Integer;
    function GetRow: Integer;
    function InArray: Boolean;
    function InObject: Boolean;
    function ParseLiteral: TJsonItem;
    function ReadValue: TJsonItem;
  public
    constructor Create(const Text: WideString); overload;
    constructor Create(const Data: TBytes; Utf8: Boolean); overload;
{$ifndef NEXTGEN}
    constructor Create(const Data: AnsiString; Utf8: Boolean); overload;
{$endif NEXTGEN}
    constructor Create(Stream: TStream; Utf8: Boolean); overload;
    constructor Create(const FileName: string; Utf8, FromFile: Boolean); overload;

    function Read: TJsonItem;

    property Column: Integer read GetColumn;
    property IsMember: Boolean read FIsMember;
    property ItemPosition: Integer read FItemPosition;
    property MemberName: WideString read FMemberName;
    property NumberValue: Double read FNumberValue;
    property Row: Integer read GetRow;
    property StringValue: WideString read FStringValue;
  end;

implementation

{$ifdef NEXTGEN}
uses System.Math, System.DateUtils {$ifdef TRIAL}, FMX.Dialogs {$endif TRIAL};
{$else}
  {$ifdef D104PLUS}
  uses System.Math, System.DateUtils {$ifdef TRIAL} {$ifdef MSWINDOWS}, Winapi.Windows {$else} {$ifndef LINUX}, FMX.Dialogs {$endif LINUX} {$endif MSWINDOWS} {$endif TRIAL};
  {$else}
  uses Math {$ifdef D6PLUS}, DateUtils {$else}, Utf8 {$endif D6PLUS} {$ifdef TRIAL} {$ifdef MSWINDOWS}, Windows {$else} {$ifndef LINUX}, FMX.Dialogs {$endif LINUX} {$endif MSWINDOWS} {$endif TRIAL};
  {$endif D104PLUS}
{$endif NEXTGEN}

{$ifdef TRIAL}
procedure ShowTrial; forward;
{$endif TRIAL}

const
  CR = #$d;
  LF = #$a;
  Tab = #9;
  NewLine = CR + LF;

var
  JsonNull: TJsonNull;
  JsonFalse: TJsonFalse;
  JsonTrue: TJsonTrue;

procedure Check(Value: Boolean; const Message: string);
begin
  if not Value then
    raise EJsonError.Create(Message);
end;

{$ifdef NEXTGEN}
function StreamToBytes(Stream: TStream): TBytes;
begin
  SetLength(Result, Stream.Size - Stream.Position);
  if Result <> nil then
    Stream.ReadBuffer(Result[0], Length(Result));
end;
{$else}
function BytesToAnsiString(const Value: TBytes): AnsiString;
begin
  SetLength(Result, Length(Value));
  if Value <> nil then
    Move(Value[0], Result[1], Length(Result));
end;

function StreamToAnsiString(Stream: TStream): AnsiString;
begin
  SetLength(Result, Stream.Size - Stream.Position);
  if Result <> '' then
    Stream.ReadBuffer(Result[1], Length(Result));
end;
{$endif NEXTGEN}

{$ifdef NEXTGEN}
procedure Write(Stream: TStream; Value: string; Utf8: Boolean); overload;
var Utf8Value: TBytes;
begin
  if Value <> '' then
    if Utf8 then
    begin
      Utf8Value := TEncoding.UTF8.GetBytes(value);
      if Length(Utf8Value) > 0 then
        Stream.WriteBuffer(Utf8Value, Length(Utf8Value));
    end
    else
      Stream.WriteBuffer(Value[0], Length(Value) * SizeOf(Char));
end;
{$else}
procedure Write(Stream: TStream; Value: WideString; Utf8: Boolean); overload;
var Utf8Value: UTF8String;
begin
  if Value <> '' then
    if Utf8 then
    begin
      Utf8Value := {$ifdef D6PLUS} UTF8Encode {$else} ToUtf8Ansi {$endif}(Value);
      if Utf8Value <> '' then
        Stream.WriteBuffer(Utf8Value[1], Length(Utf8Value));
    end
    else
      Stream.WriteBuffer(Value[1], Length(Value) * SizeOf(WideChar));
end;
{$endif NEXTGEN}

{$ifdef NEXTGEN}
procedure Write(Stream: TStream; Value: Char; Utf8: Boolean); overload;
begin
  if Utf8 then
    Write(Stream, string(Value), Utf8)
  else
    Stream.WriteBuffer(Value, SizeOf(Char));
end;
{$else}
procedure Write(Stream: TStream; Value: WideChar; Utf8: Boolean); overload;
var AnsiValue: AnsiChar;
begin
  if Utf8 then
  begin
    if Ord(Value) < $80 then
    begin
      AnsiValue := AnsiChar(Value); // required by Lazarus
      Stream.WriteBuffer(AnsiValue, SizeOf(AnsiChar))
    end
    else
      Write(Stream, WideString(Value), Utf8);
  end
  else
    Stream.WriteBuffer(Value, SizeOf(WideChar));
end;
{$endif NEXTGEN}

function ToJson(Value: WideChar): WideString;
begin
  case Value of
    '"': Result := '\"';
    '\': Result := '\\';
    #$8: Result := '\b';
    #$c: Result := '\f';
    LF: Result := '\n';
    CR: Result := '\r';
    Tab: Result := '\t';
    else
      if Value >= #$20 then
        Result := Value
      else
        Result := '\u' + IntToHex(Ord(Value), 4);
  end;
end;

procedure WriteJson(StringBuilder: TStringBuilder; const Text: WideString); overload;
var I: Integer;
begin
  StringBuilder.Append(WideChar('"'));
{$ifdef NEXTGEN}
  for I := Low(Text) to High(Text) do
{$else}
  for I := 1 to Length(Text) do
{$endif NEXTGEN}
    StringBuilder.Append(ToJson(Text[I]));
  StringBuilder.Append(WideChar('"'));
end;

procedure WriteJson(Stream: TStream; const Text: WideString; Utf8: Boolean); overload;
var I: Integer;
begin
  Write(Stream, WideChar('"'), Utf8);
{$ifdef NEXTGEN}
  for I := Low(Text) to High(Text) do
{$else}
  for I := 1 to Length(Text) do
{$endif NEXTGEN}
    Write(Stream, ToJson(Text[I]), Utf8);
  Write(Stream, WideChar('"'), Utf8);
end;

procedure WriteChars(StringBuilder: TStringBuilder; Value: WideChar; Count: Integer); overload;
var I: Integer;
begin
  for I := 1 to Count do
    StringBuilder.Append(Value);
end;

procedure WriteChars(Stream: TStream; Value: WideChar; Count: Integer; Utf8: Boolean); overload;
var I: Integer;
begin
  for I := 1 to Count do
    Write(Stream, Value, Utf8);
end;

function IsWhiteSpace(Value: WideChar): Boolean;
begin
  Result := (Value = ' ') or (Value = Tab) or (Value = LF) or (Value = CR);
end;

function IsStructuralCharacter(Value: WideChar): Boolean;
begin
  Result := (Value = '[') or (Value = '{') or (Value = ']') or (Value = '}') or (Value = ':') or (Value = ',');
end;

function IsDigit(Value: WideChar): Boolean;
begin
  Result := (Value >= '0') and (Value <= '9');
end;

function ToDigit(Value: WideChar): Integer;
begin
  Result := Ord(Value) - Ord('0');
end;

function NumberToString(Value: Double): WideString;
var OldDecimalSeparator, OldThousandSeparator: Char;
begin
{$ifdef DXEPLUS}
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  OldThousandSeparator := FormatSettings.ThousandSeparator;

  FormatSettings.DecimalSeparator := '.'; // decimal point is '.'
  FormatSettings.ThousandSeparator := #0; // no thousand separator
{$else}
  OldDecimalSeparator := DecimalSeparator;
  OldThousandSeparator := ThousandSeparator;

  DecimalSeparator := '.'; // decimal point is '.'
  ThousandSeparator := #0; // no thousand separator
{$endif DXEPLUS}

  try
    Result := FloatToStr(Value);
  finally
{$ifdef DXEPLUS}
    FormatSettings.ThousandSeparator := OldThousandSeparator;
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
{$else}
    ThousandSeparator := OldThousandSeparator;
    DecimalSeparator := OldDecimalSeparator;
{$endif DXEPLUS}
  end
end;

function ReadFile(const FileName: string): {$ifdef NEXTGEN} TBytes {$else} AnsiString {$endif NEXTGEN};
begin
{$ifdef NEXTGEN}
  Result := nil;
{$else}
  Result := '';
{$endif NEXTGEN}

  with TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone) do
  try
    if Size > 0 then
    begin
      SetLength(Result, Size);
  {$ifdef NEXTGEN}
      ReadBuffer(Result, Size);
  {$else}
      ReadBuffer(Result[1], Size);
  {$endif NEXTGEN}
    end;
  finally
    Free;
  end;
end;

procedure WriteFile(const FileName: string; const Content: {$ifdef NEXTGEN} TBytes {$else} AnsiString{$endif NEXTGEN});
begin
  with TFileStream.Create(FileName, fmCreate) do
  try
  {$ifdef NEXTGEN}
    if Content <> nil then
  {$else}
    if Content <> '' then
  {$endif NEXTGEN}
    begin
  {$ifdef NEXTGEN}
      WriteBuffer(Content, Length(Content));
  {$else}
      WriteBuffer(Content[1], Length(Content));
  {$endif NEXTGEN}
    end;
  finally
    Free;
  end;
end;

{$ifdef NEXTGEN}
function ToDateTime(const Text: string; var DateTime: TDateTime; var Offset: Integer): Boolean;
var
  Year, Month, Day, Hour, Minute, Second, MilliSecond, OffsetHour, OffsetMinute: Word;
  Index, Multiplier: Integer;
begin
  Result := False;

  if not ((Length(Text) >= 20)
    and IsDigit(Text[0])
    and IsDigit(Text[1])
    and IsDigit(Text[2])
    and IsDigit(Text[3])
    and (Text[4] = '-')
    and IsDigit(Text[5])
    and IsDigit(Text[6])
    and (Text[7] = '-')
    and IsDigit(Text[8])
    and IsDigit(Text[9])
    and ((Text[10] = 'T') or (Text[10] = 't') or (Text[10] = ' '))
    and IsDigit(Text[11])
    and IsDigit(Text[12])
    and (Text[13] = ':')
    and IsDigit(Text[14])
    and IsDigit(Text[15])
    and (Text[16] = ':')
    and IsDigit(Text[17])
    and IsDigit(Text[18])) then
    Exit;

  Year := 1000 * ToDigit(Text[0]) + 100 * ToDigit(Text[1]) + 10 * ToDigit(Text[2]) + ToDigit(Text[3]);
  Month := 10 * ToDigit(Text[5]) + ToDigit(Text[6]);
  Day := 10 * ToDigit(Text[8]) + ToDigit(Text[9]);
  Hour := 10 * ToDigit(Text[11]) + ToDigit(Text[12]);
  Minute := 10 * ToDigit(Text[14]) + ToDigit(Text[15]);
  Second := 10 * ToDigit(Text[17]) + ToDigit(Text[18]);
  MilliSecond := 0;

  Index := 19;
  if Text[Index] = '.' then
  begin
    Inc(Index);
    Multiplier := 100;
    while (Length(Text) >= Index) and IsDigit(Text[Index]) do
    begin
      Inc(MilliSecond, Multiplier * ToDigit(Text[Index]));
      Multiplier := Multiplier div 10;
      Inc(Index);
    end;
    if Multiplier = 100 then
      Exit; // no digit after '.'
  end;

  if not TryEncodeDateTime(Year, Month, Day, Hour, Minute, Second, MilliSecond, DateTime) then
    Exit;

  Offset := 0;
  if Length(Text) = Index then
    Result := (Text[Index] = 'Z') or (Text[Index] = 'z')
  else
  begin
    if Length(Text) <> Index + 5 then
      Exit;

    if not (((Text[Index] = '+') or (Text[Index] = '-'))
      and IsDigit(Text[Index + 1])
      and IsDigit(Text[Index + 2])
      and (Text[Index + 3] = ':')
      and IsDigit(Text[Index + 4])
      and IsDigit(Text[Index + 5])) then
      Exit;

    OffsetHour := 10 * ToDigit(Text[Index + 1]) + ToDigit(Text[Index + 2]);
    OffsetMinute := 10 * ToDigit(Text[Index + 4]) + ToDigit(Text[Index + 5]);
    Result := (OffsetHour < 24) and (OffsetMinute < 60);
    if Result then
    begin
      Offset := 60 * OffsetHour + OffsetMinute;
      if Text[Index] = '-' then
        Offset := -Offset;
    end;
  end;
end;
{$else}

function ToDateTime(const Text: WideString; var DateTime: TDateTime; var Offset: Integer): Boolean;
var
  Year, Month, Day, Hour, Minute, Second, MilliSecond, OffsetHour, OffsetMinute: Word;
  Index, Multiplier: Integer;
begin
  Result := False;

  if not ((Length(Text) >= 20)
    and IsDigit(Text[1])
    and IsDigit(Text[2])
    and IsDigit(Text[3])
    and IsDigit(Text[4])
    and (Text[5] = '-')
    and IsDigit(Text[6])
    and IsDigit(Text[7])
    and (Text[8] = '-')
    and IsDigit(Text[9])
    and IsDigit(Text[10])
    and ((Text[11] = 'T') or (Text[11] = 't') or (Text[11] = ' '))
    and IsDigit(Text[12])
    and IsDigit(Text[13])
    and (Text[14] = ':')
    and IsDigit(Text[15])
    and IsDigit(Text[16])
    and (Text[17] = ':')
    and IsDigit(Text[18])
    and IsDigit(Text[19])) then
    Exit;

  Year := 1000 * ToDigit(Text[1]) + 100 * ToDigit(Text[2]) + 10 * ToDigit(Text[3]) + ToDigit(Text[4]);
  Month := 10 * ToDigit(Text[6]) + ToDigit(Text[7]);
  Day := 10 * ToDigit(Text[9]) + ToDigit(Text[10]);
  Hour := 10 * ToDigit(Text[12]) + ToDigit(Text[13]);
  Minute := 10 * ToDigit(Text[15]) + ToDigit(Text[16]);
  Second := 10 * ToDigit(Text[18]) + ToDigit(Text[19]);
  MilliSecond := 0;

  Index := 20;
  if Text[Index] = '.' then
  begin
    Inc(Index);
    Multiplier := 100;
    while (Length(Text) >= Index) and IsDigit(Text[Index]) do
    begin
      Inc(MilliSecond, Multiplier * ToDigit(Text[Index]));
      Multiplier := Multiplier div 10;
      Inc(Index);
    end;
    if Multiplier = 100 then
      Exit; // no digit after '.'
  end;

{$ifdef D6PLUS}
  if not TryEncodeDateTime(Year, Month, Day, Hour, Minute, Second, MilliSecond, DateTime) then
    Exit;
{$else}
  try
    DateTime := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, MilliSecond);
  except
    on E: Exception do
      Exit;
  end;    
{$endif D6PLUS}

  Offset := 0;
  if Length(Text) = Index then
    Result := (Text[Index] = 'Z') or (Text[Index] = 'z')
  else
  begin
    if Length(Text) <> Index + 5 then
      Exit;

    if not (((Text[Index] = '+') or (Text[Index] = '-'))
      and IsDigit(Text[Index + 1])
      and IsDigit(Text[Index + 2])
      and (Text[Index + 3] = ':')
      and IsDigit(Text[Index + 4])
      and IsDigit(Text[Index + 5])) then
      Exit;

    OffsetHour := 10 * ToDigit(Text[Index + 1]) + ToDigit(Text[Index + 2]);
    OffsetMinute := 10 * ToDigit(Text[Index + 4]) + ToDigit(Text[Index + 5]);
    Result := (OffsetHour < 24) and (OffsetMinute < 60);
    if Result then
    begin
      Offset := 60 * OffsetHour + OffsetMinute;
      if Text[Index] = '-' then
        Offset := -Offset;
    end;
  end;
end;

{$endif NEXTGEN}

function ToString(const DateTime: TDateTime; Offset: Integer): WideString;
var
  Year, Month, Day, Hour, Minute, Second, MilliSecond: Word;
  Signum, Fraction: string;
begin
{$ifdef D6PLUS}
  DecodeDateTime(DateTime, Year, Month, Day, Hour, Minute, Second, MilliSecond);
{$else}
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Minute, Second, MilliSecond);
{$endif D6PLUS}
  if Offset < 0 then
    Signum := '-'
  else
    Signum := '+';
  Offset := Abs(Offset);
  if MilliSecond > 0 then
    Fraction := Format('.%.3d', [MilliSecond])
  else
    Fraction := '';
  Result := Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d%s%s%.2d:%.2d', [Year, Month, Day, Hour, Minute, Second, Fraction, Signum, ((Offset div 60) mod 24), Offset mod 60]);
end;

// TJsonCustomParser

constructor TJsonCustomParser.Create;
begin
  inherited Create;

  {$ifdef TRIAL} ShowTrial; {$endif TRIAL}

  FStringBuilder := TStringBuilder.Create($100);
end;

destructor TJsonCustomParser.Destroy;
begin
  FreeAndNil(FStringBuilder);
  inherited;
end;

function TJsonCustomParser.IsEnd: Boolean;
begin
{$ifdef NEXTGEN}
  Result := FPosition >= Length(FText);
{$else}
  Result := FPosition > Length(FText);
{$endif NEXTGEN}
end;

function TJsonCustomParser.Current: WideChar;
begin
  Result := FText[FPosition];
end;

procedure TJsonCustomParser.Next;
begin
  Inc(FPosition);
end;

function TJsonCustomParser.IsWhiteSpace: Boolean;
begin
  Result := WinJson.IsWhiteSpace(Current);
end;

procedure TJsonCustomParser.SkipWhiteSpace;
begin
  while not IsEnd and IsWhiteSpace do
    Next;
end;

function TJsonCustomParser.IsStructuralCharacter: Boolean;
begin
  Result := WinJson.IsStructuralCharacter(Current);
end;

function TJsonCustomParser.IsDigit: Boolean;
begin
  Result := WinJson.IsDigit(Current);
end;

procedure TJsonCustomParser.PositionToRowColumn(Position: Integer; var Row, Column: Integer);
const
  TabSize = 8;
var
  I: Integer;
begin
  // optimization
  Row := FLastRow;
  Column := FLastColumn;
  if Position = FLastPosition then
    Exit;

  if FLastPosition = -1 then
  begin
    {$ifdef NEXTGEN}
    I := 0;
    {$else}
    I := 1;
    {$endif NEXTGEN}

    Row := 1;
    Column := 1;
  end
  else
    I := FLastPosition;

  FLastPosition := Position;

{$ifdef NEXTGEN}
  if Position >= Length(FText) then
    Position := Length(FText);
{$else}
  if Position > Length(FText) then
    Position := Length(FText) + 1;
{$endif NEXTGEN}
  while I < Position do
  begin
    case FText[I] of
      CR:
        begin
          Inc(Row);
          Column := 1;
        end;

      LF:
{$ifdef NEXTGEN}
        if (I > 0) and (FText[I - 1] <> CR) then
{$else}
        if (I > 1) and (FText[I - 1] <> CR) then
{$endif NEXTGEN}
        begin
          Inc(Row);
          Column := 1;
        end;

      Tab:
       Column := Column + (TabSize - (Column - 1) mod TabSize);

      else
        Inc(Column);
    end;
    Inc(I);
  end;

  FLastRow := Row;
  FLastColumn := Column;
end;

procedure TJsonCustomParser.Error(Position: Integer; const ErrorMessage: string);
var Row, Column: Integer;
begin
  PositionToRowColumn(Position, Row, Column);
  raise EJsonError.Create('Error at ' + IntToStr(Row) + ', ' + IntToStr(Column) + ': ' + ErrorMessage);
end;

procedure TJsonCustomParser.Read(C: WideChar);
begin
  if IsEnd or (Current <> C) then
    Error(FPosition, WideString('''') + C + ''' expected');
  Next;
end;

function TJsonCustomParser.ReadDigit: Integer;
begin
  Result := ToDigit(Current);
  Next;
end;

function TJsonCustomParser.ReadHexaDigit: Integer;
begin
  Result := 0; // to avoid warning
  if IsEnd then
    Error(FPosition, 'Hexadecimal digit expected')
  else if (Current >= '0') and (Current <= '9') then
    Result := Ord(Current) - Ord('0')
  else if (Current >= 'A') and (Current <= 'F') then
    Result := Ord(Current) - Ord('A') + 10
  else if (Current >= 'a') and (Current <= 'f') then
    Result := Ord(Current) - Ord('a') + 10
  else
    Error(FPosition, 'Hexadecimal digit expected');
  Next;
end;

function TJsonCustomParser.ParseNumber: Double;
var
  Minus, ExpMinus: Boolean;
  Frac: Double;
  Exp: Integer;
begin
  Result := 0.0;
  if Current = '-' then
  begin
    Minus := True;
    Next;
  end
  else
    Minus := False;

  if IsEnd then
    Error(FPosition, 'Number expected');

  if (Current >= '1') and (Current <= '9') then
  begin
    Result := ReadDigit;
    while not IsEnd and IsDigit do
      Result := 10.0 * Result + ReadDigit;
  end
  else if Current = '0' then
    Next
  else
    Error(FPosition, 'Number expected');

  if not IsEnd and (Current = '.') then
  begin
    Next; // skip '.'

    if IsEnd or not IsDigit then
      Error(FPosition, 'Digit expected');

    Frac := 0.1;
    while not IsEnd and IsDigit do
    begin
      Result := Result + Frac * ReadDigit;
      Frac := 0.1 * Frac;
    end;
  end;

  if not IsEnd and ((Current = 'e') or (Current = 'E')) then
  begin
    Next; // skip 'e' / 'E'
    if not IsEnd and ((Current = '+') or (Current = '-')) then
    begin
      ExpMinus := Current = '-';
      Next; // skip '+' / '-'
    end
    else
      ExpMinus := False;

    if IsEnd or not IsDigit then
      Error(FPosition, 'Digit expected');

    Exp := 0;
    while not IsEnd and IsDigit do
      Exp := 10 * Exp + ReadDigit;

    if ExpMinus then
      Exp := -Exp;

    Result := Result * IntPower(10, Exp);
  end;

  if Minus then
    Result := -Result;
end;

function TJsonCustomParser.ParseString: WideString;
var
  Character: WideChar;
  Value, I: Integer;
begin
  if IsEnd or (Current <> '"') then
    Error(FPosition, 'String expected');
  Next; // skip '"'

  FStringBuilder.Clear;
  while not IsEnd and (Current <> '"') do
  begin
    Character := Current;
    if Character = '\' then
    begin
      Next;
      if IsEnd then
        Break;

      case Current of
        '"', '\', '/': Character := Current;
        'b': Character := #$8;
        'f': Character := #$c;
        'n': Character := LF;
        'r': Character := CR;
        't': Character := Tab;
        'u': begin
               Next;
               Value := 0;
               for I := 1 to 4 do
                 Value := Value * 16 + ReadHexaDigit;
               Character := WideChar(Value);
               Dec(FPosition);
             end;
        else Error(FPosition - 1, 'Incorrect escape sequence');
      end;
    end;

    FStringBuilder.Append(Character);
    Next;
  end;

  if IsEnd then
    Error(FPosition, 'Unterminated string');
  Next; // skip '"'

{$ifdef NEXTGEN}
  Result := FStringBuilder.ToString;
{$else}
  Result := FStringBuilder.Text;
{$endif NEXTGEN}
end;

function TJsonCustomParser.Parse(const Text: WideString): TJson;
begin
  FText := Text;
  FLastPosition := -1;
{$ifdef NEXTGEN}
  FPosition := 0;
{$else}
  FPosition := 1;
{$endif NEXTGEN}
  Result := nil;
end;

function TJsonCustomParser.ParseUtf8(const Utf8: TBytes): TJson;
{$ifdef NEXTGEN}
var Data: TBytes;
{$endif NEXTGEN}
begin
{$ifdef NEXTGEN}
  if (Length(Utf8) >= 3) and (Ord(Utf8[0]) = $EF) and (Ord(Utf8[1]) = $BB) and (Ord(Utf8[2]) = $BF) then
    Data := Copy(Utf8, 3, MaxInt) // delete UTF8 BOM
  else
    Data := Utf8;

  Result := Parse(TEncoding.UTF8.GetString(Data));
{$else}
  Result := ParseUtf8(BytesToAnsiString(Utf8));
{$endif NEXTGEN}
end;

function TJsonCustomParser.ParseAnsi(const Ansi: TBytes): TJson;
begin
{$ifdef NEXTGEN}
  Result := Parse(TEncoding.Ansi.GetString(Ansi));
{$else}
  Result := ParseAnsi(BytesToAnsiString(Ansi));
{$endif NEXTGEN}
end;

{$ifndef NEXTGEN}
function TJsonCustomParser.ParseUtf8(const Utf8: AnsiString): TJson;
var
  Data: AnsiString;
  Text: WideString;
begin
  if (Length(Utf8) >= 3) and (Ord(Utf8[1]) = $EF) and (Ord(Utf8[2]) = $BB) and (Ord(Utf8[3]) = $BF) then
    Data := Copy(Utf8, 4, MaxInt) // delete UTF8 BOM
  else
    Data := Utf8;

  Text := {$ifdef D2009PLUS}UTF8ToString{$else} {$ifdef D6PLUS} UTF8Decode {$else} ToAnsiString {$endif D6PLUS} {$endif D2009PLUS}(Data);
  if (Data <> '') and (Text = '') then
    raise EJsonError.Create('Incorrect UTF-8 encoding');
  Result := Parse(Text);
end;

function TJsonCustomParser.ParseAnsi(const Ansi: AnsiString): TJson;
begin
  Result := Parse(WideString(Ansi));
end;
{$endif NEXTGEN}

function TJsonCustomParser.ParseUtf8(Stream: TStream): TJson;
begin
{$ifdef NEXTGEN}
  Result := ParseUtf8(StreamToBytes(Stream));
{$else}
  Result := ParseUtf8(StreamToAnsiString(Stream));
{$endif NEXTGEN}
end;

function TJsonCustomParser.ParseAnsi(Stream: TStream): TJson;
begin
{$ifdef NEXTGEN}
  Result := ParseAnsi(StreamToBytes(Stream));
{$else}
  Result := ParseAnsi(StreamToAnsiString(Stream));
{$endif NEXTGEN}
end;

function TJsonCustomParser.ParseUtf8File(const FileName: string): TJson;
begin
  Result := ParseUTF8(ReadFile(FileName));
end;

function TJsonCustomParser.ParseAnsiFile(const FileName: string): TJson;
begin
  Result := ParseAnsi(ReadFile(FileName));
end;

// TJsonParser

constructor TJsonParser.Create;
begin
  inherited Create;
end;

function TJsonParser.ParseLiteral: TJson;
var
  Start: Integer;
  Value: WideString;
begin
  Result := nil; // to avoid warning
  Start := FPosition;
  while not IsEnd and not IsWhiteSpace and not IsStructuralCharacter and (Current <> '-') and (Current <> '"') do
    Next;

{$ifdef NEXTGEN}
  Value := FText.Substring(Start, FPosition - Start);
{$else}
  Value := Copy(FText, Start, FPosition - Start);
{$endif NEXTGEN}
  if Value = 'null' then
    Result := JsonNull
  else if Value = 'false' then
    Result := JsonFalse
  else if Value = 'true' then
    Result := JsonTrue
  else
    Error(Start, 'Incorrect value ' + Value);
end;

function TJsonParser.ParseArray: TJsonArray;
var Count: Integer;
begin
  Result := TJsonArray.Create(16);
  try
    Count := 0;
    Next; // skip '['
    SkipWhiteSpace;
    if not IsEnd and (Current <> ']') then
      while True do
      begin
        if Result.ElementCount = Count then
          Result.ElementCount := 2 * Result.ElementCount;
        Result.FElements[Count] := ParseValue;
        Inc(Count);
        SkipWhiteSpace;
        if IsEnd or (Current <> ',') then
          Break;
        Next; // skip ','
        SkipWhiteSpace;
      end;
    Read(']');
    Result.ElementCount := Count;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TJsonParser.ParseObject: TJsonObject;
var
  Name: WideString;
  Value: TJson;
begin
  Result := TJsonObject.Create;
  try
    Next; // skip '{'
    SkipWhiteSpace;
    if not IsEnd and (Current <> '}') then
      while True do
      begin
        Name := ParseString;
        SkipWhiteSpace;
        Read(':');
        SkipWhiteSpace;
        Value := ParseValue;
        Result.SetRawMember(-1, Name, Value);
        SkipWhiteSpace;
        if IsEnd or (Current <> ',') then
          Break;
        Next; // skip ','
        SkipWhiteSpace;
      end;
    Read('}');
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TJsonParser.ParseValue: TJson;
begin
  SkipWhiteSpace;
  if IsEnd then
    Error(FPosition, 'No JSON value found');

  case Current of
    '{': Result := ParseObject;
    '[': Result := ParseArray;
    '"': Result := TJsonString.Create(ParseString);
    '-', '0'..'9': Result := TJsonNumber.Create(ParseNumber);
    else
      Result := ParseLiteral
  end;
end;

function TJsonParser.Parse(const Text: WideString): TJson;
begin
  inherited Parse(Text);
  Result := ParseValue;
  SkipWhiteSpace;
  if not IsEnd then
    Error(FPosition, 'Unexpected character');
end;

// TStringBuilder

{$ifndef NEXTGEN}
constructor TStringBuilder.Create(Capacity: Integer);
begin
  inherited Create;
  Self.Capacity := Capacity;
end;

procedure TStringBuilder.Clear;
begin
  FLength := 0;
end;

function TStringBuilder.GetCapacity: Integer;
begin
  Result := Length(FText);
end;

procedure TStringBuilder.SetCapacity(Value: Integer);
begin
  SetLength(FText, Value);
  if FLength > Value then
    FLength := Value;
end;

procedure TStringBuilder.EnsureCapacity(Value: Integer);
begin
  if Value > Capacity then
    if Value > 2 * Capacity then
      Capacity := 2 * Value
    else
      Capacity := 2 * Capacity
end;

procedure TStringBuilder.Append(Value: WideChar);
begin
  Inc(FLength);
  EnsureCapacity(FLength);
  FText[FLength] := Value;
end;

procedure TStringBuilder.Append(const Value: WideString);
begin
  if Value <> '' then
  begin
    EnsureCapacity(FLength + Length(Value));
    Move(Value[1], FText[FLength + 1], Length(Value) * SizeOf(WideChar));
    Inc(FLength, Length(Value));
  end  
end;

function TStringBuilder.GetText: WideString;
begin
  Result := '';
  if FLength > 0 then
  begin
    SetLength(Result, FLength);
    Move(FText[1], Result[1], FLength * SizeOf(WideChar));
  end;
end;
{$endif NEXTGEN}

// TJson

function TJson.ToString(PrettyPrint, UseTabChar: Boolean; TabSize: Integer): WideString;
var
  StringBuilder: TStringBuilder;
  TabChar: WideChar;
begin
  StringBuilder := TStringBuilder.Create($10000);
  try
    if PrettyPrint then
    begin
      if UseTabChar then
        TabChar := #$9
      else
        TabChar := ' ';
      Write(StringBuilder, TabChar, TabSize, 0);
    end
    else
      Write(StringBuilder);
{$ifdef NEXTGEN}
    Result := StringBuilder.ToString;
{$else}
    Result := StringBuilder.Text;
{$endif NEXTGEN}
  finally
    FreeAndNil(StringBuilder);
  end;
end;

{$ifdef NEXTGEN}
function TJson.ToUtf8(PrettyPrint, UseTabChar: Boolean; TabSize: Integer): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(ToString(PrettyPrint, UseTabChar, TabSize));
end;
{$else}
function TJson.ToUtf8(PrettyPrint, UseTabChar: Boolean; TabSize: Integer): AnsiString;
begin
  Result := {$ifdef D6PLUS} UTF8Encode {$else} ToUtf8Ansi {$endif D6PLUS} (ToString(PrettyPrint, UseTabChar, TabSize));
end;
{$endif NEXTGEN}

procedure TJson.ToUtf8File(const FileName: string; PrettyPrint, UseTabChar: Boolean; TabSize: Integer);
begin
  WriteFile(FileName, ToUtf8(PrettyPrint, UseTabChar, TabSize));
end;

procedure TJson.Write(StringBuilder: TStringBuilder; TabChar: WideChar; TabSize, Indentation: Integer);
begin
  Write(StringBuilder);
end;

function TJson.GetAt(Index: Integer): TJson;
begin
  raise EJsonError.Create('JSON array required');
end;

procedure TJson.SetAt(Index: Integer; Value: TJson);
begin
  raise EJsonError.Create('JSON array required');
end;

function TJson.GetItem(const Name: WideString): TJson;
begin
  raise EJsonError.Create('JSON object required');
end;

procedure TJson.SetItem(const Name: WideString; Value: TJson);
begin
  raise EJsonError.Create('JSON object required');
end;

function TJson.AsBoolean: Boolean;
begin
  raise EJsonError.Create('JSON boolean required');
end;

function TJson.AsDateTime: TDateTime;
begin
  raise EJsonError.Create('JSON date/time required');
end;

function TJson.AsDateTimeOffset: Integer;
begin
  raise EJsonError.Create('JSON date/time required');
end;

function TJson.AsNumber: Double;
begin
  raise EJsonError.Create('JSON number required');
end;

function TJson.AsString: WideString;
begin
  raise EJsonError.Create('JSON string required');
end;

function TJson.IsArray: Boolean;
begin
  Result := False;
end;

function TJson.IsBoolean: Boolean;
begin
  Result := False;
end;

function TJson.IsDateTime: Boolean;
begin
  Result := False;
end;

function TJson.IsLiteral: Boolean;
begin
  Result := False;
end;

function TJson.IsNull: Boolean;
begin
  Result := False;
end;

function TJson.IsNumber: Boolean;
begin
  Result := False;
end;

function TJson.IsObject: Boolean;
begin
  Result := False;
end;

function TJson.IsString: Boolean;
begin
  Result := False;
end;

function TJson.GetNullAt(Index: Integer): TJsonNull;
begin
  raise EJsonError.Create('JSON array required');
end;

function TJson.GetFalseAt(Index: Integer): TJsonFalse;
begin
  raise EJsonError.Create('JSON array required');
end;

function TJson.GetTrueAt(Index: Integer): TJsonTrue;
begin
  raise EJsonError.Create('JSON array required');
end;

function TJson.GetObjectAt(Index: Integer): TJsonObject;
begin
  raise EJsonError.Create('JSON array required');
end;

function TJson.GetStringAt(Index: Integer): TJsonString;
begin
  raise EJsonError.Create('JSON array required');
end;

function TJson.GetArrayAt(Index: Integer): TJsonArray;
begin
  raise EJsonError.Create('JSON array required');
end;

function TJson.GetNumberAt(Index: Integer): TJsonNumber;
begin
  raise EJsonError.Create('JSON array required');
end;

function TJson.GetNullItem(const Name: WideString): TJsonNull;
begin
  raise EJsonError.Create('JSON object required');
end;

function TJson.GetFalseItem(const Name: WideString): TJsonFalse;
begin
  raise EJsonError.Create('JSON object required');
end;

function TJson.GetTrueItem(const Name: WideString): TJsonTrue;
begin
  raise EJsonError.Create('JSON object required');
end;

function TJson.GetObjectItem(const Name: WideString): TJsonObject;
begin
  raise EJsonError.Create('JSON object required');
end;

function TJson.GetStringItem(const Name: WideString): TJsonString;
begin
  raise EJsonError.Create('JSON object required');
end;

function TJson.GetArrayItem(const Name: WideString): TJsonArray;
begin
  raise EJsonError.Create('JSON object required');
end;

function TJson.GetNumberItem(const Name: WideString): TJsonNumber;
begin
  raise EJsonError.Create('JSON object required');
end;

// TJsonLiteral

function TJsonLiteral.IsLiteral: Boolean;
begin
  Result := True;
end;

procedure TJsonLiteral.FreeInstance;
begin
  if FFreeInstance then
    inherited;
end;

// TJsonNull

function TJsonNull.Clone: TJson;
begin
  Result := JsonNull;
end;

function TJsonNull.IsNull: Boolean;
begin
  Result := True;
end;

procedure TJsonNull.Write(StringBuilder: TStringBuilder);
begin
  StringBuilder.Append('null');
end;

// TJsonFalse

function TJsonFalse.AsBoolean: Boolean;
begin
  Result := False;
end;

function TJsonFalse.Clone: TJson;
begin
  Result := JsonFalse;
end;

function TJsonFalse.IsBoolean: Boolean;
begin
  Result := True;
end;

procedure TJsonFalse.Write(StringBuilder: TStringBuilder);
begin
  StringBuilder.Append('false');
end;

// TJsonTrue

function TJsonTrue.AsBoolean: Boolean;
begin
  Result := True;
end;

function TJsonTrue.Clone: TJson;
begin
  Result := JsonTrue;
end;

function TJsonTrue.IsBoolean: Boolean;
begin
  Result := True;
end;

procedure TJsonTrue.Write(StringBuilder: TStringBuilder);
begin
  StringBuilder.Append('true');
end;

// TJsonNumber

constructor TJsonNumber.Create(Value: Double);
begin
  inherited Create;
  FValue := Value;
end;

function TJsonNumber.AsNumber: Double;
begin
  Result := Value;
end;

function TJsonNumber.Clone: TJson;
begin
  Result := TJsonNumber.Create(FValue);
end;

function TJsonNumber.IsNumber: Boolean;
begin
  Result := True;
end;

procedure TJsonNumber.Write(StringBuilder: TStringBuilder);
begin
  StringBuilder.Append(NumberToString(FValue));
end;

// TJsonString

function TJsonString.Clone: TJson;
begin
  Result := TJsonString.Create(FValue);
end;

constructor TJsonString.Create(const Value: WideString);
begin
  inherited Create;
  FValue := Value;
end;

procedure TJsonString.Write(StringBuilder: TStringBuilder);
begin
  WriteJson(StringBuilder, FValue);
end;

function TJsonString.AsString: WideString;
begin
  Result := Value;
end;

function TJsonString.AsDateTime: TDateTime;
begin
  Check(IsDateTime, 'JSON date/time required');
  Result := DateTime;
end;

function TJsonString.AsDateTimeOffset: Integer;
begin
  Check(IsDateTime, 'JSON date/time required');
  Result := DateTimeOffset;
end;

function TJsonString.IsDateTime: Boolean;
var
  DateTime: TDateTime;
  Offset: Integer;
begin
  Result := ToDateTime(FValue, DateTime, Offset);
end;

function TJsonString.IsString: Boolean;
begin
  Result := True;
end;

function TJsonString.GetDateTime: TDateTime;
var Offset: Integer;
begin
  if not ToDateTime(FValue, Result, Offset) then
    Result := 0;
end;

function TJsonString.GetDateTimeOffset: Integer;
var DateTime: TDateTime;
begin
  if not ToDateTime(FValue, DateTime, Result) then
    Result := 0;
end;

// TJsonArray

constructor TJsonArray.Create(ElementCount: Integer);
begin
  inherited Create;
  SetElementCount(ElementCount);
end;

destructor TJsonArray.Destroy;
var I: Integer;
begin
  for I := ElementCount - 1 downto 0 do
    FreeAndNil(FElements[I]);
  inherited;
end;

function TJsonArray.GetElementCount: Integer;
begin
  Result := Length(FElements);
end;

procedure TJsonArray.SetElementCount(Value: Integer);
var I: Integer;
begin
  Check(Value >= 0, 'Value can''t be negative');
  for I := ElementCount - 1 downto Value do
    FreeAndNil(FElements[I]);
  SetLength(FElements, Value);
end;

procedure TJsonArray.CheckIndex(Index: Integer);
begin
  Check((Index >= 0) and (Index < ElementCount), 'Incorrect index ' + IntToStr(Index));
end;

procedure TJsonArray.SetRawElement(Index: Integer; Element: TJSon);
begin
  FElements[Index].Free;
  FElements[Index] := Element;
end;

function TJsonArray.GetElement(Index: Integer): TJson;
begin
  CheckIndex(Index);
  Result := FElements[Index];
  if Result = nil then
    Result := JsonNull;
end;

procedure TJsonArray.SetElement(Index: Integer; Element: TJSon);
begin
  SetJson(Index, Element);
end;

function TJsonArray.Clone: TJson;
var
  I: Integer;
  JsonArray: TJsonArray;
begin
  JsonArray := TJsonArray.Create(ElementCount);
  for I := 0 to ElementCount - 1 do
    JsonArray.FElements[I] := Elements[I].Clone;
  Result := JsonArray;
end;

procedure TJsonArray.Write(StringBuilder: TStringBuilder);
var I: Integer;
begin
  StringBuilder.Append(WideChar('['));
  for I := 0 to ElementCount - 1 do
  begin
    if I > 0 then
      StringBuilder.Append(WideChar(','));
    Elements[I].Write(StringBuilder);
  end;
  StringBuilder.Append(WideChar(']'));
end;

procedure TJsonArray.Write(StringBuilder: TStringBuilder; TabChar: WideChar; TabSize, Indentation: Integer);
var I: Integer;
begin
  StringBuilder.Append('[' + NewLine);
  for I := 0 to ElementCount - 1 do
  begin
    WriteChars(StringBuilder, TabChar, Indentation + TabSize);
    Elements[I].Write(StringBuilder, TabChar, TabSize, Indentation + TabSize);
    if I < ElementCount - 1 then
      StringBuilder.Append(WideChar(','));
    StringBuilder.Append(NewLine);
  end;
  WriteChars(StringBuilder, TabChar, Indentation);
  StringBuilder.Append(WideChar(']'));
end;

procedure TJsonArray.SetNull(Index: Integer);
begin
  CheckIndex(Index);
  SetRawElement(Index, JSonNull);
end;

procedure TJsonArray.SetFalse(Index: Integer);
begin
  CheckIndex(Index);
  SetRawElement(Index, JsonFalse);
end;

procedure TJsonArray.SetTrue(Index: Integer);
begin
  CheckIndex(Index);
  SetRawElement(Index, JsonTrue);
end;

procedure TJsonArray.SetNumber(Index: Integer; Value: Double);
begin
  CheckIndex(Index);
  SetRawElement(Index, TJsonNumber.Create(Value));
end;

procedure TJsonArray.SetString(Index: Integer; const Value: WideString);
begin
  CheckIndex(Index);
  SetRawElement(Index, TJsonString.Create(Value));
end;

procedure TJsonArray.SetDateTime(Index: Integer; const Value: TDateTime; Offset: Integer);
begin
  SetString(Index, WinJson.ToString(Value, Offset));
end;

function TJsonArray.SetArray(Index: Integer; ElementCount: Integer): TJsonArray;
begin
  CheckIndex(Index);
  Result := TJsonArray.Create(ElementCount);
  SetRawElement(Index, Result);
end;

function TJsonArray.SetObject(Index: Integer): TJsonObject;
begin
  CheckIndex(Index);
  Result := TJsonObject.Create;
  SetRawElement(Index, Result);
end;

function TJsonArray.SetJson(Index: Integer; Value: TJson): TJson;
begin
  CheckIndex(Index);
  if Value = nil then
    Result := nil
  else
    Result := Value.Clone;
  SetRawElement(Index, Result);
end;

function TJsonArray.IsArray: Boolean;
begin
  Result := True;
end;

function TJsonArray.GetAt(Index: Integer): TJson;
begin
  Result := Elements[Index];
end;

procedure TJsonArray.SetAt(Index: Integer; Value: TJson);
begin
  Elements[Index] := Value;
end;

function TJsonArray.GetNullAt(Index: Integer): TJsonNull;
var Element: TJson;
begin
  Element := At[Index];
  if not (Element is TJsonNull) then
    raise EJsonError.Create('Value ''null'' expected');
  Result := TJsonNull(Element);
end;

function TJsonArray.GetFalseAt(Index: Integer): TJsonFalse;
var Element: TJson;
begin
  Element := At[Index];
  if not (Element is TJsonFalse) then
    raise EJsonError.Create('Value ''false'' expected');
  Result := TJsonFalse(Element);
end;

function TJsonArray.GetTrueAt(Index: Integer): TJsonTrue;
var Element: TJson;
begin
  Element := At[Index];
  if not (Element is TJsonTrue) then
    raise EJsonError.Create('Value ''true'' expected');
  Result := TJsonTrue(Element);
end;

function TJsonArray.GetObjectAt(Index: Integer): TJsonObject;
var Element: TJson;
begin
  Element := At[Index];
  if not (Element is TJsonObject) then
    raise EJsonError.Create('JSON object expected');
  Result := TJsonObject(Element);
end;

function TJsonArray.GetStringAt(Index: Integer): TJsonString;
var Element: TJson;
begin
  Element := At[Index];
  if not (Element is TJsonString) then
    raise EJsonError.Create('JSON string expected');
  Result := TJsonString(Element);
end;

function TJsonArray.GetArrayAt(Index: Integer): TJsonArray;
var Element: TJson;
begin
  Element := At[Index];
  if not (Element is TJsonArray) then
    raise EJsonError.Create('JSON array expected');
  Result := TJsonArray(Element);
end;

function TJsonArray.GetNumberAt(Index: Integer): TJsonNumber;
var Element: TJson;
begin
  Element := At[Index];
  if not (Element is TJsonNumber) then
    raise EJsonError.Create('JSON number expected');
  Result := TJsonNumber(Element);
end;

procedure TJsonArray.Swap(Index1, Index2: Integer);
var Temp: TJson;
begin
  CheckIndex(Index1);
  CheckIndex(Index2);
  Temp := FElements[Index1];
  FElements[Index1] := FElements[Index2];
  FElements[Index2] := Temp;
end;

procedure TJsonArray.Delete(Index: Integer);
var I: Integer;
begin
  CheckIndex(Index);
  Elements[Index] := nil; // free item
  for I := Index + 1 to ElementCount - 1 do
    FElements[I - 1] := FElements[I]; // move items without free
  FElements[ElementCount - 1] := nil; // set last item without free
  ElementCount := ElementCount - 1;
end;

procedure TJsonArray.AppendNull;
begin
  ElementCount := ElementCount + 1;
  SetNull(ElementCount - 1);
end;

procedure TJsonArray.AppendFalse;
begin
  ElementCount := ElementCount + 1;
  SetFalse(ElementCount - 1);
end;

procedure TJsonArray.AppendTrue;
begin
  ElementCount := ElementCount + 1;
  SetTrue(ElementCount - 1);
end;

procedure TJsonArray.AppendNumber(Value: Double);
begin
  ElementCount := ElementCount + 1;
  SetNumber(ElementCount - 1, Value);
end;

procedure TJsonArray.AppendString(const Value: WideString);
begin
  ElementCount := ElementCount + 1;
  SetString(ElementCount - 1, Value);
end;

procedure TJsonArray.AppendDateTime(const Value: TDateTime; Offset: Integer);
begin
  ElementCount := ElementCount + 1;
  SetDateTime(ElementCount - 1, Value, Offset);
end;

function TJsonArray.AppendArray(ElementCount: Integer): TJsonArray;
begin
  Self.ElementCount := Self.ElementCount + 1;
  Result := SetArray(Self.ElementCount - 1, ElementCount);
end;

function TJsonArray.AppendObject: TJsonObject;
begin
  ElementCount := ElementCount + 1;
  Result := SetObject(ElementCount - 1);
end;

function TJsonArray.AppendJson(Value: TJSon): TJson;
begin
  ElementCount := ElementCount + 1;
  Result := SetJson(ElementCount - 1, Value);
end;

procedure TJsonArray.InsertNull(Index: Integer);
var I: Integer;
begin
  if Index = ElementCount then
    AppendNull
  else
  begin
    CheckIndex(Index);
    ElementCount := ElementCount + 1;
    for I := ElementCount - 1 downto Index + 1 do
      FElements[I] := FElements[I - 1]; // move items without free
    FElements[Index] := nil; // set item without free
  end;
end;

procedure TJsonArray.InsertFalse(Index: Integer);
begin
  InsertNull(Index);
  SetFalse(Index);
end;

procedure TJsonArray.InsertTrue(Index: Integer);
begin
  InsertNull(Index);
  SetTrue(Index);
end;

procedure TJsonArray.InsertNumber(Index: Integer; Value: Double);
begin
  InsertNull(Index);
  SetNumber(Index, Value);
end;

procedure TJsonArray.InsertString(Index: Integer; const Value: WideString);
begin
  InsertNull(Index);
  SetString(Index, Value);
end;

procedure TJsonArray.InsertDateTime(Index: Integer; const Value: TDateTime; Offset: Integer);
begin
  InsertNull(Index);
  SetDateTime(Index, Value, Offset);
end;

function TJsonArray.InsertArray(Index: Integer; ElementCount: Integer): TJsonArray;
begin
  InsertNull(Index);
  Result := SetArray(Index, ElementCount);
end;

function TJsonArray.InsertObject(Index: Integer): TJsonObject;
begin
  InsertNull(Index);
  Result := SetObject(Index);
end;

function TJsonArray.InsertJson(Index: Integer; Value: TJSon): TJson;
begin
  InsertNull(Index);
  Result := SetJson(Index, Value);
end;

// TJsonObject

constructor TJsonObject.Create;
begin
  inherited Create;
end;

destructor TJsonObject.Destroy;
begin
  DeleteAll;
  inherited;
end;

function TJsonObject.GetMemberCapacity: Integer;
begin
  Result := Length(FMembers);
end;

procedure TJsonObject.CheckIndex(Index: Integer);
begin
  Check((Index >= 0) and (Index < MemberCount), 'Incorrect index ' + IntToStr(Index));
end;

procedure TJsonObject.ClearMember(Index: Integer);
begin
  FreeAndNil(FMembers[Index].FValue);
  FMembers[Index].FName := '';
end;

procedure TJsonObject.SetRawMember(Index: Integer; const Name: WideString; Value: TJSon);
begin
  if Index = -1 then
  begin
    Index := FMemberCount;
    if FMemberCount = MemberCapacity then
    begin
      if MemberCapacity = 0 then
        SetLength(FMembers, 16) // initial capacity
      else
        SetLength(FMembers, 2 * MemberCapacity);
    end;
    Inc(FMemberCount);
  end
  else
  begin
    CheckIndex(Index);
    ClearMember(Index);
  end;

  with FMembers[Index] do
  begin
    FName := Name;
    FValue := Value;
  end;
end;

function TJsonObject.GetMemberName(Index: Integer): WideString;
begin
  CheckIndex(Index);
  Result := FMembers[Index].FName;
end;

procedure TJsonObject.SetMemberName(Index: Integer; const Value: WideString);
begin
  CheckIndex(Index);
  FMembers[Index].FName := Value;
end;

function TJsonObject.GetMemberValue(Index: Integer): TJson;
begin
  CheckIndex(Index);
  Result := FMembers[Index].FValue;
  if Result = nil then
    Result := JsonNull;
end;

procedure TJsonObject.SetMemberValue(Index: Integer; Value: TJson);
var OldValue: TJson;
begin
  CheckIndex(Index);
  OldValue := FMembers[Index].FValue;

  if Value = nil then
    FMembers[Index].FValue := nil
  else
    FMembers[Index].FValue := Value.Clone;

  OldValue.Free; // delete old value after Value.Clone line
end;

function TJsonObject.Clone: TJson;
var
  I: Integer;
  JsonObject: TJsonObject;
begin
  JsonObject := TJsonObject.Create;
  for I := 0 to MemberCount - 1 do
    JsonObject.SetRawMember(-1, FMembers[I].FName, MemberValue[I].Clone);
  Result := JsonObject;
end;

procedure TJsonObject.Write(StringBuilder: TStringBuilder);
var I: Integer;
begin
  StringBuilder.Append(WideChar('{'));
  for I := 0 to MemberCount - 1 do
  begin
    if I > 0 then
      StringBuilder.Append(WideChar(','));
    WriteJson(StringBuilder, FMembers[I].FName);
    StringBuilder.Append(WideChar(':'));
    MemberValue[I].Write(StringBuilder);
  end;
  StringBuilder.Append(WideChar('}'));
end;

procedure TJsonObject.Write(StringBuilder: TStringBuilder; TabChar: WideChar; TabSize, Indentation: Integer);
var I: Integer;
begin
  StringBuilder.Append('{' + NewLine);
  for I := 0 to MemberCount - 1 do
  begin
    WriteChars(StringBuilder, TabChar, Indentation + TabSize);
    WriteJson(StringBuilder, FMembers[I].FName);
    StringBuilder.Append(': ');
    MemberValue[I].Write(StringBuilder, TabChar, TabSize, Indentation + TabSize);
    if I < MemberCount - 1 then
      StringBuilder.Append(WideChar(','));
    StringBuilder.Append(NewLine);
  end;
  WriteChars(StringBuilder, TabChar, Indentation);
  StringBuilder.Append(WideChar('}'));
end;

function TJsonObject.Find(const Name: WideString): Integer;
var I: Integer;
begin
  for I := MemberCount - 1 downto 0 do
    if FMembers[I].FName = Name then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TJsonObject.GetMember(const Name: WideString): TJson;
var Index: Integer;
begin
  Index := Find(Name);
  if Index = -1 then
    Result := nil
  else
  begin
    Result := FMembers[Index].FValue;
    if Result = nil then
      Result := JsonNull;
  end;
end;

procedure TJsonObject.SetMember(const Name: WideString; Value: TJSon);
begin
  SetJson(Name, Value);
end;

procedure TJsonObject.DeleteAll;
var I: Integer;
begin
  for I := MemberCapacity - 1 downto 0 do
    ClearMember(I);
  FMembers := nil;
  FMemberCount := 0;
end;

procedure TJsonObject.Delete(Index: Integer);
begin
  CheckIndex(Index);
  ClearMember(Index);
  Dec(FMemberCount);
  if Index < FMemberCount then
  begin
    FMembers[Index] := FMembers[FMemberCount];
    FMembers[FMemberCount].FName := '';
    FMembers[FMemberCount].FValue := nil;
  end;

  if 2 * FMemberCount < MemberCapacity then
    SetLength(FMembers, FMemberCount);
end;

procedure TJsonObject.Delete(const Name: WideString);
var Index: Integer;
begin
  Index := Find(Name);
  if Index <> -1 then
    Delete(Index);
end;

procedure TJsonObject.SetNull(const Name: WideString);
begin
  SetRawMember(Find(Name), Name, JsonNull);
end;

procedure TJsonObject.SetFalse(const Name: WideString);
begin
  SetRawMember(Find(Name), Name, JsonFalse);
end;

procedure TJsonObject.SetTrue(const Name: WideString);
begin
  SetRawMember(Find(Name), Name, JsonTrue);
end;

procedure TJsonObject.SetNumber(const Name: WideString; Value: Double);
begin
  SetRawMember(Find(Name), Name, TJsonNumber.Create(Value));
end;

procedure TJsonObject.SetString(const Name, Value: WideString);
begin
  SetRawMember(Find(Name), Name, TJsonString.Create(Value));
end;

procedure TJsonObject.SetDateTime(const Name: WideString; const Value: TDateTime; Offset: Integer);
begin
  SetString(Name, WinJson.ToString(Value, Offset));
end;

function TJsonObject.SetArray(const Name: WideString; ElementCount: Integer): TJsonArray;
begin
  Result := TJsonArray.Create(ElementCount);
  SetRawMember(Find(Name), Name, Result);
end;

function TJsonObject.SetObject(const Name: WideString): TJsonObject;
begin
  Result := TJsonObject.Create;
  SetRawMember(Find(Name), Name, Result);
end;

function TJsonObject.SetJson(const Name: WideString; Value: TJson): TJson;
begin
  if Value = nil then
    Result := nil
  else
    Result := Value.Clone;
  SetRawMember(Find(Name), Name, Result);
end;

function TJsonObject.IsObject: Boolean;
begin
  Result := True;
end;

function TJsonObject.GetItem(const Name: WideString): TJson;
begin
  Result := Members[Name];
  Check(Result <> nil, 'Item ''' + Name + ''' not found');
end;

procedure TJsonObject.SetItem(const Name: WideString; Value: TJson);
begin
  Members[Name] := Value;
end;

function TJsonObject.GetNullItem(const Name: WideString): TJsonNull;
var Member: TJson;
begin
  Member := Items[Name];
  if not (Member is TJsonNull) then
    raise EJsonError.Create('Value ''null'' expected');
  Result := TJsonNull(Member);
end;

function TJsonObject.GetFalseItem(const Name: WideString): TJsonFalse;
var Member: TJson;
begin
  Member := Items[Name];
  if not (Member is TJsonFalse) then
    raise EJsonError.Create('Value ''false'' expected');
  Result := TJsonFalse(Member);
end;

function TJsonObject.GetTrueItem(const Name: WideString): TJsonTrue;
var Member: TJson;
begin
  Member := Items[Name];
  if not (Member is TJsonTrue) then
    raise EJsonError.Create('Value ''true'' expected');
  Result := TJsonTrue(Member);
end;

function TJsonObject.GetObjectItem(const Name: WideString): TJsonObject;
var Member: TJson;
begin
  Member := Items[Name];
  if not (Member is TJsonObject) then
    raise EJsonError.Create('JSON object expected');
  Result := TJsonObject(Member);
end;

function TJsonObject.GetStringItem(const Name: WideString): TJsonString;
var Member: TJson;
begin
  Member := Items[Name];
  if not (Member is TJsonString) then
    raise EJsonError.Create('JSON string expected');
  Result := TJsonString(Member);
end;

function TJsonObject.GetArrayItem(const Name: WideString): TJsonArray;
var Member: TJson;
begin
  Member := Items[Name];
  if not (Member is TJsonArray) then
    raise EJsonError.Create('JSON array expected');
  Result := TJsonArray(Member);
end;

function TJsonObject.GetNumberItem(const Name: WideString): TJsonNumber;
var Member: TJson;
begin
  Member := Items[Name];
  if not (Member is TJsonNumber) then
    raise EJsonError.Create('JSON number expected');
  Result := TJsonNumber(Member);
end;

// TJsonWriter

constructor TJsonWriter.Create(Stream: TStream; PrettyPrint: Boolean = True; UseTabChar: Boolean = False; TabSize: Integer = 2; Utf8: Boolean = True);
begin
  inherited Create;

  {$ifdef TRIAL} ShowTrial; {$endif TRIAL}

  FStream := Stream;
  FDestroyStream := False;
  FUtf8 := Utf8;
  FPrettyPrint := PrettyPrint;
  FUseTabChar := UseTabChar;
  FTabSize := TabSize;
  FFirstItem := True;
end;

constructor TJsonWriter.Create(const FileName: string; PrettyPrint: Boolean = True; UseTabChar: Boolean = False; TabSize: Integer = 2);
begin
  inherited Create;

  {$ifdef TRIAL} ShowTrial; {$endif TRIAL}

  FStream := TFileStream.Create(FileName, fmCreate);
  FDestroyStream := True;
  FUtf8 := True;
  FPrettyPrint := PrettyPrint;
  FUseTabChar := UseTabChar;
  FTabSize := TabSize;
  FFirstItem := True;
end;

destructor TJsonWriter.Destroy;
begin
  if FDestroyStream then
    FreeAndNil(FStream);
  inherited Destroy;
end;

function TJsonWriter.IsEmpty: Boolean;
begin
  Result := FStream.Size = 0
end;

function TJsonWriter.InArray: Boolean;
begin
  Result := (FNestedLevel > 0) and not FNesting[FNestedLevel - 1];
end;

function TJsonWriter.InObject: Boolean;
begin
  Result := (FNestedLevel > 0) and FNesting[FNestedLevel - 1];
end;

procedure TJsonWriter.AddNesting(IsObject: Boolean);
begin
  if Length(FNesting) = FNestedLevel then
    SetLength(FNesting, FNestedLevel + 256);
  FNesting[FNestedLevel] := IsObject;
  Inc(FNestedLevel);
end;

procedure TJsonWriter.WriteIndentation;
begin
  if not IsEmpty then
    if FPrettyPrint then
    begin
      WinJson.Write(FStream, NewLine, FUtf8);
      if FUseTabChar then
        WriteChars(FStream, WideChar(#$9), FNestedLevel, FUtf8)
      else
        WriteChars(FStream, WideChar(' '), FNestedLevel * FTabSize, FUtf8);
    end;
end;

procedure TJsonWriter.WriteSeparator;
begin
  if FFirstItem then
    FFirstItem := False
  else
    WinJson.Write(FStream, WideChar(','), FUtf8);
  WriteIndentation;
end;

procedure TJsonWriter.WriteNamePart(const Name: WideString);
begin
  RequireObject;
  WriteSeparator;
  WriteJson(FStream, Name, FUtf8);
  WinJson.Write(FStream, WideChar(':'), FUtf8);
  if FPrettyPrint then
    WinJson.Write(FStream, WideChar(' '), FUtf8);
end;

procedure TJsonWriter.Check;
begin
  WinJson.Check(FNestedLevel = 0, 'Incorrect nested level');
  WinJson.Check(not IsEmpty, 'No JSON data');
end;

procedure TJsonWriter.RequireObject;
begin
  WinJson.Check(InObject, 'Can be used in JSON object only');
end;

procedure TJsonWriter.RequireArray;
begin
  WinJson.Check(InArray, 'Can be used in JSON array only');
end;

procedure TJsonWriter.WriteNull;
begin
  if not IsEmpty then
  begin
    RequireArray;
    WriteSeparator;
  end;
  WinJson.Write(FStream, 'null', FUtf8);
end;

procedure TJsonWriter.Write(Value: Boolean);
begin
  if not IsEmpty then
  begin
    RequireArray;
    WriteSeparator;
  end;
  if Value then
    WinJson.Write(FStream, 'true', FUtf8)
  else
    WinJson.Write(FStream, 'false', FUtf8)
end;

procedure TJsonWriter.Write(Value: Double);
begin
  if not IsEmpty then
  begin
    RequireArray;
    WriteSeparator;
  end;
  WinJson.Write(FStream, NumberToString(Value), FUtf8);
end;

procedure TJsonWriter.Write(const Value: WideString);
begin
  if not IsEmpty then
  begin
    RequireArray;
    WriteSeparator;
  end;
  WriteJson(FStream, Value, FUtf8);
end;

procedure TJsonWriter.Write(const Value: TDateTime; Offset: Integer);
begin
  if not IsEmpty then
  begin
    RequireArray;
    WriteSeparator;
  end;
  WriteJson(FStream, WinJson.ToString(Value, Offset), FUtf8);
end;

procedure TJsonWriter.WriteNull(const Name: WideString);
begin
  WriteNamePart(Name);
  WinJson.Write(FStream, 'null', FUtf8);
end;

procedure TJsonWriter.Write(const Name: WideString; Value: Boolean);
begin
  WriteNamePart(Name);
  if Value then
    WinJson.Write(FStream, 'true', FUtf8)
  else
    WinJson.Write(FStream, 'false', FUtf8)
end;

procedure TJsonWriter.Write(const Name: WideString; Value: Double);
begin
  WriteNamePart(Name);
  WinJson.Write(FStream, NumberToString(Value), FUtf8);
end;

procedure TJsonWriter.Write(const Name: WideString; const Value: WideString);
begin
  WriteNamePart(Name);
  WriteJson(FStream, Value, FUtf8);
end;

procedure TJsonWriter.Write(const Name: WideString; const Value: TDateTime; Offset: Integer);
begin
  WriteNamePart(Name);
  WriteJson(FStream, WinJson.ToString(Value, Offset), FUtf8);
end;

procedure TJsonWriter.BeginArray;
begin
  if (FNestedLevel > 0) or ((FNestedLevel = 0) and not IsEmpty) then
    RequireArray;
  WriteSeparator;
  WinJson.Write(FStream, WideChar('['), FUtf8);
  AddNesting(False);
  FFirstItem := True;
end;

procedure TJsonWriter.BeginObject;
begin
  if (FNestedLevel > 0) or ((FNestedLevel = 0) and not IsEmpty) then
    RequireArray;
  WriteSeparator;
  WinJson.Write(FStream, WideChar('{'), FUtf8);
  AddNesting(True);
  FFirstItem := True;
end;

procedure TJsonWriter.BeginArray(const Name: WideString);
begin
  WriteNamePart(Name);
  WinJson.Write(FStream, WideChar('['), FUtf8);
  AddNesting(False);
  FFirstItem := True;
end;

procedure TJsonWriter.BeginObject(const Name: WideString);
begin
  WriteNamePart(Name);
  WinJson.Write(FStream, WideChar('{'), FUtf8);
  AddNesting(True);
  FFirstItem := True;
end;

procedure TJsonWriter.EndArray;
begin
  RequireArray;
  Dec(FNestedLevel);
  WriteIndentation;
  WinJson.Write(FStream, WideChar(']'), FUtf8);
  FFirstItem := False;
end;

procedure TJsonWriter.EndObject;
begin
  RequireObject;
  Dec(FNestedLevel);
  WriteIndentation;
  WinJson.Write(FStream, WideChar('}'), FUtf8);
  FFirstItem := False;
end;

// TJsonReader

constructor TJsonReader.Create(const Text: WideString);
begin
  inherited Create;
  Parse(Text);
  FItemPosition := FPosition;
end;

constructor TJsonReader.Create(const Data: TBytes; Utf8: Boolean);
begin
  inherited Create;
  if Utf8 then
    ParseUtf8(Data)
  else
    ParseAnsi(Data);
  FItemPosition := FPosition;
end;

{$ifndef NEXTGEN}
constructor TJsonReader.Create(const Data: AnsiString; Utf8: Boolean);
begin
  inherited Create;
  if Utf8 then
    ParseUtf8(Data)
  else
    ParseAnsi(Data);
  FItemPosition := FPosition;
end;
{$endif NEXTGEN}

constructor TJsonReader.Create(Stream: TStream; Utf8: Boolean);
begin
  inherited Create;
{$ifdef NEXTGEN}
  if Utf8 then
    ParseUtf8(StreamToBytes(Stream))
  else
    ParseAnsi(StreamToBytes(Stream));
{$else}
  if Utf8 then
    ParseUtf8(StreamToAnsiString(Stream))
  else
    ParseAnsi(StreamToAnsiString(Stream));
{$endif NEXTGEN}
end;

constructor TJsonReader.Create(const FileName: string; Utf8, FromFile: Boolean);
begin
  inherited Create;
  if Utf8 then
    ParseUtf8File(FileName)
  else
    ParseAnsiFile(FileName);
  FItemPosition := FPosition;
end;

function TJsonReader.InArray: Boolean;
begin
  Result := (FNestedLevel > 0) and not FNesting[FNestedLevel - 1];
end;

function TJsonReader.InObject: Boolean;
begin
  Result := (FNestedLevel > 0) and FNesting[FNestedLevel - 1];
end;

procedure TJsonReader.AddNesting(IsObject: Boolean);
begin
  if Length(FNesting) = FNestedLevel then
    SetLength(FNesting, FNestedLevel + 256);
  FNesting[FNestedLevel] := IsObject;
  Inc(FNestedLevel);
end;

function TJsonReader.Read: TJsonItem;
begin
  FIsMember := False;
  SkipWhiteSpace;

  if IsEnd then
    if InObject then
      inherited Read('}') // raise expected '}' exception
    else if InArray then
      inherited Read(']') // raise expected ']' exception
    else if not FWasJsonItem then
      Error(FPosition, 'No JSON value found')
    else
    begin
      FItemPosition := FPosition;
      Result := itEof;
      Exit;
    end;

  if (FNestedLevel = 0) and FWasJsonItem then
    Error(FPosition, 'Unexpected character');

  FWasJsonItem := True;

  if InObject then
  begin
    if Current = '}' then
    begin
      FItemPosition := FPosition;
      Next; // skip '}'
      Dec(FNestedLevel);
      FFirstItem := False;
      Result := itEndObject;
      Exit;
    end;

    if FFirstItem then
      FFirstItem := False
    else
    begin
      inherited Read(',');
      SkipWhiteSpace;
    end;

    FIsMember := True;
    FItemPosition := FPosition;
    FMemberName := ParseString;
    SkipWhiteSpace;
    inherited Read(':');
    SkipWhiteSpace;
    Result := ReadValue;
  end
  else if InArray then
  begin
    if Current = ']' then
    begin
      FItemPosition := FPosition;
      Next; // skip ']'
      Dec(FNestedLevel);
      FFirstItem := False;
      Result := itEndArray;
      Exit;
    end;

    if FFirstItem then
      FFirstItem := False
    else
    begin
      inherited Read(',');
      SkipWhiteSpace;
    end;

    FItemPosition := FPosition;
    Result := ReadValue;
  end
  else
  begin
    FItemPosition := FPosition;
    Result := ReadValue;
  end;
end;

function TJsonReader.ReadValue: TJsonItem;
begin
  case Current of
    '{':
    begin
      Next; // skip '{'
      AddNesting(True);
      FFirstItem := True;
      Result := itBeginObject;
    end;

    '[':
    begin
      Next; // skip '['
      AddNesting(False);
      FFirstItem := True;
      Result := itBeginArray;
    end;

    '"':
    begin
      FStringValue := ParseString;
      Result := itString;
    end;

    '-', '0'..'9':
    begin
      FNumberValue := ParseNumber;
      Result := itNumber;
    end;

    else
      Result := ParseLiteral
  end;
end;

function TJsonReader.ParseLiteral: TJsonItem;
var
  Start: Integer;
  Value: WideString;
begin
  Result := itNull; // to avoid warning
  Start := FPosition;
  while not IsEnd and not IsWhiteSpace and not IsStructuralCharacter and (Current <> '-') and (Current <> '"') do
    Next;

{$ifdef NEXTGEN}
  Value := FText.Substring(Start, FPosition - Start);
{$else}
  Value := Copy(FText, Start, FPosition - Start);
{$endif NEXTGEN}
  if Value = 'null' then
    Result := itNull
  else if Value = 'false' then
    Result := itFalse
  else if Value = 'true' then
    Result := itTrue
  else
    Error(Start, 'Incorrect value ' + Value);
end;

function TJsonReader.GetColumn: Integer;
var Row: Integer;
begin
  PositionToRowColumn(FItemPosition, Row, Result);
end;

function TJsonReader.GetRow: Integer;
var Column: Integer;
begin
  PositionToRowColumn(FItemPosition, Result, Column);
end;

{$ifdef TRIAL}
var WasTrial: Boolean;

procedure ShowTrial;
begin
  if not WasTrial then
  begin
    WasTrial := True;
{$ifdef MSWINDOWS}
    MessageBox(0,
     'A trial version of JSON library started.' + #13#13 +
     'Please note that trial version is supposed to be used for evaluation only. ' +
     'If you wish to distribute JSON library as part of your application, ' +
     'you must register from website at https://www.winsoft.sk.' + #13#13 +
     'Thank you for trialing JSON library.',
     'JSON library 4.8, Copyright (c) 2013-2021 WINSOFT', MB_OK or MB_ICONINFORMATION);
{$else}
  {$ifdef LINUX}
    WriteLn('JSON library 4.8, Copyright (c) 2013-2021 WINSOFT');
    WriteLn('A trial version of JSON library started.');
    WriteLn('Please note that trial version is supposed to be used for evaluation only.');
    WriteLn('If you wish to distribute JSON library as part of your application,');
    WriteLn('you must register from website at https://www.winsoft.sk.');
    WriteLn('Thank you for trialing JSON library.');
    ReadLn;
  {$else}
    ShowMessage(
     'JSON library 4.8' + #13#10 +
     'Copyright (c) 2013-2021 WINSOFT' + #13#10#13#10 +
     'A trial version of JSON library started.' + #13#10#13#10 +
     'Please note that trial version is supposed to be used for evaluation only. ' +
     'If you wish to distribute JSON library as part of your application, ' +
     'you must register from website at https://www.winsoft.sk.' + #13#10#13#10 +
     'Thank you for trialing JSON library.');
  {$endif}
{$endif MSWINDOWS}
  end;
end;

{$endif TRIAL}

initialization
  JsonNull := TJsonNull.Create;
  JsonFalse := TJsonFalse.Create;
  JsonTrue := TJsonTrue.Create;
finalization
  JsonTrue.FFreeInstance := True;
  JsonFalse.FFreeInstance := True;
  JsonNull.FFreeInstance := True;
  FreeAndNil(JsonTrue);
  FreeAndNil(JsonFalse);
  FreeAndNil(JsonNull);
end.