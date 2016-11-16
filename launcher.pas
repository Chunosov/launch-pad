unit Launcher;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, FGL, OriXmlFile;

type
  TLauncher = class
    abstract
  protected
    FTitle: string;
    FHidden: boolean;
    procedure SaveXmlInternal(Xml: TOriXmlFileWriter); virtual; abstract;
    procedure LoadXmlInternal(Xml: TOriXmlFileReader); virtual; abstract;
  public
    constructor Create; virtual;
    procedure Launch; virtual; abstract;
    function Configure: boolean; virtual; abstract;
    procedure SaveXml(Xml: TOriXmlFileWriter);
    procedure LoadXml(Xml: TOriXmlFileReader);
    property Title: string read FTitle write FTitle;
    property Hidden: boolean read FHidden write FHidden;
    class function TypeTitle: string; virtual; abstract;
  end;

  TLauncherType = class of TLauncher;
  TLauncherTypeList = specialize TFPGList<TLauncherType>;
  TLauncherList = specialize TFPGList<TLauncher>;

  TLauncherCategory = class
  private
    FTitle: string;
    FHidden: boolean;
    FLaunchers: TLauncherList;
    procedure SaveXml(Xml: TOriXmlFileWriter);
    procedure LoadXml(Xml: TOriXmlFileReader);
  public
    constructor Create;
    destructor Destroy; override;
    property Title: string read FTitle write FTitle;
    property Hidden: boolean read FHidden write FHidden;
    property Launchers: TLauncherList read FLaunchers;
  end;

  TLauncherCategoryList = specialize TFPGList<TLauncherCategory>;

  TLaunchersBank = class
  private
    FFileName: string;
    FCategories: TLauncherCategoryList;
    procedure LoadXml;
    procedure SaveXml;
    procedure Load(AFileName: string);
  public
    constructor Create(AFileName: string);
    destructor Destroy; override;
    procedure Save;
    property FileName: string read FFileName;
    property Categories: TLauncherCategoryList read FCategories;

    function IndexOf(ACategory: TLauncherCategory): Integer;
    function MoveLeft(ACategory: TLauncherCategory): Boolean;
    function MoveRight(ACategory: TLauncherCategory): Boolean;
  end;

  ELauncher = class(Exception);

function GetLauncherTypeByName(const AName: string): TLauncherType;

var
  LauncherTypes: TLauncherTypeList;

const
  DefBankFileName = 'LaunchPad.xml';
  DefCategoryTitle = 'Default';
  BankVersion = '1.0';
  TagBank = 'Bank';
  TagCategories = 'Categories';
  TagCategory = 'Category';
  TagLaunchers = 'Launchers';
  TagLauncher = 'Launcher';
  AttrBankVersion = 'Version';
  AttrCategoryTitle = 'Title';
  AttrCategoryHidden = 'Hidden';
  AttrLauncherTitle = 'Title';
  AttrLauncherType = 'Type';
  AttrLauncherHidden = 'Hidden';

implementation

uses
  LazFileUtils, OriUtils;

resourcestring
  LoadErr_UnsupportedFileVersion = 'Unsupported verion of configuration file';

{%region Helpers}
function GetLauncherTypeByName(const AName: string): TLauncherType;
var
  I: integer;
begin
  for I := 0 to LauncherTypes.Count - 1 do
  begin
    if SameText(LauncherTypes[I].ClassName, AName) then
    begin
      Result := LauncherTypes[I];
      Exit;
    end;
  end;
  Result := nil;
end;

{%endregion}

{%region TLauncher}
constructor TLauncher.Create;
begin
end;

procedure TLauncher.SaveXml(Xml: TOriXmlFileWriter);
begin
  Xml.Open(TagLauncher);
  Xml.Attribute[AttrLauncherTitle] := FTitle;
  Xml.Attribute[AttrLauncherType] := ClassName;
  if FHidden then
    Xml.BoolAttribute[AttrLauncherHidden] := FHidden;
  SaveXMLInternal(Xml);
  Xml.Close;
end;

procedure TLauncher.LoadXml(Xml: TOriXmlFileReader);
begin
  FTitle := Xml.Attribute[AttrLauncherTitle];
  FHidden := Xml.BoolAttribute[AttrLauncherHidden];
  LoadXmlInternal(Xml);
end;

{%endregion}

{%region TLauncherCategory}
constructor TLauncherCategory.Create;
begin
  FLaunchers := TLauncherList.Create;
end;

destructor TLauncherCategory.Destroy;
begin
  FreeAndClearList(FLaunchers);
end;

procedure TLauncherCategory.SaveXml(Xml: TOriXmlFileWriter);
var
  L: TLauncher;
begin
  Xml.Open(TagCategory);
  Xml.Attribute[AttrCategoryTitle] := FTitle;
  if FHidden then
    Xml.BoolAttribute[AttrCategoryHidden] := FHidden;
  Xml.Open(TagLaunchers);
  for L in FLaunchers do
    L.SaveXml(Xml);
  Xml.Close;
  Xml.Close;
end;

procedure TLauncherCategory.LoadXml(Xml: TOriXmlFileReader);

  procedure LoadLauncher;
  var
    LauncherTypeName: string;
    LauncherType: TLauncherType;
    Launcher: TLauncher;
  begin
    LauncherTypeName := Xml.Attribute[AttrLauncherType];
    LauncherType := GetLauncherTypeByName(LauncherTypeName);
    if Assigned(LauncherType) then
    begin
      Launcher := LauncherType.Create;
      Launcher.LoadXml(Xml);
      FLaunchers.Add(Launcher);
    end;
  end;

begin
  FTitle := Xml.Attribute[AttrCategoryTitle];
  FHidden := Xml.BoolAttribute[AttrCategoryHidden];

  Xml.Open(TagLaunchers);
  while Xml.List(TagLauncher) do
    LoadLauncher;
  Xml.Close;
end;

{%endregion}

{%region TLaunchersBank}
constructor TLaunchersBank.Create(AFileName: string);
begin
  FCategories := TLauncherCategoryList.Create;

  Load(AFileName);
end;

destructor TLaunchersBank.Destroy;
begin
  FreeAndClearList(FCategories);
end;

procedure TLaunchersBank.Load(AFileName: string);
var
  C: TLauncherCategory;
begin
  FFileName := AFileName;
  if FileExists(AFileName) then
    LoadXml;

  if FCategories.Count = 0 then
  begin
    C := TLauncherCategory.Create;
    C.Title := DefCategoryTitle;
    FCategories.Add(C);
  end;
end;

procedure TLaunchersBank.Save;
begin
  SaveXML;
end;

procedure TLaunchersBank.LoadXml;
var
  Xml: TOriXmlFileReader;

  procedure LoadCategory;
  var
    C: TLauncherCategory;
  begin
    C := TLauncherCategory.Create;
    C.LoadXml(Xml);
    FCategories.Add(C);
  end;

begin
  Xml := TOriXmlFileReader.Create(FFileName);
  try
    Xml.Open(TagBank);
    if Xml.Attribute[AttrBankVersion] <> BankVersion then
      raise Exception.Create(LoadErr_UnsupportedFileVersion);
    Xml.Open(TagCategories);
    while Xml.List(TagCategory) do
      LoadCategory;
    Xml.Close;
    Xml.Close;
  finally
    Xml.Free;
  end;
end;

procedure TLaunchersBank.SaveXml;
var
  Xml: TOriXmlFileWriter;
  C: TLauncherCategory;
begin
  Xml := TOriXmlFileWriter.Create(FFileName);
  try
    Xml.Open(TagBank);
    Xml.Attribute[AttrBankVersion] := BankVersion;
    Xml.Open(TagCategories);
    for C in FCategories do
      C.SaveXml(Xml);
    Xml.Close;
    Xml.Close;
  finally
    Xml.Free;
  end;
end;

function TLaunchersBank.IndexOf(ACategory: TLauncherCategory): Integer;
begin
  Result := FCategories.IndexOf(ACategory);
end;

function TLaunchersBank.MoveLeft(ACategory: TLauncherCategory): Boolean;
var Index: Integer;
begin
  Result := False;
  Index := IndexOf(ACategory);
  if Index > 0 then
  begin
    FCategories.Delete(Index);
    FCategories.Insert(Index-1, ACategory);
    Result := True;
  end;
end;

function TLaunchersBank.MoveRight(ACategory: TLauncherCategory): Boolean;
var Index: Integer;
begin
  Result := False;
  Index := IndexOf(ACategory);
  if Index < FCategories.Count-1 then
  begin
    FCategories.Delete(Index);
    FCategories.Insert(Index+1, ACategory);
    Result := True;
  end;
end;

{%endregion}

initialization
  LauncherTypes := TLauncherTypeList.Create;

finalization
  LauncherTypes.Free;

end.
