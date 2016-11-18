unit LauncherExe;

{$mode objfpc}{$H+}

interface

uses
  OriXmlFile, Launcher;

type
  TLauncherExe = class(TLauncher)
  private
    FFileName: string;
    FCmdLine: string;
    FCurDir: string;
    FUseStderr: boolean;
  protected
    function GetExecutablePath: string; virtual;
    procedure SaveXMLInternal(Xml: TOriXmlFileWriter); override;
    procedure LoadXmlInternal(Xml: TOriXmlFileReader); override;
  public
    property FileName: string read FFileName write FFileName;
    property CmdLine: string read FCmdLine write FCmdLine;
    property CurDir: string read FCurDir write FCurDir;
    property UseStdErr: boolean read FUseStderr write FUseStderr;
    procedure Launch; override;
    function Configure: boolean; override;
    class function TypeTitle: string; override;
  end;

  TLauncherBat = class(TLauncherExe)
  private
    class function GetScriptsDir: string;
    class function MakeFileName: string;
  public
    function GetExecutablePath: string; override;
    function Configure: boolean; override;
    class function TypeTitle: string; override;
  end;

implementation

uses
  SysUtils, Classes, Dialogs, Process, UTF8Process, LazFileUtils,
  OriUtils,
  WinLauncherExeProps, WinLauncherBatProps;

resourcestring
  SLauncherExeTitle = 'Executable File Launcher';
  SLauncherBatTitle = 'Shell Commands Launcher';
  SExecutableNotFound = 'Executable file or script not found:'#10#13'%s';

const
  ExtBat = '.bat';

{%region TLauncherExe}
class function TLauncherExe.TypeTitle: string;
begin
  Result := SLauncherExeTitle;
end;

function TLauncherExe.Configure: boolean;
begin
  Result := TWndLauncherExeProps.Create(Self).Exec;
end;

procedure TLauncherExe.LoadXmlInternal(Xml: TOriXmlFileReader);
begin
  FFileName := Xml.Text['FileName'];
  FCmdLine := Xml.Text['CmdLine'];
  FCurDir := Xml.Text['CurDir'];
  if Xml.TryOpen('Options') then
  begin
    FUseStderr := not Xml.BoolAttribute['DontShowStderr'];
    Xml.Close;
  end;
end;

procedure TLauncherExe.SaveXMLInternal(Xml: TOriXmlFileWriter);
begin
  Xml.Text['FileName'] := FFileName;
  Xml.Text['CmdLine'] := FCmdLine;
  Xml.Text['CurDir'] := FCurDir;
  Xml.Open('Options');
  if not FUseStderr then
    Xml.BoolAttribute['DontShowStderr'] := True;
  Xml.Close;
end;

function TLauncherExe.GetExecutablePath: string;
begin
  Result := FFileName;
end;

procedure TLauncherExe.Launch;
var
  Process: TProcessUTF8;

  function GetStderr: string;
  var
    Stderr: TStrings;
  begin
    Stderr := TStringList.Create;
    try
      Stderr.LoadFromStream(Process.Stderr);
      Result := Trim(Stderr.Text);
    finally
      Stderr.Free;
    end;
  end;

var
  Error, Exe: string;
begin
  Exe := GetExecutablePath;
  if (Exe <> '') and not FileExistsUTF8(Exe) then
    raise Exception.CreateFmt(SExecutableNotFound, [Exe]);

  Process := TProcessUTF8.Create(nil);
  try
    Process.Executable := Exe;
    Process.Parameters.Text := CmdLine;
    if UseStderr then
      Process.Options := [poUsePipes];
    if (CurDir <> '') and DirectoryExistsUTF8(CurDir) then
      Process.CurrentDirectory := CurDir;
    Process.Execute;
    if UseStderr then
    begin
      Error := GetStderr;
      if Error <> '' then
        raise ELauncher.Create(Error);
    end;
  finally
    Process.Free;
  end;
end;
{%endregion}

{%region TLauncherBat}
class function TLauncherBat.GetScriptsDir: string;
begin
  Result := GetLocalPath + ExtBat;
  if not DirectoryExistsUTF8(Result) then
    CreateDirUTF8(Result);
end;

class function TLauncherBat.MakeFileName: string;
var Guid: TGuid;
begin
  CreateGUID(Guid);
  Result := GUIDToString(Guid) + ExtBat;
end;

class function TLauncherBat.TypeTitle: string;
begin
  Result := SLauncherBatTitle;
end;

function TLauncherBat.Configure: boolean;
begin
  if FFileName = '' then FFileName := MakeFileName;
  Result := TWndLauncherBatProps.Create(Self).Exec;
end;

function TLauncherBat.GetExecutablePath: string;
begin
  Result := AppendPathDelim(GetScriptsDir) + FFileName;
end;

{%endregion}

initialization
  LauncherTypes.Add(TLauncherExe);
  LauncherTypes.Add(TLauncherBat);

end.
