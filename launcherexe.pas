unit LauncherExe;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  OriXmlFile, Launcher;

type
  TLauncherExe = class(TLauncher)
  private
    FFileName: string;
    FCmdLine: string;
    FCurDir: string;
  protected
    function GetExecutablePath: string; virtual;
    procedure SaveXMLInternal(Xml: TOriXmlFileWriter); override;
    procedure LoadXmlInternal(Xml: TOriXmlFileReader); override;
  public
    property FileName: string read FFileName write FFileName;
    property CmdLine: string read FCmdLine write FCmdLine;
    property CurDir: string read FCurDir write FCurDir;
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

function FormatError(Error: Exception): string;

implementation

uses
  Classes, Dialogs, Process, UTF8Process, LazFileUtils,  LazUTF8,
  {$ifdef WINDOWS} Windows, {$endif}
  OriUtils,
  WinLauncherExeProps, WinLauncherBatProps;

resourcestring
  SLauncherExeTitle = 'Executable File Launcher';
  SLauncherBatTitle = 'Shell Commands Launcher';
  SLauncherFailed = 'Failed to execute file:'#10#13'%s:'#10#13#10#13'%s: %s';

const
  ExtBat = '.bat';

{%region Helpers}
{$ifdef WINDOWS}
function FormatError(Error: Exception): string;
var
  Pos, ErrorCode, Size: Integer;
  Buf: array[0..2047] of WideChar;
begin
  if not (Error is EProcess) then
    exit(Error.ClassName + ': ' + Error.Message);

  Pos := CharPos(Error.Message, ':');
  // TProcessUTF8.Execute raises EProcess with message: 'Failed to execute {CommandLine} : {GetLastError}'
  if (Pos < 1) or not TryStrToInt(Trim(Copy(Error.Message, Pos+1, MaxInt)), ErrorCode) then exit(Error.Message);
  Size := FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS,
    nil, ErrorCode, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), Buf, 2048, nil);
  Result := UTF16ToUTF8(Buf, Size);
end;
{$else}
function FormatError(Error: EProcess): string;
begin
  Result := Error.Message;
end;
{$endif}
{%endregion}

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
end;

procedure TLauncherExe.SaveXMLInternal(Xml: TOriXmlFileWriter);
begin
  if FFileName <> '' then Xml.Text['FileName'] := FFileName;
  if FCmdLine <> '' then Xml.Text['CmdLine'] := FCmdLine;
  if FCurDir <> '' then Xml.Text['CurDir'] := FCurDir;
end;

function TLauncherExe.GetExecutablePath: string;
begin
  Result := FFileName;
end;

procedure TLauncherExe.Launch;
var
  Process: TProcessUTF8;
  Exe: string;
begin
  Exe := GetExecutablePath;
  // Do not check if file exists! It will fail for file without full path.

  Process := TProcessUTF8.Create(nil);
  try
    try
      Process.Executable := Exe;
      Process.Parameters.Text := CmdLine;
      if (CurDir <> '') and DirectoryExistsUTF8(CurDir) then
        Process.CurrentDirectory := CurDir;
      Process.Execute;
    except
      on e: Exception do
        raise Exception.CreateFmt(SLauncherFailed, [Exe, e.ClassName, e.Message]);
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
