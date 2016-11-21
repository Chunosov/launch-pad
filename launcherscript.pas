unit LauncherScript;

{$mode objfpc}{$H+}

interface

uses
  Launcher, OriXmlFile;

type
  TLauncherScript = class abstract(TLauncher)
  private
    FCurDir: string;
    FRunner: string;
    FRunnerArgs: string;
    FScriptFile: string;
    class function GetScriptsDir: string;
    class function MakeFileName: string;
    function GetScriptFile: string;
  protected
    class function GetScriptsExt: string; virtual; abstract;
    procedure SaveXMLInternal(Xml: TOriXmlFileWriter); override;
    procedure LoadXmlInternal(Xml: TOriXmlFileReader); override;
  public
    procedure Launch; override;
    function Configure: boolean; override;
    property CurDir: string read FCurDir write FCurDir;
    property Runner: string read FRunner write FRunner;
    property RunnerArgs: string read FRunnerArgs write FRunnerArgs;
    property ScriptFile: string read GetScriptFile;
  end;

  TLauncherPas = class(TLauncherScript)
  protected
    class function GetScriptsExt: string; override;
  public
    class function TypeTitle: string; override;
  end;

  TLauncherPython = class(TLauncherScript)
  protected
    class function GetScriptsExt: string; override;
  public
    constructor Create; override;
    class function TypeTitle: string; override;
  end;

  TLauncherBat1 = class(TLauncherScript)
  protected
    class function GetScriptsExt: string; override;
  public
    class function TypeTitle: string; override;
  end;

implementation

uses
  SysUtils, LazFileUtils, UTF8Process,
  OriUtils, LauncherExe, WinScriptEditor;

resourcestring
  SLauncherBatTitle = 'Shell Commands';
  SLauncherPasTitle = 'Pascal Script';
  SLauncherPyTitle = 'Python Script';
  SLauncherFailed = 'Failed to execute script:'#10#13'%s:'#10#13#10#13'%s';
  SScriptNotFound = 'Script file not found:'#10#13'%s';

{%region TLauncherScript}
class function TLauncherScript.GetScriptsDir: string;
begin
  Result := GetLocalPath + GetScriptsExt;
  if not DirectoryExistsUTF8(Result) then
    CreateDirUTF8(Result);
end;

class function TLauncherScript.MakeFileName: string;
var Guid: TGuid;
begin
  CreateGUID(Guid);
  Result := GUIDToString(Guid) + GetScriptsExt;
end;

function TLauncherScript.GetScriptFile: string;
begin
  Result := AppendPathDelim(GetScriptsDir) + FScriptFile;
end;

function TLauncherScript.Configure: boolean;
begin
  if FScriptFile = '' then FScriptFile := MakeFileName;
  Result := TWndScriptEditor.Create(Self).Exec;
end;

procedure TLauncherScript.LoadXmlInternal(Xml: TOriXmlFileReader);
begin
  FCurDir := Xml.Text['CurDir'];
  FScriptFile := Xml.Text['ScriptFile'];
  FRunner := Xml.Text['Runner'];
  FRunnerArgs := Xml.Text['RunnerArgs'];
end;

procedure TLauncherScript.SaveXMLInternal(Xml: TOriXmlFileWriter);
begin
  if FCurDir <> '' then Xml.Text['CurDir'] := FCurDir;
  if FScriptFile <> '' then Xml.Text['ScriptFile'] := FScriptFile;
  if FRunner <> '' then Xml.Text['Runner'] := FRunner;
  if FRunnerArgs <> '' then Xml.Text['RunnerArgs'] := FRunnerArgs;
end;

procedure TLauncherScript.Launch;
var
  Process: TProcessUTF8;
  Script: string;
begin
  Script := GetScriptFile;
  if (Script <> '') and not FileExistsUTF8(Script) then
    raise Exception.CreateFmt(SScriptNotFound, [Script]);

  Process := TProcessUTF8.Create(nil);
  try
    try
      if FRunner <> '' then
      begin
        Process.Executable := FRunner;
        Process.Parameters.Text := FRunnerArgs;
        Process.Parameters.Add(Script);
      end
      else
        Process.Executable := Script;
      if (CurDir <> '') and DirectoryExistsUTF8(CurDir) then
        Process.CurrentDirectory := CurDir;
      Process.Execute;
    except
      on e: Exception do
        raise Exception.CreateFmt(SLauncherFailed, [Script, FormatError(e)]);
    end;
  finally
    Process.Free;
  end;
end;
{%endregion}

{%region TLauncherPas}
class function TLauncherPas.TypeTitle: string;
begin
  Result := SLauncherPasTitle;
end;

class function TLauncherPas.GetScriptsExt: string;
begin
  Result := '.pas';
end;
{%endregion}

{%region TLauncherBat}
class function TLauncherBat1.TypeTitle: string;
begin
  Result := SLauncherBatTitle;
end;

class function TLauncherBat1.GetScriptsExt: string;
begin
  Result := '.bat';
end;
{%endregion}

{%region TLauncherPython}
constructor TLauncherPython.Create;
begin
  inherited;
  FRunner := 'python';
end;

class function TLauncherPython.TypeTitle: string;
begin
  Result := SLauncherPyTitle;
end;

class function TLauncherPython.GetScriptsExt: string;
begin
  Result := '.py';
end;
{%endregion}

initialization
  LauncherTypes.Add(TLauncherBat1);
  //LauncherTypes.Add(TLauncherPas);
  LauncherTypes.Add(TLauncherPython);

end.

