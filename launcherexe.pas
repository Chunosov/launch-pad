unit LauncherExe;

{$mode objfpc}{$H+}

interface

uses
  OriXmlFile, Launcher;

type
  TLauncherExe = class(TLauncher)
  private
    FFileName: String;
    FCmdLine: String;
    FCurDir: String;
    FUseStderr: Boolean;
  protected
    procedure SaveXMLInternal(Xml: TOriXmlFileWriter); override;
    procedure LoadXmlInternal(Xml: TOriXmlFileReader); override;
  public
    property FileName: String read FFileName write FFileName;
    property CmdLine: String read FCmdLine write FCmdLine;
    property CurDir: String read FCurDir write FCurDir;
    property UseStdErr: Boolean read FUseStderr write FUseStderr;
    procedure Launch; override;
    function Configure: Boolean; override;
    class function TypeTitle: String; override;
  end;

implementation

uses
  SysUtils, Classes, Dialogs, Process, UTF8Process, LazFileUtils,
  WinLauncherExeProps;

resourcestring
  SLauncherExeTitle = 'Executable File Launcher';

{%region TLauncherExe}
class function TLauncherExe.TypeTitle: String;
begin
  Result := SLauncherExeTitle;
end;

function TLauncherExe.Configure: Boolean;
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

procedure TLauncherExe.Launch;
var
  Process: TProcessUTF8;

  function GetStderr: String;
  var Stderr: TStrings;
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
  Error: String;
begin
	Process := TProcessUTF8.Create(nil);
  try
	  Process.Executable := FileName;
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

initialization
  LauncherTypes.Add(TLauncherExe);

end.

