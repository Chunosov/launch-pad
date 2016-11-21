unit WinScriptEditor;

{$mode objfpc}{$H+}

interface

uses
  Controls, ButtonPanel, ExtCtrls, StdCtrls,
  SynEdit, SynEditHighlighter, SynHighlighterPython,
  EditBtn, OriTabs, OriDialogs, LauncherScript;

type

  { TWndScriptEditor }

  TWndScriptEditor = class(TOriDialog)
    ButtonPanel: TButtonPanel;
    CodeEditor: TSynEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MemoRunnerArgs: TMemo;
    Tabs: TOriTabSet;
    PanelOptions: TPanel;
    TextCurDir: TDirectoryEdit;
    TextRunner: TFileNameEdit;
    TextTitle: TEdit;
  private
    FLauncher: TLauncherScript;
    function GetScriptStub: string;
    function GetHighlighter: TSynCustomHighlighter;
  protected
    procedure Populate; override;
    procedure Collect; override;
  public
    constructor Create(ALauncher: TLauncherScript); reintroduce;
  end;

implementation

uses
  SysUtils, LazFileUtils,
  OriUtils_Gui, OriIniFile;

{$R *.lfm}

resourcestring
  SUnableSaveScript = 'Unable to save script into the file'#10#13'%s:'#10#13#10#13'%s';

constructor TWndScriptEditor.Create(ALauncher: TLauncherScript);
begin
  inherited Create;
  FLauncher := ALauncher;

  PanelOptions.Align := alClient;
  CodeEditor.Highlighter := GetHighlighter;
end;

procedure TWndScriptEditor.Populate;

  procedure LoadScript(const Path: string);
  begin
    if FileExistsUTF8(Path) then
    begin
      CodeEditor.Lines.LoadFromFile(Path);
      Tabs.ActiveTabIndex := 1;
      CodeEditor.SetFocus;
    end
    else
    begin
      CodeEditor.Lines.Text := GetScriptStub;
      Tabs.ActiveTabIndex := 0;
      TextTitle.SetFocus;
    end;
  end;

begin
  TextTitle.Text := FLauncher.Title;
  TextCurDir.Text := FLauncher.CurDir;
  TextRunner.Text := FLauncher.Runner;
  MemoRunnerArgs.Lines.Text := FLauncher.RunnerArgs;
  LoadScript(FLauncher.ScriptFile);
end;

procedure TWndScriptEditor.Collect;

  procedure SaveScript(const Path: string);
  begin
    try
      CodeEditor.Lines.SaveToFile(Path);
    except on e: Exception do
      ErrorDlg(SUnableSaveScript, [Path, e.Message]);
    end;
  end;

begin
  FLauncher.Title := Trim(TextTitle.Text);
  FLauncher.CurDir := Trim(TextCurDir.Text);
  FLauncher.Runner := Trim(TextRunner.Text);
  FLauncher.RunnerArgs := Trim(MemoRunnerArgs.Lines.Text);
  SaveScript(FLauncher.ScriptFile);
end;

function TWndScriptEditor.GetScriptStub: string;
begin
  if FLauncher is TLauncherPython then Result :=
      'import sys'#13#10#13#10 +
      'print("Python path = %s" % sys.path)'#13#10#13#10 +
      'input("Press Enter to exit...")'#13#10
  else Result := '';
end;

function TWndScriptEditor.GetHighlighter: TSynCustomHighlighter;
begin
  if FLauncher is TLauncherPython then
    Result := TSynPythonSyn.Create(CodeEditor)
  else Result := nil;
end;

end.

