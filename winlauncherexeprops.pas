unit WinLauncherExeProps;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, ButtonPanel, StdCtrls, ExtCtrls, Dialogs,
  OriDialogs,
  LauncherExe;

type
  TWndLauncherExeProps = class(TOriDialog)
    ButtonFileName: TButton;
		ButtonCurDir: TButton;
    ButtonPanel: TButtonPanel;
		Label3: TLabel;
		OptionsGroup: TCheckGroup;
		Label6: TLabel;
		OpenDialog1: TOpenDialog;
		PanelCurDir: TPanel;
		SelectDirectoryDialog1: TSelectDirectoryDialog;
    TextFileName: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    MemoCmdLine: TMemo;
    PanelFileName: TPanel;
		TextCurDir: TEdit;
		TextTitle: TEdit;
		procedure ButtonCurDirClick(Sender: TObject);
    procedure ButtonFileNameClick(Sender: TObject);
  private
    FLauncher: TLauncherExe;
  protected
    procedure Populate; override;
    procedure Collect; override;
  public
    constructor Create(ALauncher: TLauncherExe); reintroduce;
  end;

implementation

uses
  SysUtils, LazFileUtils, LazUTF8;

resourcestring
{$ifdef WINDOWS}
  SOpenDialogFilter = 'All files (*.*)|*.*|Executable files (*.exe)|*.exe';
{$else}
  SOpenDialogFilter = 'All files (*.*)';
{$endif}

{$R *.lfm}

constructor TWndLauncherExeProps.Create(ALauncher: TLauncherExe);
begin
  inherited Create;
  FLauncher := ALauncher;
end;

procedure TWndLauncherExeProps.Populate;
begin
  TextTitle.Text := FLauncher.Title;
  TextFileName.Text := FLauncher.FileName;
  TextCurDir.Text := FLauncher.CurDir;
  MemoCmdLine.Lines.Text := FLauncher.CmdLine;
  OptionsGroup.Checked[0] := not FLauncher.UseStdErr;
end;

procedure TWndLauncherExeProps.Collect;
begin
  FLauncher.Title := Trim(TextTitle.Text);
  FLauncher.FileName := Trim(TextFileName.Text);
  FLauncher.CurDir := Trim(TextCurDir.Text);
  FLauncher.CmdLine := Trim(MemoCmdLine.Lines.Text);
  FLauncher.UseStdErr := not OptionsGroup.Checked[0];
end;

procedure TWndLauncherExeProps.ButtonFileNameClick(Sender: TObject);
var
  FileName, Dir: String;
begin
  FileName := Trim(TextFileName.Text);
  Dir := ExtractFilePath(FileName);
  if (Dir = '') or not DirectoryExists(Dir) then
    Dir := ExtractFilePath(LazUTF8.ParamStrUTF8(0));
  OpenDialog1.Filter := SOpenDialogFilter;
  OpenDialog1.FileName := FileName;
  OpenDialog1.InitialDir := Dir;
  if OpenDialog1.Execute then
    TextFileName.Text := OpenDialog1.FileName;
end;

procedure TWndLauncherExeProps.ButtonCurDirClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := Trim(TextCurDir.Text);
  if (Dir = '') or not DirectoryExistsUTF8(Dir) then
    Dir := ExtractFilePath(ParamStrUTF8(0));
  SelectDirectoryDialog1.FileName := Dir;
  if SelectDirectoryDialog1.Execute then
    TextCurDir.Text := SelectDirectoryDialog1.FileName;
end;

end.

