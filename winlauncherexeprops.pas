unit WinLauncherExeProps;

{$mode objfpc}{$H+}

interface

uses
  ButtonPanel, StdCtrls, Dialogs, EditBtn,
  OriDialogs,
  LauncherExe, Classes;

type

  { TWndLauncherExeProps }

  TWndLauncherExeProps = class(TOriDialog)
    ButtonPanel: TButtonPanel;
    TextFileName: TFileNameEdit;
    TextCurDir: TDirectoryEdit;
    Label3: TLabel;
    Label6: TLabel;
    OpenDialog1: TOpenDialog;
    Label4: TLabel;
    Label5: TLabel;
    MemoCmdLine: TMemo;
    TextTitle: TEdit;
    procedure TextFileNameButtonClick(Sender: TObject);
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
  SysUtils, LazFileUtils;

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

procedure TWndLauncherExeProps.TextFileNameButtonClick(Sender: TObject);
begin
  TextFileName.Filter := SOpenDialogFilter;
  TextFileName.InitialDir := ExtractFilePath(Trim(TextFileName.Text));
end;

procedure TWndLauncherExeProps.Populate;
begin
  TextTitle.Text := FLauncher.Title;
  TextFileName.Text := FLauncher.FileName;
  TextCurDir.Text := FLauncher.CurDir;
  MemoCmdLine.Lines.Text := FLauncher.CmdLine;
end;

procedure TWndLauncherExeProps.Collect;
begin
  FLauncher.Title := Trim(TextTitle.Text);
  FLauncher.FileName := Trim(TextFileName.Text);
  FLauncher.CurDir := Trim(TextCurDir.Text);
  FLauncher.CmdLine := Trim(MemoCmdLine.Lines.Text);
end;

end.
