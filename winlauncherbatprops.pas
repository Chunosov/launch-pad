unit WinLauncherBatProps;

{$mode objfpc}{$H+}

interface

uses
  Classes, ButtonPanel, StdCtrls, Dialogs, Menus, EditBtn, SynEdit,
  SynHighlighterBat, OriDialogs, LauncherExe;

type

  { TWndLauncherBatProps }

  TWndLauncherBatProps = class(TOriDialog)
    ButtonPanel: TButtonPanel;
    TextCurDir: TDirectoryEdit;
    FontDialog: TFontDialog;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemEditorFont: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemUndo: TMenuItem;
    PopupMenuEditor: TPopupMenu;
    SynBatSyn1: TSynBatSyn;
    CodeEditor: TSynEdit;
    TextTitle: TEdit;
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemCutClick(Sender: TObject);
    procedure MenuItemEditorFontClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
  private
    FLauncher: TLauncherBat;
  protected
    procedure Populate; override;
    procedure Collect; override;
  public
    constructor Create(ALauncher: TLauncherBat); reintroduce;
  end;

implementation

uses
  SysUtils, LazFileUtils,
  OriUtils_Gui, OriIniFile;

{$R *.lfm}

resourcestring
  SUnableSaveScript = 'Unable to save commands into the file'#10#13'%s:'#10#13#10#13'%s';

constructor TWndLauncherBatProps.Create(ALauncher: TLauncherBat);
begin
  inherited Create;
  FLauncher := ALauncher;

  with TOriIniFile.Create do
  try
    ReadFont('BatEditorFont', CodeEditor.Font);
  finally
    Free;
  end;
end;

{%region Context menu edit commands}
procedure TWndLauncherBatProps.MenuItemUndoClick(Sender: TObject);
begin
  CodeEditor.Undo;
end;

procedure TWndLauncherBatProps.MenuItemCutClick(Sender: TObject);
begin
  CodeEditor.CutToClipboard;
end;

procedure TWndLauncherBatProps.MenuItemSelectAllClick(Sender: TObject);
begin
  CodeEditor.SelectAll;
end;

procedure TWndLauncherBatProps.MenuItemPasteClick(Sender: TObject);
begin
  CodeEditor.PasteFromClipboard;
end;

procedure TWndLauncherBatProps.MenuItemCopyClick(Sender: TObject);
begin
  CodeEditor.CopyToClipboard;
end;
{%endregion}

procedure TWndLauncherBatProps.MenuItemEditorFontClick(Sender: TObject);
begin
  FontDialog.Font := CodeEditor.Font;
  if FontDialog.Execute then
  begin
    CodeEditor.Font := FontDialog.Font;

    with TOriIniFile.Create do
    try
      WriteFont('BatEditorFont', CodeEditor.Font);
    finally
      Free;
    end;
  end;
end;

procedure TWndLauncherBatProps.Populate;

  procedure LoadScript(const Path: string);
  begin
    if FileExistsUTF8(Path) then
      CodeEditor.Lines.LoadFromFile(Path);
  end;

begin
  TextTitle.Text := FLauncher.Title;
  TextCurDir.Text := FLauncher.CurDir;
  LoadScript(FLauncher.GetExecutablePath);
end;

procedure TWndLauncherBatProps.Collect;

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
  SaveScript(FLauncher.GetExecutablePath);
end;

end.

