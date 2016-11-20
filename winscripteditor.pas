unit WinScriptEditor;

{$mode objfpc}{$H+}

interface

uses
  SynEdit, Controls, ButtonPanel, ExtCtrls, StdCtrls, EditBtn,
  OriTabs, OriDialogs, Launcher;

type

  { TWndScriptEditor }

  TWndScriptEditor = class(TOriDialog)
    ButtonPanel: TButtonPanel;
    CodeEditor: TSynEdit;
    FlagRunner: TCheckBox;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MemoCmdLine: TMemo;
    Tabs: TOriTabSet;
    PanelOptions: TPanel;
    TextCurDir: TDirectoryEdit;
    TextRunner: TFileNameEdit;
    TextTitle: TEdit;
  private
    FLauncher: TLauncher;
  public
    constructor Create(ALauncher: TLauncher); reintroduce;
  end;

implementation

{$R *.lfm}

constructor TWndScriptEditor.Create(ALauncher: TLauncher);
begin
  inherited Create;
  FLauncher := ALauncher;

  PanelOptions.Align := alClient;
end;

end.

