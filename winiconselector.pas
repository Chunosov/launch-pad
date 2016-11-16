unit WinIconSelector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ButtonPanel,
  ComCtrls, Launcher, OriTabs, CommonData;

type
  TWndIconSelector = class(TForm)
    ButtonPanel: TButtonPanel;
    CategoryTabs: TOriTabSet;
    ListBuiltinIcons: TListView;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FLauncher: TLauncher;
  end;

function SelectIcon(ALauncher: TLauncher): boolean;

implementation

uses
  StrUtils,
  OriUtils_Gui;

{$R *.lfm}

var
  SavedSize, SavedPos: longword;

function SelectIcon(ALauncher: TLauncher): boolean;
begin
  with TWndIconSelector.Create(Application.MainForm) do
  begin
    FLauncher := ALauncher;
    Result := ShowModal = mrOk;
  end;
end;

function GetImages: TImageList;
begin
  Result := CommonData.CommonDat.ImagesBig;
end;

procedure TWndIconSelector.FormShow(Sender: TObject);
var
  I: integer;
  It: TListItem;
begin
  ListBuiltinIcons.LargeImages := GetImages;
  for I := 0 to GetImages.Count - 1 do
  begin
    It := ListBuiltinIcons.Items.Add;
    It.Caption := IfThen(I = 0, '(none)', IntToStr(I));
    It.ImageIndex := I;
  end;

  RestoreFormSizePos(Self, SavedSize, SavedPos);
end;

procedure TWndIconSelector.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  SaveFormSizePos(Self, SavedSize, SavedPos);
end;

end.
