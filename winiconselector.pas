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
    procedure PresentIcon;
    procedure ApplyIcon;
  end;

function SelectIcon(ALauncher: TLauncher): boolean;

implementation

uses
  StrUtils,
  OriUtils_Gui,
  LauncherIcons;

{$R *.lfm}

resourcestring
  SIconNone = '(none)';

var
  SavedSize, SavedPos: longword;

function SelectIcon(ALauncher: TLauncher): boolean;
begin
  with TWndIconSelector.Create(Application.MainForm) do
  begin
    FLauncher := ALauncher;
    Result := ShowModal = mrOk;
    if Result then ApplyIcon;
  end;
end;

function GetImages: TImageList;
begin
  Result := CommonData.CommonDat.ImagesBig;
end;

procedure TWndIconSelector.PresentIcon;

  procedure PresentBuiltinIcon(Icon: TBuiltinLauncherIcon);
  var I: Integer;
  begin
    for I := 0 to ListBuiltinIcons.Items.Count-1 do
      if ListBuiltinIcons.Items[i].ImageIndex = Icon.Index then
      begin
        ListBuiltinIcons.Selected := ListBuiltinIcons.Items[i];
        break;
      end;
  end;

begin
  if Assigned(FLauncher.Icon) then
    if FLauncher.Icon is TBuiltinLauncherIcon then
      PresentBuiltinIcon(TBuiltinLauncherIcon(FLauncher.Icon));
end;

procedure TWndIconSelector.FormShow(Sender: TObject);

  procedure PopulateBuilinIcons;
  var
    I: integer;
    It: TListItem;
  begin
    ListBuiltinIcons.LargeImages := GetImages;
    for I := 0 to GetImages.Count - 1 do
    begin
      It := ListBuiltinIcons.Items.Add;
      It.Caption := IfThen(I = 0, SIconNone, IntToStr(I));
      It.ImageIndex := I;
    end;
  end;

begin
  PopulateBuilinIcons;
  PresentIcon;
  RestoreFormSizePos(Self, SavedSize, SavedPos);
end;

procedure TWndIconSelector.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  SaveFormSizePos(Self, SavedSize, SavedPos);
end;

procedure TWndIconSelector.ApplyIcon;

  function MakeBuiltinIcon: TBuiltinLauncherIcon;
  begin
    Result := TBuiltinLauncherIcon.Create;
    Result.Index := ListBuiltinIcons.Selected.ImageIndex;
  end;

begin
  if Assigned(ListBuiltinIcons.Selected) then
  begin
    FLauncher.ClearIcon;

    if ListBuiltinIcons.Selected.ImageIndex > 0 then
      FLauncher.Icon := MakeBuiltinIcon;
  end;
end;

end.
