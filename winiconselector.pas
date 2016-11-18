unit WinIconSelector;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Controls, ButtonPanel, ComCtrls,
  Launcher, OriTabs;

type

  { TWndIconSelector }

  TWndIconSelector = class(TForm)
    ButtonPanel: TButtonPanel;
    IconTabs: TOriTabSet;
    ImagesPresetFileIcons: TImageList;
    ListBuiltinIcons: TListView;
    ListPresetFileIcons: TListView;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ListIconsDblClick(Sender: TObject);
  private
    FLauncher: TLauncher;
    procedure PresentIcon;
    procedure ApplyIcon;
  end;

function SelectIcon(ALauncher: TLauncher): boolean;

implementation

uses
  StrUtils, Graphics,
  OriUtils_Gui,
  LauncherIcons, IconPreset;

{$R *.lfm}

resourcestring
  SIconNone = '(none)';

var
  SavedSize, SavedPos: longword;

const
  TabBuiltinIcon = 0;
  TabPresetFileIcon = 1;

function SelectIcon(ALauncher: TLauncher): boolean;
begin
  with TWndIconSelector.Create(Application.MainForm) do
  begin
    Caption := Format('%s - %s', [Caption, ALauncher.Title]);
    FLauncher := ALauncher;
    Result := ShowModal = mrOk;
    if Result then ApplyIcon;
  end;
end;

procedure TWndIconSelector.PresentIcon;

  type TPredicate = function(it: TListItem): Boolean is nested;

  procedure SelectItem(List: TListView; ShouldSelect: TPredicate);
  var I: Integer;
  begin
    for I := 0 to List.Items.Count-1 do
      if ShouldSelect(List.Items[i]) then
      begin
        List.ItemIndex := I;
        break;
      end;
  end;

  procedure Present(Icon: TBuiltinLauncherIcon);
    function CanSelect(it: TListItem): Boolean;
    begin
      Result := it.ImageIndex = Icon.Index;
    end;
  begin
    IconTabs.ActiveTabIndex := TabBuiltinIcon;
    SelectItem(ListBuiltinIcons, @CanSelect);
  end;

  procedure Present(Icon: TPresetFileLauncherIcon);
    function CanSelect(it: TListItem): Boolean;
    begin
      Result := it.Caption = Icon.FileName;
    end;
  begin
    IconTabs.ActiveTabIndex := TabPresetFileIcon;
    SelectItem(ListPresetFileIcons, @CanSelect);
  end;

begin
  if Assigned(FLauncher.Icon) then
  begin
    if FLauncher.Icon is TBuiltinLauncherIcon then
      Present(TBuiltinLauncherIcon(FLauncher.Icon))
    else if FLauncher.Icon is TPresetFileLauncherIcon then
      Present(TPresetFileLauncherIcon(FLauncher.Icon))
  end;
end;

procedure TWndIconSelector.FormShow(Sender: TObject);

  procedure PopulateBuilinIcons;
  var
    I: integer;
    It: TListItem;
  begin
    for I := 0 to ListBuiltinIcons.LargeImages.Count - 1 do
    begin
      It := ListBuiltinIcons.Items.Add;
      It.Caption := IfThen(I = 0, SIconNone, IntToStr(I));
      It.ImageIndex := I;
    end;
  end;

  procedure PopulatePresetFileIcons;
  var
    FileName: String;
    FileNames: TStringList;
    It: TListItem;

    function LoadIcon: Boolean;
    var
      PrevCount: Integer;
      Picture: TPicture;
    begin
      Result := False;
      Picture := IconPreset.LoadIcon(FileName, True);
      if Assigned(Picture) then
      try
        PrevCount := ImagesPresetFileIcons.Count;
        ImagesPresetFileIcons.Add(Picture.Bitmap, nil);
        if ImagesPresetFileIcons.Count > PrevCount then
          Result := True;
      finally
        Picture.Free;
      end;
    end;

  begin
    FileNames := IconPreset.GetFileNames;
    if Assigned(FileNames) then
    try
      for FileName in FileNames do
        if LoadIcon then
        begin
          It := ListPresetFileIcons.Items.Add;
          It.Caption := ExtractFileName(FileName);
          It.ImageIndex := ImagesPresetFileIcons.Count-1;
        end;
    finally
      FileNames.Free;
    end;
  end;

begin
  PopulateBuilinIcons;
  PopulatePresetFileIcons;
  PresentIcon;
  RestoreFormSizePos(Self, SavedSize, SavedPos);
end;

procedure TWndIconSelector.ListIconsDblClick(Sender: TObject);
var List: TListView;
begin
  List := Sender as TListView;
  if Assigned(List) and Assigned(List.Selected) then ButtonPanel.OKButton.Click;
end;

procedure TWndIconSelector.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  SaveFormSizePos(Self, SavedSize, SavedPos);
end;

procedure TWndIconSelector.ApplyIcon;

  function MakeBuiltinIcon: TBuiltinLauncherIcon;
  begin
    if not Assigned(ListBuiltinIcons.Selected) then exit(nil);
    if ListBuiltinIcons.Selected.ImageIndex < 1 then exit(nil);
    Result := TBuiltinLauncherIcon.Create;
    Result.Index := ListBuiltinIcons.Selected.ImageIndex;
  end;

  function MakePresetFileIcon: TPresetFileLauncherIcon;
  begin
    if not Assigned(ListPresetFileIcons.Selected) then exit(nil);
    Result := TPresetFileLauncherIcon.Create;
    Result.FileName := ListPresetFileIcons.Selected.Caption;
  end;

begin
  FLauncher.ClearIcon;

  case IconTabs.ActiveTabIndex of
    TabBuiltinIcon: FLauncher.Icon := MakeBuiltinIcon;
    TabPresetFileIcon: FLauncher.Icon := MakePresetFileIcon;
  end;
end;

end.
