unit WinIconSelector;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Controls, ButtonPanel, ComCtrls,
  Launcher, OriTabs, OriDialogs;

type

  { TWndIconSelector }

  TWndIconSelector = class(TOriDialog)
    ButtonPanel: TButtonPanel;
    IconTabs: TOriTabSet;
    ImagesPresetFileIcons: TImageList;
    ListBuiltinIcons: TListView;
    ListPresetFileIcons: TListView;
    procedure ListIconsDblClick(Sender: TObject);
  private
    FLauncher: TLauncher;
    procedure PresentIcon;
  protected
    procedure Populate; override;
    procedure Collect; override;
  public
    constructor Create(ALauncher: TLauncher); reintroduce;
  end;

function SelectIcon(ALauncher: TLauncher): boolean;

implementation

uses
  StrUtils, Graphics,
  LauncherIcons, IconPreset;

{$R *.lfm}

resourcestring
  SIconNone = '(none)';

const
  TabBuiltinIcon = 0;
  TabPresetFileIcon = 1;

function SelectIcon(ALauncher: TLauncher): boolean;
begin
  Result := TWndIconSelector.Create(ALauncher).Exec;
end;

constructor TWndIconSelector.Create(ALauncher: TLauncher);
begin
  inherited Create;
  FLauncher := ALauncher;
  Caption := Format('%s - %s', [Caption, FLauncher.Title]);
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

procedure TWndIconSelector.Populate;

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
end;

procedure TWndIconSelector.ListIconsDblClick(Sender: TObject);
var List: TListView;
begin
  List := Sender as TListView;
  if Assigned(List) and Assigned(List.Selected) then ButtonPanel.OKButton.Click;
end;

procedure TWndIconSelector.Collect;

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
