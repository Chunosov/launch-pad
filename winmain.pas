unit WinMain;

{$mode objfpc}{$H+}

interface

uses
  Forms, Menus, ActnList, Classes,
  OriTabs,
  Launcher, categoryview;

type

	{ TWndMain }

  TWndMain = class(TForm)
	  ActionCategoryVisibility: TAction;
		ActionLauncherIcon: TAction;
	  ActionLauncherMoveDown: TAction;
		ActionLauncherMoveUp: TAction;
		ActionLauncherRun: TAction;
    ActionLauncherDelete: TAction;
    ActionLauncherConfig: TAction;
    ActionAppSettings: TAction;
    ActionCategoryDelete: TAction;
    ActionCategoryProps: TAction;
    ActionCategoryAdd: TAction;
    ActionList1: TActionList;
    MenuAddLauncher1: TMenuItem;
    MenuItem1: TMenuItem;
		MenuItem10: TMenuItem;
		MenuItem11: TMenuItem;
		MenuItem12: TMenuItem;
		MenuItem13: TMenuItem;
		MenuItem14: TMenuItem;
		MenuItem15: TMenuItem;
		MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    CategoryTabs: TOriTabSet;
		MenuAddLauncher2: TMenuItem;
		MenuItem8: TMenuItem;
		MenuItem9: TMenuItem;
		PopupMenuLaunchers: TPopupMenu;
    PopupMenuLauncher: TPopupMenu;
    PopupMenuCategories: TPopupMenu;
    procedure ActionCategoryAddExecute(Sender: TObject);
    procedure ActionCategoryDeleteExecute(Sender: TObject);
    procedure ActionCategoryPropsExecute(Sender: TObject);
		procedure ActionCategoryVisibilityExecute(Sender: TObject);
    procedure ActionLauncherDeleteExecute(Sender: TObject);
		procedure ActionLauncherIconExecute(Sender: TObject);
		procedure ActionLauncherMoveDownExecute(Sender: TObject);
    procedure ActionLauncherConfigExecute(Sender: TObject);
		procedure ActionLauncherRunExecute(Sender: TObject);
		procedure ActionLauncherMoveUpExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FBank: TLaunchersBank;
    procedure PopulateLauncherTypesMenu;
    procedure CreateView; overload;
    procedure CreateView(ACategory: TLauncherCategory; AActivate: Boolean = False); overload;
    procedure AddNewLauncherMenuItemClicked(Sender: TObject);
    function CurCategory: TLauncherCategory;
    function CurCategoryView: TCategoryView;
    function CurLauncher: TLauncher;
  end;

var
  WndMain: TWndMain;

implementation

uses
  SysUtils, Controls, Dialogs, LazFileUtils,
  WinCategoryProps, WinIconSelector, WinCategoryVisibility,
  OriUtils_Gui, OriIniFile, OriDialogs;

resourcestring
  SConfirmLauncherDeletion = 'Delete launcher "%s"?';
  SConfirmCategoryDeletion = 'Delete category "%s"?';
  SNewCategoryCaption = 'Create Category';
  SEditCategoryPrompt = 'New cateory title:';
  SRestartForCategoriesVibility = 'Restart application to apply visibility of categories';

{$R *.lfm}

function DefaultBankFile: String;
begin
  Result := AppendPathDelim(ExtractFilePath(ParamStr(0))) + DefBankFileName;
end;

procedure TWndMain.FormCreate(Sender: TObject);
begin
  PopulateLauncherTypesMenu;

  FBank := TLaunchersBank.Create(DefaultBankFile);

  CreateView;

  with TOriIniFile.Create do
  try
    CategoryTabs.ActiveTabIndex := ReadInteger('CurrentTab', 0);
    if CategoryTabs.ActiveTabIndex < 0 then CategoryTabs.ActiveTabIndex := 0;

    ReadFormSizePos(Self);
  finally
    Free;
  end;
end;

procedure TWndMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  with TOriIniFile.Create do
  try
    WriteInteger('CurrentTab', CategoryTabs.ActiveTabIndex);
    WriteFormSizePos(Self);
  finally
    Free;
  end;

  FBank.Free;
  CloseAction := caFree; // warning suppress
end;

{%region Category view methods}
procedure TWndMain.CreateView;
var C: TLauncherCategory;
begin
  for C in FBank.Categories do
    if not C.Hidden then CreateView(C);
end;

procedure TWndMain.CreateView(ACategory: TLauncherCategory; AActivate: Boolean);
var
  Tab: TOriTab;
  List: TCategoryView;
begin
  List := TCategoryView.Create(Self, ACategory);
  List.Parent := Self;
  List.Align := alClient;
  List.PopupMenu := PopupMenuLaunchers;
  List.ItemMenu := PopupMenuLauncher;
  List.Visible := False;
  Tab := CategoryTabs.Tabs.Add;
  Tab.Caption := ACategory.Title;
  Tab.Control := List;
  if AActivate then CategoryTabs.ActiveTab := Tab;
end;

{%endregion}

{%region Category Actions}
procedure TWndMain.ActionCategoryAddExecute(Sender: TObject);
var
  Title: String = '';
  Category: TLauncherCategory;
begin
  if InputQuery(SNewCategoryCaption, SEditCategoryPrompt, Title) then
  begin
    Category := TLauncherCategory.Create;
    Category.Title := Title;
    FBank.Categories.Add(Category);
    FBank.Save;
    CreateView(Category, True);
  end;
end;

procedure TWndMain.ActionCategoryDeleteExecute(Sender: TObject);
var
  Category: TLauncherCategory;
begin
  Category := CurCategory;
  if not Assigned(Category) then exit;
  if ConfirmDlg(SConfirmCategoryDeletion, [Category.Title]) then
  begin
    CategoryTabs.Tabs.Delete(CategoryTabs.ActiveTabIndex);
    CategoryTabs.ActiveTabIndex := 0;
    FBank.Categories.Remove(Category);
    FBank.Save;
    Category.Free;
  end;
end;

procedure TWndMain.ActionCategoryPropsExecute(Sender: TObject);
begin
  if not Assigned(CurCategory) then exit;
  if TWndCategoryProps.Create(CurCategory).Exec then
  begin
    FBank.Save;
    CategoryTabs.ActiveTab.Caption := CurCategory.Title;
    CurCategoryView.Clear;
    CurCategoryView.Populate;
	end;
end;

procedure TWndMain.ActionCategoryVisibilityExecute(Sender: TObject);
begin
  if TWndCategoryVisibility.Create(FBank).Exec then
  begin
    FBank.Save;
    MessageDlg(SRestartForCategoriesVibility);
  end;
end;

{%endregion}

{%region Launchers Actions}
procedure TWndMain.AddNewLauncherMenuItemClicked(Sender: TObject);
var
  Launcher: TLauncher;
begin
  if not Assigned(CurCategory) then exit;
  Launcher := LauncherTypes[TComponent(Sender).Tag].Create;
  if Launcher.Configure then
  begin
    CurCategory.Launchers.Add(Launcher);
    CurCategoryView.AddItem(Launcher);
    FBank.Save;
  end
  else Launcher.Free;
end;

procedure TWndMain.ActionLauncherDeleteExecute(Sender: TObject);
var
  Launcher: TLauncher;
begin
  Launcher := CurLauncher;
  if Assigned(Launcher) then
    if ConfirmDlg(SConfirmLauncherDeletion, [Launcher.Title]) then
    begin
      CurCategoryView.DeleteItem(Launcher);
      CurCategory.Launchers.Remove(Launcher);
      Launcher.Free;
      FBank.Save;
    end;
end;

procedure TWndMain.ActionLauncherIconExecute(Sender: TObject);
var
  Launcher: TLauncher;
begin
  Launcher := CurLauncher;
  if Assigned(Launcher) then
    if WinIconSelector.SelectIcon(Launcher) then
    begin
      CurCategoryView.UpdateItem(Launcher);
      FBank.Save;
    end;
end;

procedure TWndMain.ActionLauncherConfigExecute(Sender: TObject);
var
  Launcher: TLauncher;
begin
  Launcher := CurLauncher;
  if Assigned(Launcher) then
    if Launcher.Configure then
    begin
      CurCategoryView.UpdateItem(Launcher);
      FBank.Save;
    end;
end;

procedure TWndMain.ActionLauncherMoveDownExecute(Sender: TObject);
begin
  if Assigned(CurLauncher) then
  begin
    CurCategoryView.MoveItemDown(CurLauncher);
    FBank.Save;
	end;
end;

procedure TWndMain.ActionLauncherMoveUpExecute(Sender: TObject);
begin
  if Assigned(CurLauncher) then
  begin
    CurCategoryView.MoveItemUp(CurLauncher);
    FBank.Save;
	end;
end;

procedure TWndMain.ActionLauncherRunExecute(Sender: TObject);
begin
  if Assigned(CurLauncher) then LaunchWithErrorHandler(CurLauncher);
end;
{%endregion}

procedure TWndMain.PopulateLauncherTypesMenu;
var
  Index: Integer;

  function MakeMenuItem: TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Tag := Index;
    Result.Caption := LauncherTypes[Index].TypeTitle;
    Result.OnClick := @AddNewLauncherMenuItemClicked;
  end;

begin
  for Index := 0 to LauncherTypes.Count-1 do
  begin
    MenuAddLauncher1.Add(MakeMenuItem);
    MenuAddLauncher2.Add(MakeMenuItem);
  end;
end;

function TWndMain.CurCategory: TLauncherCategory;
begin
  if Assigned(CategoryTabs.ActiveTab)
    then Result := TCategoryView(CategoryTabs.ActiveTab.Control).Category
    else Result := nil;
end;

function TWndMain.CurCategoryView: TCategoryView;
begin
  if Assigned(CategoryTabs.ActiveTab)
    then Result := TCategoryView(CategoryTabs.ActiveTab.Control)
    else Result := nil;
end;

function TWndMain.CurLauncher: TLauncher;
var
  LauncherView: TLauncherView;
begin
  LauncherView := PopupMenuLauncher.PopupComponent as TLauncherView;
  if Assigned(LauncherView)
    then Result := LauncherView.Launcher
    else Result := nil;
end;

end.

