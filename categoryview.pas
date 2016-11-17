unit CategoryView;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Buttons, Menus, FGL,
  Launcher;

type
  TLauncherView = class(TSpeedButton)
  private
    FLauncher: TLauncher;
  public
    constructor Create(AOwner: TComponent; ALauncher: TLauncher); reintroduce;
    procedure Click; override;
    procedure Populate;
    property Launcher: TLauncher read FLauncher;
  end;

  TCategoryItems = specialize TFPGList<TLauncherView>;

  TCategoryView = class(TScrollBox)
  private
    FCategory: TLauncherCategory;
    FItems: TCategoryItems;
    FItemMenu: TPopupMenu;
    function GetItem(ALauncher: TLauncher): TLauncherView;
    procedure SetItemMenu(Menu: TPopupMenu);
    procedure Arrange;
  public
    constructor Create(AOwner: TComponent; ACategory: TLauncherCategory); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure Populate;
    procedure AddItem(ALauncher: TLauncher);
    procedure DeleteItem(ALauncher: TLauncher);
    procedure UpdateItem(ALauncher: TLauncher);
    procedure MoveItemUp(ALauncher: TLauncher);
    procedure MoveItemDown(ALauncher: TLauncher);
    property Category: TLauncherCategory read FCategory;
    property ItemMenu: TPopupMenu read FItemMenu write SetItemMenu;
  end;

procedure LaunchWithErrorHandler(ALauncher: TLauncher);

implementation

uses
  SysUtils, Controls, Dialogs;

procedure LaunchWithErrorHandler(ALauncher: TLauncher);
begin
  try
    ALauncher.Launch;
  except
    on e: Exception do
      MessageDlg(ALauncher.TypeTitle, e.Message, mtError, [mbOK], 0);
  end;
end;

{%region TLauncherView}
constructor TLauncherView.Create(AOwner: TComponent; ALauncher: TLauncher);
begin
  inherited Create(AOwner);
  FLauncher := ALauncher;
  ShowHint := True;
  Align := alTop;
  Flat := True;
  Margin := 6;
  Height := 32;
  Populate;
end;

procedure TLauncherView.Click;
begin
  LaunchWithErrorHandler(FLauncher);
end;

procedure TLauncherView.Populate;
begin
  Caption := FLauncher.Title;
  if Assigned(FLauncher.Icon) then
    FLauncher.Icon.GetBitmap(Glyph)
  else Glyph.Clear;
end;

{%endregion}

{%region TCategoryView}
constructor TCategoryView.Create(AOwner: TComponent; ACategory: TLauncherCategory);
begin
  inherited Create(AOwner);
  FCategory := ACategory;
  BorderStyle := bsNone;
  Align := alClient;
  FItems := TCategoryItems.Create;
  Populate;
end;

destructor TCategoryView.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

procedure TCategoryView.Clear;
var
  I: integer;
begin
  for I := FItems.Count - 1 downto 0 do
    FItems[I].Free;
  FItems.Clear;
end;

procedure TCategoryView.Populate;
var
  L: TLauncher;
begin
  for L in FCategory.Launchers do
    if not L.Hidden then
      AddItem(L);
end;

procedure TCategoryView.AddItem(ALauncher: TLauncher);
var
  Item: TLauncherView;
begin
  Item := TLauncherView.Create(Self, ALauncher);
  Item.PopupMenu := FItemMenu;
  Item.Parent := Self;
  FItems.Add(Item);
  Arrange;
end;

procedure TCategoryView.UpdateItem(ALauncher: TLauncher);
var
  Item: TLauncherView;
begin
  Item := GetItem(ALauncher);
  if Assigned(Item) then
    Item.Populate;
end;

procedure TCategoryView.DeleteItem(ALauncher: TLauncher);
var
  Item: TLauncherView;
begin
  Item := GetItem(ALauncher);
  if Assigned(Item) then
  begin
    FItems.Remove(Item);
    Item.Free;
  end;
end;

function TCategoryView.GetItem(ALauncher: TLauncher): TLauncherView;
begin
  for Result in FItems do
    if Result.Launcher = ALauncher then
      Exit;
  Result := nil;
end;

procedure TCategoryView.SetItemMenu(Menu: TPopupMenu);
var
  Item: TLauncherView;
begin
  FItemMenu := Menu;
  for Item in FItems do
    Item.PopupMenu := Menu;
end;

procedure TCategoryView.MoveItemUp(ALauncher: TLauncher);
var
  I: integer;
  Item: TLauncherView;
begin
  for I := 0 to FItems.Count - 1 do
    if Fitems[I].Launcher = ALauncher then
    begin
      Item := FItems[I];
      FItems.Delete(I);
      FCategory.Launchers.Delete(I);
      if I = 0 then
      begin
        FItems.Add(Item);
        FCategory.Launchers.Add(ALauncher);
      end
      else
      begin
        FItems.Insert(I - 1, Item);
        FCategory.Launchers.Insert(I - 1, ALauncher);
      end;
      Arrange;
      Break;
    end;
end;

procedure TCategoryView.MoveItemDown(ALauncher: TLauncher);
var
  I: integer;
  Item: TLauncherView;
begin
  for I := 0 to FItems.Count - 1 do
    if Fitems[I].Launcher = ALauncher then
    begin
      Item := FItems[I];
      FItems.Delete(I);
      FCategory.Launchers.Delete(I);
      if I = FItems.Count then
      begin
        FItems.Insert(0, Item);
        FCategory.Launchers.Insert(0, ALauncher);
      end
      else
      begin
        FItems.Insert(I + 1, Item);
        FCategory.Launchers.Insert(I + 1, ALauncher);
      end;
      Arrange;
      Break;
    end;
end;

procedure TCategoryView.Arrange;
var
  Item: TLauncherView;
  Pos: integer = 0;
begin
  DisableAlign;
  try
    for Item in FItems do
    begin
      Item.Top := Pos + 1;
      Inc(Pos, Item.Height);
    end;
  finally
    EnableAlign;
    ReAlign;
  end;
end;

{%endregion}


end.
