unit WinCategoryVisibility;

{$mode objfpc}{$H+}

interface

uses
  ButtonPanel, CheckLst,
  OriDialogs,
  Launcher;

type
  TWndCategoryVisibility = class(TOriDialog)
    ButtonPanel: TButtonPanel;
    ListCategories: TCheckListBox;
  private
    FBank: TLaunchersBank;
  protected
    procedure Populate; override;
    procedure Collect; override;
  public
    constructor Create(ABank: TLaunchersBank); reintroduce;
  end;

implementation

{$R *.lfm}

constructor TWndCategoryVisibility.Create(ABank: TLaunchersBank);
begin
  inherited Create;
  FBank := ABank;
end;

procedure TWndCategoryVisibility.Populate;
var
  I: Integer;
  C: TLauncherCategory;
begin
  for I := 0 to FBank.Categories.Count-1 do
  begin
    C := FBank.Categories[I];
    ListCategories.AddItem(C.Title, C);
    ListCategories.Checked[I] := not C.Hidden;
	end;
end;

procedure TWndCategoryVisibility.Collect;
var
  I: Integer;
  C: TLauncherCategory;
begin
  for I := 0 to ListCategories.Count-1 do
  begin
    C := TLauncherCategory(ListCategories.Items.Objects[I]);
    C.Hidden := not ListCategories.Checked[I];
  end;
end;

end.

