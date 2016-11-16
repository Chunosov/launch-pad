unit WinCategoryProps;

{$mode objfpc}{$H+}

interface

uses
  ButtonPanel, StdCtrls, CheckLst,
  OriDialogs,
  Launcher;

type
  TWndCategoryProps = class(TOriDialog)
    ButtonPanel: TButtonPanel;
		Label1: TLabel;
		Label2: TLabel;
		ListItems: TCheckListBox;
		TextTitle: TEdit;
  private
    FCategory: TLauncherCategory;
  protected
    procedure Populate; override;
    procedure Collect; override;
  public
    constructor Create(ACategory: TLauncherCategory); reintroduce;
  end;

implementation

uses
  SysUtils;

{$R *.lfm}

constructor TWndCategoryProps.Create(ACategory: TLauncherCategory);
begin
  inherited Create;
  FCategory := ACategory;
end;

procedure TWndCategoryProps.Populate;
var
  I: Integer;
  L: TLauncher;
begin
  TextTitle.Text := FCategory.Title;

  for I := 0 to FCategory.Launchers.Count-1 do
  begin
    L := FCategory.Launchers[I];
    ListItems.AddItem(L.Title, L);
    ListItems.Checked[I] := not L.Hidden;
	end;
end;

procedure TWndCategoryProps.Collect;
var
  I: Integer;
  L: TLauncher;
begin
  FCategory.Title := Trim(TextTitle.Text);

  for I := 0 to ListItems.Count-1 do
  begin
    L := TLauncher(ListItems.Items.Objects[I]);
    L.Hidden := not ListItems.Checked[I];
  end;
end;

end.

