unit LauncherIcons;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Graphics, OriXmlFile, Launcher;

type
  TBuiltinLauncherIcon = class(TLauncherIcon)
  private
    FIndex: Integer;
  public
    procedure SaveXml(Xml: TOriXmlFileWriter); override;
    procedure LoadXml(Xml: TOriXmlFileReader); override;
    procedure GetBitmap(Bmp: TCustomBitmap); override;
    property Index: Integer read FIndex write FIndex;
  end;

  TPresetFileLauncherIcon = class(TLauncherIcon)
  private
    FFileName: String;
  public
    procedure SaveXml(Xml: TOriXmlFileWriter); override;
    procedure LoadXml(Xml: TOriXmlFileReader); override;
    procedure GetBitmap(Bmp: TCustomBitmap); override;
    property FileName: String read FFileName write FFileName;
  end;

implementation

uses
  CommonData, IconPreset;

{%region TBuiltinLauncherIcon}
procedure TBuiltinLauncherIcon.SaveXml(Xml: TOriXmlFileWriter);
begin
  Xml.Text['Index'] := IntToStr(FIndex);
end;

procedure TBuiltinLauncherIcon.LoadXml(Xml: TOriXmlFileReader);
begin
  if not TryStrToInt(Xml.Text['Index'], FIndex) then FIndex := -1;
end;

procedure TBuiltinLauncherIcon.GetBitmap(Bmp: TCustomBitmap);
begin
  if Assigned(Bmp) then
    if (FIndex >= 0) and (FIndex < CommonDat.ImagesBig.Count) then
      CommonDat.ImagesBig.GetBitmap(FIndex, Bmp)
    else Bmp.Clear;
end;
{%endregion}

{%region TPresetFileLauncherIcon}
procedure TPresetFileLauncherIcon.SaveXml(Xml: TOriXmlFileWriter);
begin
  Xml.Text['FileName'] := FFileName;
end;

procedure TPresetFileLauncherIcon.LoadXml(Xml: TOriXmlFileReader);
begin
  FFileName := Xml.Text['FileName'];
end;

procedure TPresetFileLauncherIcon.GetBitmap(Bmp: TCustomBitmap);
var
  Picture: TPicture;
begin
  Picture := IconPreset.LoadIcon(FFileName);
  if Assigned(Picture) then
  try
    Bmp.Assign(Picture.Bitmap);
  finally
    Picture.Free;
  end;
end;
{%endregion}

initialization
  LauncherIconTypes.Add(TBuiltinLauncherIcon);
  LauncherIconTypes.Add(TPresetFileLauncherIcon);

end.

