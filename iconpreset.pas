unit IconPreset;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics;

function GetFileNames: TStringList;
function LoadIcon(const AFileName: String; AbsolutePath: Boolean = False): TPicture;

implementation

uses
  SysUtils, FileUtil, LazFileUtils,
  OriUtils_Gui;

const
  PresetFileIconsDir = 'icons';
  PresetFileIconsFilter = '*.png;*.bmp;*.xmp;*.ico';
  SUnableLoadPreset = 'Unable to load preset icon file "%s": %s';

function GetPresetFileIconsPath: String;
begin
  Result := AppendPathDelim(ExtractFilePath(ParamStr(0))) + PresetFileIconsDir;
end;

function GetFileNames: TStringList;
var
  Dir: String;
begin
  Dir := GetPresetFileIconsPath;
  if DirectoryExistsUTF8(Dir)
    then Result := FindAllFiles(Dir, PresetFileIconsFilter, False)
    else Result := nil;
end;

function GetFullPath(const AFileName: String): String;
begin
  Result := AppendPathDelim(GetPresetFileIconsPath) + AFileName;
end;

function LoadIcon(const AFileName: String; AbsolutePath: Boolean): TPicture;
var
  FullPath: String;
begin
  if AbsolutePath
    then FullPath := AFileName
    else FullPath := GetFullPath(AFileName);
  if not FileExistsUTF8(FullPath) then
    Exit(nil);

  Result := TPicture.Create;
  try
    Result.LoadFromFile(FullPath);
  except
    on e: Exception do
    begin
      ErrorDlg(SUnableLoadPreset, [FullPath, e.Message]);
      FreeAndNil(Result);
    end;
  end;
end;

end.

