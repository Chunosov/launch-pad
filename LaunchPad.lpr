program LaunchPad;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, WinMain, Launcher, LauncherExe, WinLauncherExeProps, CategoryView,
  CommonData, WinIconSelector, WinCategoryVisibility, launchericons, IconPreset,
  WinLauncherBatProps, pascalscript, WinScriptEditor, LauncherScript;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TCommonDat, CommonDat);
  Application.CreateForm(TWndMain, WndMain);
  Application.Run;
end.

