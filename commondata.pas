unit CommonData;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls;

type
  TCommonDat = class(TDataModule)
    ImagesBig: TImageList;
  end;

var
  CommonDat: TCommonDat;

implementation

{$R *.lfm}

end.

