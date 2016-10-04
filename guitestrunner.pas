unit guitestrunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs
  , TestCase1
  ;

type
  TGuiTestRunner = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TestRunner: TGuiTestRunner;

implementation

{$R *.lfm}

end.

