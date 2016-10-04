unit guitestrunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs
  , zyre_library
  , zyre
  , zyre_classes
  , zyre_node
  , zyre_group
  , zyre_peer
  , zyre_msg
  , zyre_event
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

