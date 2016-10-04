program fpc_zyre_test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, TestCase1, guitestrunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

