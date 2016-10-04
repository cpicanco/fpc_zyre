unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry
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

  { TTestZyre }

  // http://wiki.freepascal.org/fpcunit

  TTestZyre= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure Test4;
    procedure Test5;
  end;

var GTestResult : TTestResult;

implementation

procedure TTestZyre.Test1;
begin
 Fail('Fail Test 1');
end;

procedure TTestZyre.Test2;
begin
  WriteLn('Fail test 2.');
end;

procedure TTestZyre.Test3;
begin
  WriteLn('Fail test 3.');
end;

procedure TTestZyre.Test4;
begin
  WriteLn('Fail test 4.');
end;

procedure TTestZyre.Test5;
begin
  WriteLn('Fail test 5.');
end;

procedure TTestZyre.SetUp;
begin

end;

procedure TTestZyre.TearDown;
begin
end;

initialization
  RegisterTest(TTestZyre);
  GTestResult := TTestResult.Create;
  try
    TTestSuite(GetTestRegistry).Run(GTestResult);
      WriteLn(IntToStr(GTestResult.RunTests));
  finally
    GTestResult.Free;
  end;
end.

