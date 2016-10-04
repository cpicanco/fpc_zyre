unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TTestZyre= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure TTestZyre.TestHookUp;
begin
  Fail('Write your own test');
end;

procedure TTestZyre.SetUp;
begin

end;

procedure TTestZyre.TearDown;
begin

end;

initialization

  RegisterTest(TTestZyre);
end.

