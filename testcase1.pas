unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry
  //, testutils
  , ctypes
  //, zyre_library
  , zyre
  , zyre_classes
  //, zyre_node
  //, zyre_group
  //, zyre_peer
  , zyre_msg
  //, zyre_event
  ;

type

  { TTestZyre }

  // http://wiki.freepascal.org/fpcunit

  TTestZyre= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Zyre;
    procedure Peer;
    procedure Group;
    procedure Node;
    procedure Message;
  end;

var
  GTestResult : TTestResult;
  GMajor,GMinor,GPatch : cint;

implementation

procedure TTestZyre.Zyre;
begin
  try
    WriteLn('zyre_test');
    zyre_test(True);
  except
    on E: Exception do
      Fail('zyre_library fail');
  end;
end;

procedure TTestZyre.Peer;
begin
  try
    WriteLn('zyre_peer_test');
    zyre_peer_test(True);
  except
    on E: Exception do
      Fail('zyre_peer fail');
  end;
end;

procedure TTestZyre.Group;
begin
  try
    WriteLn('zyre_group_test');
    zyre_group_test(True);
  except
    on E: Exception do
      Fail('zyre_group fail');
  end;
end;

procedure TTestZyre.Node;
begin
  try
    WriteLn('zyre_node_test');
    zyre_node_test(True);
  except
    on E: Exception do
      Fail('zyre_node fail');
  end;
end;

procedure TTestZyre.Message;
begin
  try
    WriteLn('zyre_msg');
    zre_msg_test(True);
  except
    on E: Exception do
      Fail('zyre_msg fail');
  end;
end;

procedure TTestZyre.SetUp;
begin
  Write('Starting test:'+#32);
end;

procedure TTestZyre.TearDown;
begin
  WriteLn(#10);
end;

initialization
  WriteLn('Testing zyre_version:',zyre_version);
  WriteLn(#10);

  RegisterTest(TTestZyre);
  GTestResult := TTestResult.Create;
  try
    TTestSuite(GetTestRegistry).Run(GTestResult);
    WriteLn(IntToStr(GTestResult.RunTests)+' tests runned.');
  finally
    GTestResult.Free;
  end;
end.

