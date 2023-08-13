unit Network;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, strutils;

procedure Network_Open;
procedure Network_Close;

procedure Network_RxData(s:string);

procedure Network_Start();
procedure Network_Stop();

procedure Network_Log(s:string);


implementation

uses Main, Run, Config, Global;

var
  NetworkStartTarget: string;


procedure Network_Open;
begin
// start Network server
  NetworkCmdStart := false;
  NetworkCmdStop := false;
  Main_Form.IdUDPServer1.Active:=true;
  Network_Log('Network Started');
end;

procedure Network_Close;
begin
// stop Network server
  Main_Form.IdUDPServer1.Active:=false;
  NetworkCmdStart := false;
  NetworkCmdStop := false;
  Network_Log('Network Stopped');
end;

// UDP message
procedure Network_RxData(s:string);
var
  sArray: array of string;
  theCMD, theTarget: string;

begin
// log
  Network_Log('Msg received: [' + s + ']');
  NetworkCmdStart := false;
  NetworkCmdStop := false;

// convert string to tokens
  sArray := nil;
  setlength(sArray,0);
  sArray := SplitString(s,'|');

// message type
  if length(sArray) = 0 then
  begin
    Network_Log('data invalid');
    exit;
  end;

// command
  theCMD := sArray[0];
  Network_Log('  Cmd: <' + theCMD + '>');

// argument
  if length(sArray) > 1  then
  begin
    theTarget := sArray[1];
    Network_Log('  Target: <' + theTarget + '>');
  end;

// START request
  if theCMD = 'START' then
  begin
    NetworkCmdStart := true;
    NetworkStartTarget := theTarget;
    exit;
  end;

// STOP request
  if theCMD = 'STOP' then
  begin
    NetworkCmdStop := true;
    exit;
  end;

// unknown request
  Network_Log('ERROR: Unknown request: ' + theCMD);

end;

procedure Network_Start;
var
  t: string;
  r: double;
  thePath: string;
  s: string;

begin
// init
  NetworkCmdStart := false;
  NetworkCmdStop := false;

// log
  s := NetworkStartTarget;
  Network_Log('  Network Start: <' + s + '>');

// stop currently running target
  Run_Stop;

// no target
  if s = '' then
  begin
    Network_Log('  Error: no  target');
    exit;
  end;

// current profile
  t := Main_Form.Profile_Edit.text;
  Network_Log('  Current profile: <' + t + '>');

// load profile
  thePath := ExtractFileDir(t) + '\' + s + '.prf';
  Main_Form.Profile_Edit.text := thePath;
  Network_Log('  New profile path: <' + thePath + '>');
  Network_Log('  Config Read: <' + thePath + '>');
  Config_Read(thePath);

  Network_Log('  Run Start');
  Run_Start;

end;

procedure Network_Stop();
begin
  NetworkCmdStart := false;
  NetworkCmdStop := false;
  StopRequest := true
end;

procedure Network_Log(s:string);
begin
  Main_Form.NetworkLog_Memo.Lines.Add(FormatDateTime('hh.mm.ss',now) + TAB + s);
end;

end.

