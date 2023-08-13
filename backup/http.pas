unit HTTP;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, strutils;

procedure HTTP_Open;
procedure HTTP_Close;

procedure HTTP_RxData(s:string);

procedure HTTP_Start(s:string);
procedure HTTP_Stop();

procedure HTTP_Log(s:string);


implementation

uses Main, Run, Config, Global;

procedure HTTP_Open;
begin
// start http server
  Main_Form.IdHTTPServer1.Active := True;
  HTTP_Log('Start HTTP server')

end;

procedure HTTP_Close;
begin
// stop http server
  Main_Form.IdHTTPServer1.Active := False;
  HTTP_Log('Stop HTTP server')

end;

// data: http://192.168.1.23/cmd/Target
procedure HTTP_RxData(s:string);
var
  sArray: array of string;
  theCMD, theTarget: string;

begin
// log
  HTTP_Log(s);

// convert string to tokens
  setlength(sArray,0);
  sArray := SplitString(s,'/');

// message type
  if length(sArray) = 0 then
  begin
    HTTP_Log('data invalid');
    exit;
  end;
  if not(sArray[0] = 'POST') then
  begin
    HTTP_Log('Invalid message type');
    exit;
  end;

// command
  if length(sArray) < 2 then
  begin
    HTTP_Log('Command not found');
    exit;
  end;
  theCMD := sArray[1];
  HTTP_Log('  Cmd: ' + theCMD);

// argument
  if length(sArray) > 2  then
  begin
    theTarget := sArray[2];
    HTTP_Log('  Target: ' + theTarget);
  end;

// START request
  if pos('START',theCMD) > 0 then
  begin
    HTTP_Start(theTarget);
    exit;
  end;

// STOP request
  if pos('STOP', theCMD) > 0 then
  begin
    HTTP_Stop();
    exit;
  end;

// unknown request
  HTTP_Log('ERROR: Unknown request: ' + theCMD);

end;

procedure HTTP_Start(s:string);
var
  t: string;
  r: double;
  thePath: string;

begin
// stop currently running target
  Run_Stop;

// no target
  if s = '' then exit;

// current profile
  t := Main_Form.Profile_Edit.text;
  HTTP_Log('Current profile: ' + t);

// already loaded
  if pos(s,t) = 0 then
  begin
// new profile
    thePath := ExtractFileDir(t) + '\' + s + '.prf';
    Main_Form.Profile_Edit.text := thePath;
    HTTP_Log('New profile path: ' + thePath);

// load profile
    HTTP_Log('Config Read: ' + thePath);
    Config_Read(thePath);
  end;

// set base tracking rate
  r := StrToFloatDef(Main_Form.BaseTrackingRateRA_MaskEdit.editText,0);
  RAp.base := r;

  r := StrToFloatDef(Main_Form.BaseTrackingRateDEC_MaskEdit.editText,0);
  DECp.base := r;

  Run_UpdateIndicators;
  Run_UpdateTrackingRates;

  HTTP_Log('HTTP Run Request set true');
  RunRequest := true;

end;

procedure HTTP_Stop();
begin
  StopRequest := true
end;

procedure HTTP_Log(s:string);
begin
  Main_Form.HTTP_Memo.Lines.Add(FormatDateTime('hh.mm.ss',now) + TAB + s);
end;

end.

