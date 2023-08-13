unit Centering;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

procedure Centering_Init;
procedure Centering_Reset;
procedure Centering_NewImage;
procedure Centering_UpdateTimers;
procedure Centering_TimeoutRA;
procedure Centering_TimeoutDEC;
procedure Centering_ButtonEvent;
procedure Centering_ForceCentering;

var
  centeringFlag: boolean;

implementation

uses Main, Run, global;

//******************************************************************************
// Init
//******************************************************************************
procedure Centering_Init;
begin
// place holder
end;

//******************************************************************************
// Reset
//******************************************************************************
procedure Centering_Reset;
begin
// init
  Centering_TimeoutRA;
  Centering_TimeoutDEC;

  RAp.accumDistanceMoved := 0.0;
  RAp.centDistanceMoved := 0.0;

  DECp.accumDistanceMoved := 0.0;
  DECp.centDistanceMoved := 0.0;

  centeringFlag := false;

  Main_Form.centeringRA_Edit.Font.color := clRed;
  Main_Form.centeringDEC_Edit.Font.color := clRed;

  if not ATrackRunning then exit;

// if centering function enabled then update indicators
  if (Main_Form.Centering_BCButton.down) then
  begin
    centeringFlag := true;
    Main_Form.centeringRA_Edit.Font.color := clLime;
    Main_Form.centeringDEC_Edit.Font.color := clLime;
  end;

end;

//******************************************************************************
// New image
//******************************************************************************
procedure Centering_NewImage;
var
  cr: double; // centering rate (arcsec/min)

  r: double;
  t: double;

begin
// init
  RAp.centDistanceMoved := RAp.accumDistanceMoved;
  RAp.accumDistanceMoved := 0.0;
  Centering_TimeoutRA;

  DECp.centDistanceMoved := DECp.accumDistanceMoved;
  DECp.accumDistanceMoved := 0.0;
  Centering_TimeoutDEC;

// centering rate (arcsec/min) --> arcsec/sec
  cr := StrToFloatDef(Main_Form.CenteringRate_MaskEdit.editText,0.0);
  if (cr <= 0) then exit;
  cr := cr / 60.0;

// RA
  r := abs(RAp.ImageShift);
  t := r / cr;
  if t > 300000.0 then t := 300000.0;
  RAp.centTime := round(t);

  if RAp.centTime > 0 then
  begin
    Main_Form.CenteringRA_Timer.Interval := RAp.centTime * 1000;
    Main_Form.CenteringRA_Timer.Enabled := true;
    RAp.cent := cr;
    if RAp.ImageShift < 0 then
      RAp.cent := -RAp.cent;
  end;

// DEC
  r := abs(DECp.ImageShift);
  t := r / cr;
  if t > 300000.0 then t := 300000.0;
  DECp.centTime := round(t);
  if DECp.centTime > 0 then
  begin
    Main_Form.CenteringDEC_Timer.Interval := DECp.centTime * 1000;
    Main_Form.CenteringDEC_Timer.Enabled := true;
    DECp.cent := cr;
    if DECp.ImageShift > 0 then
      DECp.cent := -DECp.cent;
  end;

end;

//******************************************************************************
// Centering timeout
//******************************************************************************
procedure Centering_TimeoutRA;
begin
// turn off timer
  Main_Form.CenteringRA_Timer.Enabled := false;

// reset rates
  RAp.cent:= 0.0;
  RAp.centTime := 0;

// update tracking correction
  Run_UpdateTrackingRates;
  Run_UpdateIndicators;

end;

procedure Centering_TimeoutDEC;
begin
// turn off timer
  Main_Form.CenteringDEC_Timer.Enabled := false;

// reset rates
  DECp.cent:= 0.0;
  DECp.centTime := 0;

// update tracking correction
  Run_UpdateTrackingRates;
  Run_UpdateIndicators;

end;

procedure Centering_UpdateTimers;
begin
// RA
  if RAp.centTime > 0 then
  begin
    RAp.accumDistanceMoved := RAp.accumDistanceMoved - RAp.aCent;
    dec(RAp.centTime);
    Run_UpdateIndicators;
  end;

  if DECp.centTime > 0 then
  begin
    DECp.accumDistanceMoved := DECp.accumDistanceMoved + DECp.aCent;
    dec(DECp.centTime);
    Run_UpdateIndicators;
  end;

end;

procedure Centering_ButtonEvent;
begin
  if (Main_Form.Centering_BCButton.down) and
     ATrackRunning then
  begin
    centeringFlag := true;
    Main_Form.centeringRA_Edit.Font.color := clLime;
    Main_Form.centeringDEC_Edit.Font.color := clLime;
  end
  else
  begin
    Centering_TimeoutRA;
    Centering_TimeoutDEC;
    RAp.centTime := 0;
    DECp.centTime := 0;
    centeringFlag := false;
  end;

end;

procedure Centering_ForceCentering;
var
  n: integer;
  r,cr: real;

begin
// ignore if not running
  if not ATrackRunning then exit;

// centering time
  n := StrToIntDef(Main_Form.ForceCenteringTime_MaskEdit.editText,0);
  if n = 0 then exit;

// init
  RAp.centDistanceMoved := RAp.accumDistanceMoved;
  RAp.accumDistanceMoved := 0.0;
  Centering_TimeoutRA;

  DECp.centDistanceMoved := DECp.accumDistanceMoved;
  DECp.accumDistanceMoved := 0.0;
  Centering_TimeoutDEC;

// RA
  r := abs(RAp.ImageShift);
  if r > 0.0 then
  begin
    cr := r / n;
    RAp.centTime := n;
    Main_Form.CenteringRA_Timer.Interval := RAp.centTime * 1000;
    Main_Form.CenteringRA_Timer.Enabled := true;
    RAp.cent := cr;
    if RAp.ImageShift < 0 then
      RAp.cent := -RAp.cent;
  end;

// DEC
  r := abs(DECp.ImageShift);
  if r > 0.0 then
  begin
    cr := r / n;
    DECp.centTime := n;
    Main_Form.CenteringDEC_Timer.Interval := DECp.centTime * 1000;
    Main_Form.CenteringDEC_Timer.Enabled := true;
    DECp.cent := cr;
    if DECp.ImageShift > 0 then
      DECp.cent := -DECp.cent;
  end;

// update mount
  Run_UpdateTrackingRates;

end;

end.

