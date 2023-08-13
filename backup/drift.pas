unit Drift;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphics;

procedure Drift_Init;
procedure Drift_Reset;
procedure Drift_Reset2;
procedure Drift_NewImage;

var
  driftFlag: boolean;


implementation

uses Main, global;

//******************************************************************************
// Open
//******************************************************************************
procedure Drift_Init;
begin
// place holder
end;

//******************************************************************************
// Reset
//******************************************************************************
procedure Drift_Reset;
begin
// init
  RAp.drift := 0.0;
  DECp.drift := 0.0;
  driftFlag := false;

  Main_Form.driftCorrectionRA_Edit.Font.color := clRed;
  Main_Form.driftCorrectionDEC_Edit.Font.color := clRed;

  if not ATrackRunning then exit;

  if (Main_Form.DriftCorrection_BCButton.down) then
  begin
    driftFlag := true;
    Main_Form.driftCorrectionRA_Edit.Font.color := clLime;
    Main_Form.driftCorrectionDEC_Edit.Font.color := clLime;
  end;

end;
procedure Drift_Reset2;
begin
  RAp.drift := 0.0;
  DECp.drift := 0.0;
end;

//******************************************************************************
// New image
//******************************************************************************
procedure Drift_NewImage;
var
  r,r1: double;

begin
// first image
  if nSec = 0 then exit;

// RA drift
  // drift distance of the image - centering rate * centering interval
  r := RAp.ImageDrift - RAp.centDistanceMoved;

  // drift correction
  // a + means too slow and need to speed up (positive rate correction)
  // a - means too fast and need to slow down (negative rate correction)
  r := r / nSec;

  // limit
  r1 := StrToFloatDef(Main_Form.DriftCorrectionRateLimitRA_MaskEdit.EditText,0.00010);
  if abs(r) > r1 then
    if r < 0 then
      r := -r1
    else
      r := r1;

  // update drift rate
  RAp.drift := RAp.drift + r;


  // DEC drift
  r := DECp.ImageDrift - DECp.centDistanceMoved;

  // drift correction
  // a + means too slow and need to speed up (positive rate correction)
  // a - means too fast and need to slow down (negative rate correction)
  r := -r / nSec;

  // limit
  r1 := StrToFloatDef(Main_Form.DriftCorrectionRateLimitDEC_MaskEdit.EditText,0.00100);
  if abs(r) > r1 then
  if r < 0 then
    r := -r1
  else
    r := r1;

// update drift rate
  DECp.drift := DECp.drift + r;

end;

end.

