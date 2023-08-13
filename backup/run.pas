unit Run;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, Classes, SysUtils, Global, Dialogs, Graphics;

procedure Run_Open;
procedure Run_Start;
procedure Run_Stop;
procedure Run_NewImage;
procedure Run_UpdateElapsedTime;
procedure Run_UpdateIndicators;
procedure Run_UpdateTrackingRates;
procedure Run_UpdateLog;
procedure Run_Header;
procedure Run_PSHeader;
procedure Run_SetBase;

var
  runFlag: boolean;
  skipNImagesCounter: integer;

implementation

uses Main, Centering, Drift, Mount, Profiles, PID;

procedure ResetSessionVariables; forward;
procedure AddToGrid(s: string); forward;
procedure SetIndicatorsColor; forward;
procedure UpdateStatus; forward;
procedure ResetIndicators; forward;

//******************************************************************************
// Init
//******************************************************************************
procedure Run_Open;
begin
// init log
  Main_Form.Log_StringGrid.Align := alClient;
  Main_Form.Log_StringGrid.RowCount := 1;
  Main_Form.Log_StringGrid.SelectedColor := $00666666;
  Main_Form.Log_StringGrid.GridLineColor := $00555555;

// base tracking indicators
  Main_Form.BaseRA_Edit.Font.Color := clRed;
  Main_Form.BaseDEC_Edit.Font.Color := clRed;

// watch folder
  Main_Form.WatchFolder_BCButton.Down := false;

// hemisphere
  sideOfPier := aMount.sideofpier;  // assume we are pointing telescope at east sky

end;

//******************************************************************************
// Start
//******************************************************************************
procedure Run_Start;
var
  s: string;

begin

// init
  RunRequest := false;
  ATrackRunning := true;

// disable processing of images
  processingImage := false;

// log
  if logFileOpen then CloseFile(F_Log);
  s := Main_Form.Images_DirectoryEdit.Text;
  if not DirectoryExists(s) then
    CreateDir(s);

  s := s + '\' + Main_Form.ImagesSubFolder_Edit.Text + '\';
  if not DirectoryExists(s) then
    CreateDir(s);
  logFileName := s + 'ATrack Log ' + FormatDateTime('ddmmyyyyhhnnss',now) + '.txt';

  Assign(F_Log,logFileName);
  Rewrite(F_Log);
  Append(F_Log);
  logFileOpen := true;
  Main_Log('Session Start: ' + ExtractFileName(Main_Form.Profile_Edit.text));

// check V2 driver
  if not(aMount.connected) then
    begin
      s := 'Warning: Mount not connected';
      ShowMessage(s);
      Main_Log(s);
    end;

// base tracking indicators
  Main_Form.BaseRA_Edit.Font.Color := clLime;
  Main_Form.BaseDEC_Edit.Font.Color := clLime;

// reset
  ResetSessionVariables;

// reset subsystems
  Drift_Reset;
  Centering_Reset;

// update tracking rates
  Run_UpdateTrackingRates;

// reset Log
  Main_Form.Log_StringGrid.RowCount := 1;
  Run_PSHeader;
  Run_Header;

// start folder monitor
  Main_StartDirectoryWatch;

// Activate session timer
  startTime := now - 0.00001;
  Run_UpdateElapsedTime;
  Main_Form.Session_Timer.enabled := true;

// update status
  UpdateStatus;

// update indicators
  Run_UpdateIndicators;

// ready
  SetIndicatorsColor;

end;

//******************************************************************************
// Stop
//******************************************************************************
procedure Run_Stop;
begin
  StopRequest := false;
  ATrackRunning := false;

//Turn off Folder monitor
  Main_StopDirectoryWatch;

// Turn off session timer
  Main_Form.Session_Timer.Enabled := false;

// reset subsystems
  Drift_Reset;
  Centering_Reset;

// watch folder
  Main_Form.WatchFolder_BCButton.Down := false;

//Update Status
  Main_Log('Processing Stopped');
  Main_Log('Number of Images processed: ' + IntToStr(nNewImages));
  Main_Log('Number of PlateSolve fails: ' + IntToStr(nPlateSolveFails));

//reset run button
  Main_Form.BaseRA_Edit.Font.Color := clRed;
  Main_Form.BaseDEC_Edit.Font.Color := clRed;
  Main_Form.CenterOnTargetRA_MaskEdit.font.color := clRed;
  Main_Form.CenterOnTargetDEC_MaskEdit.font.color := clRed;

// done
  SetIndicatorsColor;
  if not logFileOpen then exit;
  CloseFile(F_Log);
  logFileOpen := false;

end;

//******************************************************************************
// Reset Session variables
//******************************************************************************
procedure ResetSessionVariables;
var
  s: string;

begin
// session variables
  eTime := 0.0;
  dt := 0.0;
  nSec := 0.0;
  skipNImagesCounter := StrToIntDef(Main_Form.SkipNImages_MaskEdit.EditText,0);

// reference image
  referenceImage.solved := false;
  if Main_Form.CenterOnTarget_Checkbox.checked then
  begin
    s := Main_Form.CenterOnTargetRA_MaskEdit.editText;
    referenceImage.RAhours := global_RAStringToDouble(s);
    Main_Form.CenterOnTargetRA_MaskEdit.font.color := clLime;
    s := Main_Form.CenterOnTargetDEC_MaskEdit.editText;
    referenceImage.DECdegrees := global_DECStringToDouble(s);
    Main_Form.CenterOnTargetDEC_MaskEdit.font.color := clLime;
    referenceImage.solved := true;
  end;

// previous image
  prevImage.solved := false;

// image stats
  nNewImages := 0;
  nImagesProcessed := 0;
  nPlateSolveFails := 0;
  nImagesUsed := 0;
  nImagesRemaining := 0;

// image shift
  RAp.ImageShift := 0.0;
  DECp.ImageShift := 0.0;

// image drift
  RAp.ImageDrift := 0.0;
  DECp.ImageDrift := 0.0;

// initialize base tracking rate
  RAp.aBase := 0.0;
  DECp.aBase := 0.0;
  Run_SetBase;

// watch folder
  Main_Form.WatchFolder_BCButton.Down := true;

// hemisphere
  sideOfPier := aMount.sideOfPier;

// reset indicators
  ResetIndicators;

// done
  exit;

end;

//******************************************************************************
// Update tracking rates
//******************************************************************************
procedure Run_UpdateTrackingRates;
var
  r1,r2: double;

begin
// init
  r1 := 0.0;
  r2 := 0.0;

// base rate
  RAp.aBase := 0.0;
  if Main_Form.base_RA_checkbox.checked then
    RAp.aBase := RAp.Base;

  DECp.aBase := 0.0;
  if Main_Form.base_DEC_checkbox.checked then
    DECp.aBase := DECp.Base;

// drift correction
  RAp.aDrift := 0.0;
  DECp.aDrift := 0.0;
  if (Main_Form.DriftCorrection_BCButton.down) and
     ATrackRunning then
    driftFlag := true
  else
    driftFlag := false;

  if driftFlag then
  begin
    if Main_Form.drift_RA_checkbox.checked then
      RAp.aDrift := RAp.drift;
    if Main_Form.drift_DEC_checkbox.checked then
      DECp.aDrift := DECp.drift;
  end;

// centering
  RAp.aCent := 0.0;
  DECp.aCent := 0.0;
  if (Main_Form.Centering_BCButton.down) and
   ATrackRunning then
    centeringFlag := true
  else
    centeringFlag := false;

  if centeringFlag then
  begin
    if Main_Form.centering_RA_checkbox.checked then
      RAp.aCent := RAp.cent;
    if Main_Form.centering_DEC_checkbox.checked then
      DECp.aCent := DECp.cent;
  end;

// calculate new tracking rate
  r1 := RAp.aBase + RAp.aModel + RAp.aDrift + RAp.aCent;
  r2 := DECp.aBase + DECp.aModel + DECp.aDrift + DECp.aCent;

// invert
  if Main_Form.InvertRA_Checkbox.checked then
    r1 := -r1;
  if Main_Form.InvertDEC_Checkbox.checked then
    r2 := -r2;

// update motors
  Mount_SetTrackingRates(r1,r2);

// update indicators
  Run_UpdateIndicators;

end;

//******************************************************************************
// New Image
// called from either:
//   watch folder found a new image added
//******************************************************************************
procedure Run_NewImage;

var
  n: integer;

label done1;
label done2;

begin
// init
  if not ATrackRunning then exit;
  inc(nNewImages);
  RAp.savStatus := RAp.status;
  DECp.savStatus := DECp.status;

// do not process new images
  if not Main_Form.WatchFolder_BCButton.down then goto done2;

// tracking rate
  RAp.savTrate := aMount.raRate;
  DECp.savTrate := aMount.decRate;

// filter images
  if not (Main_Form.ImageMask_Edit.Text = '') then
    if pos(Main_Form.ImageMask_Edit.Text, newImageFilename) = 0 then goto done2;

// skip N images
  n := skipNImagesCounter - 1;
  if n > 0 then
  begin
    skipNImagesCounter := n;
    goto done2;
  end;
  skipNImagesCounter := StrToIntDef(Main_Form.SkipNImages_MaskEdit.EditText,0);

// increment number of images processed
  inc(nImagesProcessed);

// process the image
  global_ProcessImage;
  if not newImage.solved then goto done1;

// pass to subsystems
  if Main_Form.PIDTrackingCorrection_CheckBox.checked then
  begin
    PID_NewImage;
    if Main_Form.PIDFiltering_CheckBox.checked then
    begin
      RAp.drift := RA_new_rate_Dfilt;
      DECp.drift := DEC_new_rate_Dfilt;
    end
    else
    begin
      RAp.drift := RA_new_rate;
      DECp.drift := DEC_new_rate;
    end;
  end
  else
  begin
    Centering_NewImage;
    Drift_NewImage;

  end;

// update mount
  Run_UpdateTrackingRates;

// save this image as previous image
  prevImage := newImage;

// update status
done1:
  UpdateStatus;
  Run_UpdateLog;

done2:
  Run_UpdateIndicators;
  processingImage := false;

end;

//******************************************************************************
// update elapsed time (1 second timer event)
// called each second from timer event
//******************************************************************************
procedure Run_UpdateElapsedTime;
begin
//  Elapsed time = now - start time
  eTime := now - startTime;
  Main_Form.ElapsedTime_Edit.text := FormatDateTime('hh.mm.ss',eTime);

// centering timers
  Centering_UpdateTimers;

// pier flip
  if sideOfPier = aMount.sideOfPier then exit;

  sideOfPier := aMount.sideOfPier;
  prevImage.solved := false;
  Drift_Reset;
  Drift_Reset2;
  Centering_Reset;
  Run_UpdateTrackingRates;

  Main_Log('PIER FLIP');
  if not Main_Form.CenterOnTarget_Checkbox.checked then
    referenceImage.solved := false;


end;

//******************************************************************************
// Update Log
//******************************************************************************
procedure Run_UpdateLog;
var
  logRecord: string;

label done;

begin
// init
  logRecord := '';

// seq #
  logRecord := logRecord + IntToStr(nImagesProcessed);

// filename
  logRecord := logRecord + TAB + newImage.fileName;

// if image not solved then
  if not newImage.solved then
  begin
    logRecord := logRecord + TAB + 'Image Not Solved' + TAB + PlateSolveError;
    goto done;
  end;

// JD Date
  logRecord := logRecord + TAB + FloatToStr(newImage.jd);

// UTC date
  logRecord := logRecord + TAB + newImage.DateS;

// UTC time
  logRecord := logRecord + TAB + newImage.TimeS;

// LST
  logRecord := logRecord + TAB + formatfloat('00.00000',aMount.lst);

// HA
  logRecord := logRecord + TAB + formatfloat('0.00000',aMount.ha);

// eTime
  logRecord := logRecord + TAB + formatfloat('00.00000',eTime);

// nCatStars
  logRecord := logRecord + TAB + IntToStr(newImage.nCatalogStars);

// nImageStars
  logRecord := logRecord + TAB + IntToStr(newImage.nImageStars);

// airmass
  logRecord := logRecord + TAB + formatfloat('0.00',newImage.airmass);

// Exposure interval
  logRecord := logRecord + TAB + formatfloat('0000.00',newImage.ExposureInterval);

// Match Fit Order
  logRecord := logRecord + TAB + IntToStr(newImage.MatchFitOrder);

// FWHM
  logRecord := logRecord + TAB + formatfloat('00.0',newImage.FullWidthHalfMax);

// Background mean
  logRecord := logRecord + TAB + formatfloat('00000.0',newImage.ImageBackgroundMean);

// Background sigma
  logRecord := logRecord + TAB + formatfloat('00.00',newImage.ImageBackgroundSigma);

// Match Avg residual
  logRecord := logRecord + TAB + formatfloat('00.00',newImage.MatchAvgResidual);

// Match RMS residual
  logRecord := logRecord + TAB + formatfloat('00.00',newImage.MatchRMSResidual);

// RA
  logRecord := logRecord + TAB + ' ';

  // POS
    logRecord := logRecord + TAB + formatfloat('00.00000',newImage.RAhours);

  // TRATE
    logRecord := logRecord + TAB + formatfloat('0.00000',RAp.savTrate);

  // ISHFT
    logRecord := logRecord + TAB + formatfloat('000.00',RAp.ImageShift);

  // IDRFT
    logRecord := logRecord + TAB + formatfloat('000.00',RAp.ImageDrift);

  // ICENT
    logRecord := logRecord + TAB + formatfloat('000.00',RAp.centDistanceMoved);

  // (calculated) BASE
    logRecord := logRecord + TAB + formatfloat('0.00000',RAp.base);

  // (calculated) MODEL
    logRecord := logRecord + TAB + formatfloat('0.00000',RAp.model);

  // (calculated) DRIFT
    logRecord := logRecord + TAB + formatfloat('0.00000',RAp.drift);

  // (calculated) CENT
    logRecord := logRecord + TAB + formatfloat('0.00000',RAp.cent);

  // (actual) BASE
    logRecord := logRecord + TAB + formatfloat('0.00000',RAp.abase);

  // (actual) MODEL
    logRecord := logRecord + TAB + formatfloat('0.00000',RAp.amodel);

  // (actual) DRIFT
    logRecord := logRecord + TAB + formatfloat('0.00000',RAp.adrift);

  // (actual) CENT
    logRecord := logRecord + TAB + formatfloat('0.00000',RAp.acent);
    logRecord := logRecord + TAB + IntToStr(RAp.centTime);

  // new TRATE
    logRecord := logRecord + TAB + formatfloat('0.00000',aMount.raRate);


// DEC
  logRecord := logRecord + TAB + ' ';

  // POS
    logRecord := logRecord + TAB + formatfloat('00.00000',newImage.DECdegrees);

  // TRATE
    logRecord := logRecord + TAB + formatfloat('0.00000',DECp.savTrate);

  // ISHFT
    logRecord := logRecord + TAB + formatfloat('000.00',DECp.imageShift);

  // IDRFT
    logRecord := logRecord + TAB + formatfloat('000.00',DECp.ImageDrift);

  // ICENT
    logRecord := logRecord + TAB + formatfloat('000.00',DECp.centDistanceMoved);

  // (calculated) BASE
    logRecord := logRecord + TAB + formatfloat('0.00000',DECp.base);

  // (calculated) MODEL
    logRecord := logRecord + TAB + formatfloat('0.00000',DECp.model);

  // (calculated) DRIFT
    logRecord := logRecord + TAB + formatfloat('0.00000',DECp.drift);

  // (calculated) CENT
    logRecord := logRecord + TAB + formatfloat('0.00000',DECp.cent);

  // (actual) BASE
    logRecord := logRecord + TAB + formatfloat('0.00000',DECp.abase);

  // (actual) MODEL
    logRecord := logRecord + TAB + formatfloat('0.00000',DECp.amodel);

  // (actual) DRIFT
    logRecord := logRecord + TAB + formatfloat('0.00000',DECp.adrift);

  // (actual) CENT
    logRecord := logRecord + TAB + formatfloat('0.00000',DECp.acent);
    logRecord := logRecord + TAB + IntToStr(DECp.centTime);

  // new TRATE
    logRecord := logRecord + TAB + formatfloat('0.00000',aMount.decRate);

// write it to the log file
done:
  Main_Log(logRecord);
  AddToGrid(logRecord);
end;

//******************************************************************************
// Update Indicators
//******************************************************************************
procedure Run_UpdateIndicators;
begin
// image stats
  Main_Form.nNewImages_Edit.text := IntToStr(nNewImages);
  Main_Form.nImagesProcessed_Edit.text := IntToStr(nImagesProcessed);
  Main_Form.nPlateSolveFails_Edit.text := IntToStr(nPlateSolveFails);
  Main_Form.nImagesUsed_Edit.text := IntToStr(nImagesUsed);
  Main_Form.delayCounter_Edit.text := IntToStr(skipNImagesCounter);

// image time
  Main_Form.ImageTime_Edit.text := newImage.timeS;

// dt
  Main_Form.dt_Edit.text := FormatDateTime('hh.mm.ss',dt);

// image RA
  Main_Form.imageCoordinatesRA_Edit.text := global_RAToString(newImage.RAhours);

// image DEC
  Main_Form.imageCoordinatesDEC_Edit.text := global_DECToString(newImage.DECdegrees);


// shift RA
  Main_Form.imageShift1stImageRA_Edit.text := formatfloat('000.0',RAp.imageShift);


// shift DEC
  Main_Form.imageShift1stImageDEC_Edit.text := formatfloat('000.0',DECp.imageShift);


// drift RA
  Main_Form.imageShiftPrevImageRA_Edit.text := formatfloat('000.0',RAp.imageDrift);


// drift DEC
  Main_Form.imageShiftPrevImageDEC_Edit.text := formatfloat('000.0',DECp.imageDrift);


// Corrections
// Base
  Main_Form.baseRA_Edit.text := formatfloat('0.00000',RAp.base);
  if ATrackRunning then
    if Main_Form.Base_RA_CheckBox.checked then
      Main_Form.BaseRA_Edit.Font.color := clLime
    else
      Main_Form.BaseRA_Edit.Font.color := clRed
  else
    Main_Form.BaseRA_Edit.Font.color := clRed;

  Main_Form.baseDEC_Edit.text := formatfloat('0.00000',DECp.base);
  if ATrackRunning then
    if Main_Form.Base_DEC_CheckBox.checked then
      Main_Form.BaseDEC_Edit.Font.color := clLime
    else
      Main_Form.BaseDEC_Edit.Font.color := clRed
  else
    Main_Form.BaseDEC_Edit.Font.color := clRed;

// Drift
  Main_Form.DriftCorrectionRA_Edit.text := formatfloat('0.00000',RAp.drift);
  if driftFlag then
    if Main_Form.Drift_RA_CheckBox.checked then
      Main_Form.DriftCorrectionRA_Edit.Font.color := clLime
    else
      Main_Form.DriftCorrectionRA_Edit.Font.color := clRed
  else
    Main_Form.DriftCorrectionRA_Edit.Font.color := clRed;

  Main_Form.DriftCorrectionDEC_Edit.text := formatfloat('0.00000',DECp.drift);
  if driftFlag then
    if Main_Form.Drift_DEC_CheckBox.checked then
      Main_Form.DriftCorrectionDEC_Edit.Font.color := clLime
    else
      Main_Form.DriftCorrectionDEC_Edit.Font.color := clRed
  else
    Main_Form.DriftCorrectionDEC_Edit.Font.color := clRed;

// Centering
  Main_Form.CenteringRA_Edit.text := formatfloat('0.00000',RAp.cent);
  if centeringFlag then
    if Main_Form.Centering_RA_CheckBox.checked then
      Main_Form.CenteringRA_Edit.Font.color := clLime
    else
      Main_Form.CenteringRA_Edit.Font.color := clRed
  else
    Main_Form.CenteringRA_Edit.Font.color := clRed;
  Main_Form.CenteringRATimer_Edit.text := IntToStr(RAp.centTime);

  Main_Form.CenteringDEC_Edit.text := formatfloat('0.00000',DECp.cent);
  if centeringFlag then
    if Main_Form.Centering_DEC_CheckBox.checked then
      Main_Form.CenteringDEC_Edit.Font.color := clLime
    else
      Main_Form.CenteringDEC_Edit.Font.color := clRed
  else
    Main_Form.CenteringDEC_Edit.Font.color := clRed;
  Main_Form.CenteringDECTimer_Edit.text := IntToStr(DECp.centTime);

// PID
  Main_Form.PIDRACorrection_Edit.text := formatfloat('0.00000',RA_new_rate);
  Main_Form.PIDDECCorrection_Edit.text := formatfloat('0.00000',DEC_new_rate);

  Main_Form.PIDRAFilteredCorrection_Edit.text := formatfloat('0.00000',RA_new_rate_Dfilt);
  Main_Form.PIDDECFilteredCorrection_Edit.text := formatfloat('0.00000',DEC_new_rate_Dfilt);

end;

//******************************************************************************
// add log record to spreadsheet
//******************************************************************************
procedure AddToGrid(s: string);
var
  col: integer;
  t: string;
  row: integer;

begin
// init
  row := Main_Form.Log_StringGrid.RowCount;

// if not first row then add a new one
  if Main_Form.Log_StringGrid.tag = 1 then
  begin
    inc(row);
    Main_Form.Log_StringGrid.RowCount := row;
  end;

// actual row index
  row := row - 1;

// update the row
  for col:=0 to Main_Form.Log_StringGrid.colcount-1 do
  begin
    t := Global_GetField(s, col+1, TAB, false);
    Main_Form.Log_StringGrid.Cells[col,row] := t;
  end;

// no longer the first row
  Main_Form.Log_StringGrid.tag := 1;

// update column widths
  Main_Form.Log_StringGrid.AutoSizeColumns;

end;

//******************************************************************************
// do Plate Solve header to log file
//******************************************************************************
procedure Run_PSHeader;
var
  logRecord: string;

begin
// init
  logRecord := 'Pinpoint plate solve parameters';
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Use Faint Stars
  logRecord := 'Use faint stars';
  if Main_Form.UseFaintStars_CheckBox.checked then
    logRecord := logRecord + TAB + ' True'
  else
    logRecord := logRecord + TAB + ' False';
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Use SExtractor
  logRecord := 'Use SExtractor';
  if Main_Form.UseSExtractor_CheckBox.checked then
    logRecord := logRecord + TAB + ' YES'
  else
    logRecord := logRecord + TAB + ' NO';
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Convolve Gaussian
  logRecord := 'Convolve gaussian';
  if Main_Form.ConvolveGaussian_CheckBox.checked then
    logRecord := logRecord + TAB + ' YES'
  else
    logRecord := logRecord + TAB + ' NO';
  logRecord := logRecord + ' (FWHM = ' + Main_Form.FilterFWHM_MaskEdit.editText + ')';
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Convolve Log
  logRecord := 'Convolve log';
  if Main_Form.ConvolveLog_CheckBox.checked then
    logRecord := logRecord + TAB + ' YES'
  else
    logRecord := logRecord + TAB + ' NO';
  logRecord := logRecord + ' (FWHM = ' + Main_Form.FilterFWHM_MaskEdit.editText + ')';
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Remove Hot Pixels
  logRecord := 'Remove hot pixels';
  if Main_Form.RemoveHotPixels_CheckBox.checked then
    logRecord := logRecord + TAB + ' YES'
  else
    logRecord := logRecord + TAB + ' NO';
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Max Solve Time
  logRecord := 'Max solve time' + TAB + Main_Form.maxSolveTime_MaskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Max Solve Stars
  logRecord := 'Max solve stars' + TAB + Main_Form.maxSolveStars_MaskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Min Star Size
  logRecord := 'Minimum star size' + TAB + Main_Form.MinStarSize_MaskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Min Match Stars
  logRecord := 'Minimum match stars' + TAB + Main_Form.MinMatchStars_MaskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Sigma Above Mean
  logRecord := 'Sigma above mean' + TAB + Main_Form.SigmaAboveMean_MaskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Catalog
  logRecord := 'Catalog' + TAB + Main_Form.catalog_combobox.Text;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Cat Path
  logRecord := 'Catalog path' + TAB + Main_Form.catPath_DirectoryEdit.directory;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Cat Max Mag
  logRecord := 'Catalog maximum magnitude' + TAB + Main_Form.maxMag_maskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Cat Min Mag
  logRecord := 'Catalog minimum magnitude' + TAB + Main_Form.CatMinMag_maskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Cat Expansion
  logRecord := 'Catalog expansion' + TAB + Main_Form.CatExpansion_maskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// H Plate Scale
  logRecord := 'Horizontal plate scale' + TAB + Main_Form.hPlateScale_maskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// V Plate Scale
  logRecord := 'Vertical plate scale' + TAB + Main_Form.vPlateScale_maskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Inner Aperture
  logRecord := 'Inner aperture' + TAB + Main_Form.InnerAperture_maskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Outer Aperture
  logRecord := 'Outer aperture' + TAB + Main_Form.OuterAperture_maskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Exclusion Border
  logRecord := 'Image exclusion border' + TAB + Main_Form.ExclusionBorder_maskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Background Tile Size
  logRecord := 'Background tile size' + TAB + Main_Form.BackgroundTileSize_maskEdit.editText;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Centroid Algorithm
  logRecord := 'Centroid algorithm' + TAB + Main_Form.CentroidAlgorithm_combobox.text;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// Projection Type
  logRecord := 'Projection type' + TAB + Main_Form.ProjectionType_ComboBox.text;
  Main_Log(logRecord);
  AddToGrid(logRecord);

// adjust column widths

// separator
  logRecord := ' ';
  Main_Log(logRecord);
  AddToGrid(logRecord);

end;

procedure ResetIndicators;
begin
// image stats
  Main_Form.nNewImages_Edit.text := '0';
  Main_Form.nImagesProcessed_Edit.text := '0';
  Main_Form.nPlateSolveFails_Edit.text := '0';
  Main_Form.nImagesUsed_Edit.text := '0';
  Main_Form.delayCounter_edit.text := '0';
  Main_Form.nImageStars_edit.text := '0';
  Main_Form.ImageFWHM_edit.text := '0.00';
  Main_Form.ImageBackground_edit.text := '0.00';

end;

procedure SetIndicatorsColor;
var
  iColor: TColor;

begin

// run state
  if ATrackRunning then
    iColor := clLime
  else
    iColor := clRed;

// Images
  Main_Form.nNewImages_Edit.Font.color := iColor;
  Main_Form.nImagesProcessed_Edit.Font.color := iColor;
  Main_Form.nPlateSolveFails_Edit.Font.color := iColor;
  Main_Form.nImagesUsed_Edit.Font.color := iColor;
  Main_Form.delayCounter_Edit.Font.color := iColor;

//  Time = image time
  Main_Form.ImageTime_Edit.Font.color := iColor;

//  Elapsed Time = 0
  Main_Form.ElapsedTime_Edit.Font.color := iColor;

//  dt = 0
  Main_Form.dt_Edit.Font.color := iColor;

// position
  Main_Form.imageCoordinatesRA_Edit.Font.color := iColor;
  Main_Form.imageCoordinatesDEC_Edit.Font.color := iColor;

//  shift (prev) = 0
  Main_Form.imageShiftPrevImageRA_Edit.Font.color := iColor;
  Main_Form.imageShiftPrevImageDEC_Edit.Font.color := iColor;

//  shift (1st) = 0
  Main_Form.imageShift1stImageRA_Edit.Font.color := iColor;
  Main_Form.imageShift1stImageDEC_Edit.Font.color := iColor;

// image data
  Main_Form.nImageStars_edit.Font.color := iColor;
  Main_Form.ImageFWHM_edit.Font.color := iColor;
  Main_Form.ImageBackground_edit.Font.color := iColor;

end;

procedure UpdateStatus;
begin
// running
  RAp.status := 'R';
  DECp.status := 'R';

end;

//******************************************************************************
// do Header line
//******************************************************************************
procedure Run_Header;
var
  logRecord: string;

begin
// add header line to log
  logRecord := 'SEQ #' + TAB +
               'FNAME' + TAB +
               'JDATE' + TAB +
               'UTC DATE' + TAB +
               'UTC TIME' + TAB +
               'LST' + TAB +
               'HA' + TAB +
               'ETIME' + TAB +
               'NCATSTARS' + TAB +
               'NIMAGESTARS' + TAB +
               'AMASS' + TAB +
               'EXP' + TAB +
               'MFO' + TAB +
               'FWHM' + TAB +
               'BACKMEAN' + TAB +
               'BACKSD' + TAB +
               'MAR' + TAB +
               'MRMSR' + TAB +

               'RA' + TAB +
               'POS' + TAB +
               'TRATE' + TAB +
               'ISHFT' + TAB +
               'IDRFT' + TAB +
               'ICENT' + TAB +
               'BASE' + TAB +
               'MODEL' + TAB +
               'DRIFT' + TAB +
               'CENT' + TAB +
               'aBASE' + TAB +
               'aMODEL' + TAB +
               'aDRIFT' + TAB +
               'aCENT' + TAB +
               'aCTIME' + TAB +
               'TRATE' + TAB +

               'DEC' + TAB +
               'POS' + TAB +
               'TRATE' + TAB +
               'ISHFT' + TAB +
               'IDRFT' + TAB +
               'ICENT' + TAB +
               'BASE' + TAB +
               'MODEL' + TAB +
               'DRIFT' + TAB +
               'CENT' + TAB +
               'aBASE' + TAB +
               'aMODEL' + TAB +
               'aDRIFT' + TAB +
               'aCENT' + TAB +
               'aCTIME' + TAB +
               'TRATE';

  Main_Log(logRecord);
  AddToGrid(logRecord);

end;

procedure Run_SetBase;
var
  r: double;

begin
  r := StrToFloatDef(Main_Form.BaseTrackingRateRA_MaskEdit.editText,0);
  RAp.base := r;

  r := StrToFloatDef(Main_Form.BaseTrackingRateDEC_MaskEdit.editText,0);
  DECp.base := r;

  Main_Form.Base_RA_CheckBox.Checked := true;
  Main_Form.Base_DEC_CheckBox.Checked := true;

  Run_UpdateTrackingRates;
end;

end.

