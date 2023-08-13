//*****************************************************************************
//  Config
//  Handler for loading, updating, closing the config.ini file
//*****************************************************************************
unit Config;

//******************************************************************************
//********************  GLOBAL DEFINITIONS  ************************************
//******************************************************************************
interface

uses
  Forms, SysUtils, Dialogs, Classes, lazutf8sysutils;

function Config_Load: boolean;
procedure Config_Read(fileName:string);
procedure Config_Write(fileName: string);
procedure Config_Close;

//******************************************************************************
//********************  LOCAL DEFINITIONS  *************************************
//******************************************************************************
implementation
uses Main, IniFiles, global, Run, Network;

var
  ConfigIni: TIniFile;

// *****************************************************************************
// Code Section
// *****************************************************************************

// ****************************************************************************
// Config Load
//  Called by Main when the program first starts, used to load the previous
//  configuration
// ****************************************************************************
function Config_Load: boolean;
var
  s: string;

begin
// init
  result := false;

// check for a config file
  s := '.\config.ini';
  if not FileExists(s) then
  begin
    ShowMessage('config file not found' + s);
    exit;
  end;

// read
  Config_Read(s);

// done
  result := true;
end;


procedure Config_Read(fileName:string);
var
  s: string;

begin
// make sure file does exist
  if not FileExists(fileName) then
  begin
    exit;
  end;

// create class
  ConfigIni := TIniFile.Create(fileName);

// load
  s := 'PROFILE';
  Main_Form.Profile_Edit.Text := ConfigIni.ReadString(s,'Name', '');

  s := 'GUI';
  Main_Form.top := ConfigIni.ReadInteger(s, 'Top',10);
  Main_Form.left := ConfigIni.ReadInteger(s, 'Left',10);

  s := 'Mount';
  Main_Form.TelescopeDriver_Edit.Text := ConfigIni.ReadString(s, 'ASCOM Telescope Driver','');
  Main_Form.MountPollingRate_MaskEdit.Text := ConfigIni.ReadString(s,'Polling rate','001');

  s := 'PLATE SOLVE';
  Main_Form.UsePinpoint_CheckBox.checked := ConfigIni.ReadBool(s,'Use Pinpoint', False);
  Main_Form.ConvolveGaussian_CheckBox.checked := ConfigIni.ReadBool(s,'Convolve Gaussian',FALSE);
  Main_Form.ConvolveLog_CheckBox.checked := ConfigIni.ReadBool(s,'Convolve Log',FALSE);
  Main_Form.FilterFWHM_MaskEdit.Text := ConfigIni.ReadString(s,'Filter FWHM', '3.0');
  Main_Form.RemoveHotPixels_CheckBox.checked := ConfigIni.ReadBool(s,'Remove Hot Pixels',FALSE);
    Main_Form.HotPixelsThreshold_MaskEdit.Text := ConfigIni.ReadString(s,'Hot Pixels Threshold', '0.1');
  Main_Form.UseFaintStars_CheckBox.checked := ConfigIni.ReadBool(s,'Use Faint Stars',FALSE);
  Main_Form.UseSExtractor_CheckBox.checked := ConfigIni.ReadBool(s,'Use SExtractor',FALSE);

  Main_Form.maxSolveTime_MaskEdit.Text := ConfigIni.ReadString(s,'Max Solve Time','60');
  Main_Form.maxSolveStars_MaskEdit.Text := ConfigIni.ReadString(s,'Max Solve Stars','100');
  Main_Form.MinStarSize_MaskEdit.Text := ConfigIni.ReadString(s,'Min Star Size','2');
  Main_Form.MinMatchStars_MaskEdit.Text := ConfigIni.ReadString(s,'Min Match Stars','0');
  Main_Form.SigmaAboveMean_MaskEdit.Text := ConfigIni.ReadString(s,'Sigma Above Mean','3.0');

  Main_Form.catalog_ComboBox.itemIndex := ConfigIni.ReadInteger(s,'Cat index',0);
  Main_Form.catPath_DirectoryEdit.Text := ConfigIni.ReadString(s,'Cat Path','c:\catalog\');
  Main_Form.maxMag_MaskEdit.Text := ConfigIni.ReadString(s,'Cat Max Mag','20');
  Main_Form.CatMinMag_MaskEdit.Text := ConfigIni.ReadString(s,'Cat Min Mag','-2');
  Main_Form.CatExpansion_MaskEdit.Text := ConfigIni.ReadString(s,'Cat Expansion','0.3');

  Main_Form.hPlateScale_MaskEdit.Text := ConfigIni.ReadString(s,'H Plate Scale','1.0');
  Main_Form.vPlateScale_MaskEdit.Text := ConfigIni.ReadString(s,'V Plate Scale','1.0');

  Main_Form.InnerAperture_MaskEdit.Text := ConfigIni.ReadString(s,'Inner Aperture','0');
  Main_Form.OuterAperture_MaskEdit.Text := ConfigIni.ReadString(s,'Outer Aperture','0');

  Main_Form.ExclusionBorder_MaskEdit.Text := ConfigIni.ReadString(s,'Exclusion Border','0');
  Main_Form.BackgroundTileSize_MaskEdit.Text := ConfigIni.ReadString(s,'Background Tile Size','0');

  Main_Form.CentroidAlgorithm_ComboBox.itemIndex := ConfigIni.ReadInteger(s,'Centroid Algorithm',0);
  Main_Form.ProjectionType_ComboBox.itemIndex := ConfigIni.ReadInteger(s,'Projection Type',0);

  s := 'SESSION';
  Main_Form.Images_DirectoryEdit.Text := ConfigIni.ReadString(s,'Images Folder','');
  Main_Form.ImagesSubFolder_Edit.Text := FormatDateTime('yyyy-mm-dd', NowUTC);
  Main_Form.ImageMask_Edit.Text := ConfigIni.ReadString(s,'Image Mask','');

  Main_Form.DriftCorrection_BCButton.down := ConfigIni.ReadBool(s,'Drift Correction',false);
  Main_Form.Centering_BCButton.down := ConfigIni.ReadBool(s,'Centering',false);

  Main_Form.SkipNImages_MaskEdit.Text := ConfigIni.ReadString(s,'Skip N Images','000');

  s := 'CORRECTIONS';
  Main_Form.CorrectionLimitRA_MaskEdit.Text := ConfigIni.ReadString(s,'Max correction RA','0.00500');
  Main_Form.CorrectionLimitDEC_MaskEdit.Text := ConfigIni.ReadString(s,'Max correction DEC','0.10000');

  Main_Form.Base_RA_Checkbox.checked := ConfigIni.ReadBool(s,'Base RA',TRUE);
  Main_Form.Base_DEC_Checkbox.checked := ConfigIni.ReadBool(s,'Base DEC',TRUE);
  Main_Form.Drift_RA_Checkbox.checked := ConfigIni.ReadBool(s,'Drift RA',TRUE);
  Main_Form.Drift_DEC_Checkbox.checked := ConfigIni.ReadBool(s,'Drift DEC',TRUE);
  Main_Form.Centering_RA_Checkbox.checked := ConfigIni.ReadBool(s,'Centering RA',TRUE);
  Main_Form.Centering_DEC_Checkbox.checked := ConfigIni.ReadBool(s,'Centering DEC',TRUE);

  Main_Form.InvertRA_Checkbox.checked := ConfigIni.ReadBool(s,'Invert RA Corrections',False);
  Main_Form.InvertDEC_Checkbox.checked := ConfigIni.ReadBool(s,'Invert DEC Corrections',False);

  s := 'CENTERING';
  Main_Form.CenteringRate_MaskEdit.Text := ConfigIni.ReadString(s,'Centering Rate','1.0');
  Main_Form.ForceCenteringTime_MaskEdit.Text := ConfigIni.ReadString(s,'Force Centering Time','010');

  s := 'TRACKING';
  Main_Form.DriftCorrectionRateLimitRA_MaskEdit.Text := ConfigIni.ReadString(s,'RA Drift Correction Rate Limit','0.00010');
  Main_Form.DriftCorrectionRateLimitDEC_MaskEdit.Text := ConfigIni.ReadString(s,'DEC Drift Correction Rate Limit','0.00150');

  Main_Form.BaseTrackingRateRA_MaskEdit.Text := ConfigIni.ReadString(s,'RA Base Tracking Rate','+0.00000');
  Main_Form.BaseTrackingRateDEC_MaskEdit.Text := ConfigIni.ReadString(s,'DEC Base Tracking Rate','+0.00000');
  Run_SetBase;

  s := 'TARGET';
  Main_Form.CenterOnTarget_CheckBox.Checked := ConfigIni.ReadBool(s,'Center On Target',FALSE);
  Main_Form.CenterOnTargetRA_MaskEdit.Text := ConfigIni.ReadString(s,'Target RA','00 00 00.00');
  Main_Form.CenterOnTargetDEC_MaskEdit.Text := ConfigIni.ReadString(s,'Target DEC','+00 00 00.0');

  s := 'PID';
  Main_Form.PIDTrackingCorrection_CheckBox.checked := ConfigIni.ReadBool(s,'PID Tracking Correction', False);
  Main_Form.PIDFiltering_CheckBox.checked := ConfigIni.ReadBool(s,'PID Filtering', False);
  Main_Form.KpRA_MaskEdit.Text := ConfigIni.ReadString(s,'Kp RA','0.00000');
  Main_Form.KpDEC_MaskEdit.Text := ConfigIni.ReadString(s,'Kp DEC','0.00000');
  Main_Form.KiRA_MaskEdit.Text := ConfigIni.ReadString(s,'Ki RA','0.00000');
  Main_Form.KiDEC_MaskEdit.Text := ConfigIni.ReadString(s,'Ki DEC','0.00000');
  Main_Form.KdRA_MaskEdit.Text := ConfigIni.ReadString(s,'Kd RA','0.00000');
  Main_Form.KdDEC_MaskEdit.Text := ConfigIni.ReadString(s,'Kd DEC','0.00000');
  Main_Form.nFiltRA_MaskEdit.Text := ConfigIni.ReadString(s,'nFilt RA','0.00000');
  Main_Form.nFiltDEC_MaskEdit.Text := ConfigIni.ReadString(s,'nFilt DEC','0.00000');

// done
  ConfigIni.Free;

end;



//******************************************************************************
//  Config close
//******************************************************************************
procedure Config_Close;
begin
// write
  Config_Write('.\config.ini');

end;



//******************************************************************************
//  Config write
//******************************************************************************
procedure Config_Write(fileName: string);
var
  s: string;
  F: TextFile;

begin
// init
  AssignFile(F,fileName);

// create a new file
  Rewrite(F);
  CloseFile(F);

// create class
  ConfigIni := TIniFile.Create(fileName);

  s := 'PROFILE';
  ConfigIni.WriteString(s,'Name', Main_Form.Profile_Edit.Text);

// update window position
  s := 'GUI';
  ConfigIni.WriteInteger(s, 'Top', Main_Form.top);
  ConfigIni.WriteInteger(s, 'Left', Main_Form.left);

  s := 'PLATE SOLVE';
  ConfigIni.WriteBool(s,'Use Pinpoint',Main_Form.UsePinpoint_CheckBox.checked);
  ConfigIni.WriteBool(s,'Convolve Gaussian',Main_Form.ConvolveGaussian_CheckBox.checked);
  ConfigIni.WriteBool(s,'Convolve Log',Main_Form.ConvolveLog_CheckBox.checked);
    ConfigIni.WriteString(s,'Filter FWHM',Main_Form.FilterFWHM_MaskEdit.EditText);
  ConfigIni.WriteBool(s,'Remove Hot Pixels',Main_Form.RemoveHotPixels_CheckBox.checked);
    ConfigIni.WriteString(s,'Hot Pixels Threshold',Main_Form.HotPixelsThreshold_MaskEdit.EditText);
  ConfigIni.WriteBool(s,'Use Faint Stars',Main_Form.UseFaintStars_CheckBox.checked);
  ConfigIni.WriteBool(s,'Use SExtractor',Main_Form.UseSExtractor_CheckBox.checked);

  ConfigIni.WriteString(s,'Max Solve Time',Main_Form.maxSolveTime_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Max Solve Stars',Main_Form.maxSolveStars_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Min Star Size',Main_Form.MinStarSize_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Min Match Stars',Main_Form.MinMatchStars_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Sigma Above Mean',Main_Form.SigmaAboveMean_MaskEdit.EditText);

  ConfigIni.WriteInteger(s,'Cat index',Main_Form.catalog_ComboBox.itemIndex);
  ConfigIni.WriteString(s,'Cat Path',Main_Form.catPath_DirectoryEdit.Text);
  ConfigIni.WriteString(s,'Cat Max Mag',Main_Form.maxMag_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Cat Min Mag',Main_Form.CatMinMag_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Cat Expansion',Main_Form.CatExpansion_MaskEdit.EditText);

  ConfigIni.WriteString(s,'H Plate Scale',Main_Form.hPlateScale_MaskEdit.EditText);
  ConfigIni.WriteString(s,'V Plate Scale',Main_Form.vPlateScale_MaskEdit.EditText);

  ConfigIni.WriteString(s,'Inner Aperture',Main_Form.InnerAperture_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Outer Aperture',Main_Form.OuterAperture_MaskEdit.EditText);

  ConfigIni.WriteString(s,'Exclusion Border',Main_Form.ExclusionBorder_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Background Tile Size',Main_Form.BackgroundTileSize_MaskEdit.EditText);

  ConfigIni.WriteInteger(s,'Centroid Algorithm',Main_Form.CentroidAlgorithm_ComboBox.itemIndex);
  ConfigIni.WriteInteger(s,'Projection Type',Main_Form.ProjectionType_ComboBox.itemIndex);

  s := 'Mount';
  ConfigIni.WriteString(s, 'ASCOM Telescope Driver',Main_Form.TelescopeDriver_Edit.Text);
  ConfigIni.WriteString(s,'Polling rate',Main_Form.MountPollingRate_MaskEdit.EditText);

  s := 'SESSION';
  ConfigIni.WriteString(s,'Images Folder',Main_Form.Images_DirectoryEdit.Text);
  ConfigIni.WriteBool(s,'Drift Correction',Main_Form.DriftCorrection_BCButton.down);
  ConfigIni.WriteBool(s,'Centering',Main_Form.Centering_BCButton.down);

  ConfigIni.WriteString(s,'Skip N Images',Main_Form.SkipNImages_MaskEdit.EditText);

  s := 'CORRECTIONS';
  ConfigIni.WriteString(s,'Max correction RA',Main_Form.CorrectionLimitRA_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Max correction DEC',Main_Form.CorrectionLimitDEC_MaskEdit.EditText);


  ConfigIni.WriteBool(s,'Base RA',Main_Form.Base_RA_Checkbox.checked);
  ConfigIni.WriteBool(s,'Base DEC',Main_Form.Base_DEC_Checkbox.checked);
  ConfigIni.WriteBool(s,'Drift RA',Main_Form.Drift_RA_Checkbox.checked);
  ConfigIni.WriteBool(s,'Drift DEC',Main_Form.Drift_DEC_Checkbox.checked);
  ConfigIni.WriteBool(s,'Centering RA',Main_Form.Centering_RA_Checkbox.checked);
  ConfigIni.WriteBool(s,'Centering DEC',Main_Form.Centering_DEC_Checkbox.checked);

  ConfigIni.WriteBool(s,'Invert RA Corrections',Main_Form.InvertRA_Checkbox.checked);
  ConfigIni.WriteBool(s,'Invert DEC Corrections',Main_Form.InvertDEC_Checkbox.checked);

  s := 'CENTERING';
  ConfigIni.WriteString(s,'Centering Rate',Main_Form.CenteringRate_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Force Centering Time', Main_Form.ForceCenteringTime_MaskEdit.EditText);

  s := 'TRACKING';
  ConfigIni.WriteString(s,'RA Base Tracking Rate',Main_Form.BaseTrackingRateRA_MaskEdit.EditText);
  ConfigIni.WriteString(s,'DEC Base Tracking Rate',Main_Form.BaseTrackingRateDEC_MaskEdit.EditText);
  ConfigIni.WriteString(s,'RA Drift Correction Rate Limit',Main_Form.DriftCorrectionRateLimitRA_MaskEdit.EditText);
  ConfigIni.WriteString(s,'DEC Drift Correction Rate Limit',Main_Form.DriftCorrectionRateLimitDEC_MaskEdit.EditText);

  s := 'TARGET';
  ConfigIni.WriteBool(s,'Center On Target',Main_Form.CenterOnTarget_CheckBox.Checked);
  ConfigIni.WriteString(s,'Target RA',Main_Form.CenterOnTargetRA_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Target DEC',Main_Form.CenterOnTargetDEC_MaskEdit.EditText);

  s := 'PID';
  ConfigIni.WriteBool(s,'PID Tracking Correction', Main_Form.PIDTrackingCorrection_CheckBox.checked);
  ConfigIni.WriteBool(s,'PID Filtering', Main_Form.PIDFiltering_CheckBox.checked);
  ConfigIni.WriteString(s,'Kp RA',Main_Form.KpRA_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Kp DEC',Main_Form.KpDEC_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Ki RA',Main_Form.KiRA_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Ki DEC',Main_Form.KiDEC_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Kd RA',Main_Form.KdRA_MaskEdit.EditText);
  ConfigIni.WriteString(s,'Kd DEC',Main_Form.KdDEC_MaskEdit.EditText);
  ConfigIni.WriteString(s,'nFilt RA',Main_Form.nFiltRA_MaskEdit.EditText);
  ConfigIni.WriteString(s,'nFilt DEC',Main_Form.nFiltDEC_MaskEdit.EditText);

// done
  ConfigIni.Free;

end;
end.
