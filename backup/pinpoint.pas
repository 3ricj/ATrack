unit Pinpoint;

{$mode objfpc}{$H+}

interface

uses Classes, ComObj, Variants, SysUtils, COMCtrls, Dialogs, dateutils;

procedure Pinpoint_Open;
procedure PinPoint_Init;
procedure PinPoint_Close;
function PinPoint_Reset:boolean;
function PinPoint_OpenImage:boolean;
function PinPoint_CloseImage:boolean;
function PinPoint_PlateSolve:boolean;

var
  imageLoaded: boolean;
  PinpointAssigned: boolean;

implementation

uses Main, global;

procedure ImageTime; forward;

//******************************************************************************
// Open
//******************************************************************************
procedure Pinpoint_Open;
begin
// not using PP
  if not(Main_Form.UsePinPoint_CheckBox.checked) then exit;
  if PinpointAssigned then exit;

// init
  imageLoaded := false;

// create OLE object
  try
    PinPointObject := CreateOleObject('PinPoint.Plate');
    PinpointAssigned := true;
  except
    ShowMessage('Connection to Pinpoint failed');
    exit;
  end;
end;

//******************************************************************************
// Init
//******************************************************************************
procedure PinPoint_Init;
begin

end;
procedure PinPoint_Close;
begin
end;

//******************************************************************************
// Open Image
//******************************************************************************
function PinPoint_OpenImage:boolean;
begin

// not using PP
  if not(Main_Form.UsePinPoint_CheckBox.checked) then
  begin
    result := true;
    exit;
  end;


// init
  PinPoint_Open;
  result := false;

// remove old one
  if imageLoaded then
  begin
    PinPointObject.DetachFITS;
    imageLoaded := false;
  end;

// attach the image
  if not PinPointObject.AttachFITS(wideString(newImage.fileName)) then
  begin
      PlateSolveError := 'attach error';
      exit;
  end;
  imageLoaded := true;

// reset image parameters
  if not PinPoint_Reset then exit;

// I guess it worked
  newImage.UTCDateTime := 0.0;
  newImage.RAhours := 0.0;
  newImage.DECdegrees := 0.0;
  newImage.airmass := 0.0;
  newImage.ExposureInterval := 0.0;
  newImage.MatchFitOrder := 0;
  newImage.FullWidthHalfMax := 0.0;
  newImage.ImageBackgroundMean := 0.0;
  newImage.ImageBackgroundSigma := 0.0;
  newImage.MatchAvgResidual := 0.0;
  newImage.MatchRMSResidual := 0.0;

// done
  result := true;

end;



//******************************************************************************
// Plate Solve
//******************************************************************************
function PinPoint_PlateSolve:boolean;
var
  r: double;
  ra,mydec: double;

begin
// init
  result := false;
  newImage.solved := false;
  PlateSolveError := '';

// load the FITS image into PinPoint
  if not PinPoint_OpenImage then
  begin
    PlateSolveError := 'open image failed';
    exit;
  end;

// image Time
  ImageTime;

// solve starting point
  ra := PinPointObject.TargetRightAscension;
  mydec := PinPointObject.TargetDeclination;
  PinPointObject.RightAscension := ra;
  PinPointObject.Declination := mydec;

// solve
// Convolve Gaussian',);
  r := StrToFloat(Main_Form.FilterFWHM_MaskEdit.editText);
  if Main_Form.ConvolveGaussian_CheckBox.checked then
  begin
    if not PinPointObject.ConvolveGaussian(r) then
      PlateSolveError := 'ConvolveGaussian failed';
    PinPointObject.imageModified := false;
  end;

// Convolve Log',Main_Form.ConvolveLog_CheckBox.checked);
  if Main_Form.ConvolveLog_CheckBox.checked then
  begin
    PinPointObject.ConvolveLog(r);
    if not PinPointObject.ConvolveLog(r) then
      PlateSolveError := 'ConvolveLog failed';
    PinPointObject.imageModified := false;
  end;

// Remove Hot Pixels',Main_Form.RemoveHotPixels_CheckBox.checked);
  r := StrToFloat(Main_Form.HotPixelsThreshold_MaskEdit.editText);
  if Main_Form.RemoveHotPixels_CheckBox.Checked then
    PinPointObject.RemoveHotPixels(r);

// errors so far
  if not (PlateSolveError = '') then
  begin
    result := false;
    PinPoint_CloseImage;
    exit;
  end;

// plate solve
  try
    PinPointObject.Solve;
  except
    PlateSolveError := 'unable to plate solve image';
    result := false;
  end;

  if PinPointObject.solved then
  begin
    result := true;
    newImage.solved := true;
    if not PinPointObject.CatalogStarsReady then
    begin
      PinPointObject.FindCatalogStars;
      PinPointObject.FindImageStars;
    end;

    // update new image data
    newImage.RAhours := PinPointObject.RightAscension;
    newImage.DECdegrees := PinPointObject.Declination;
    newImage.TelRA := PinPointObject.TargetRightAscension;
    newImage.TelDEC := PinPointObject.TargetDeclination;
    newImage.nCatalogStars := PinPointObject.CatalogStars.count;
    newImage.nImageStars := PinPointObject.ImageStars.count;
    try
      newImage.airmass := PinPointObject.airmass;
    except
      newImage.airmass := 0.0;
    end;
    newImage.ExposureInterval := PinPointObject.ExposureInterval;
    newImage.MatchFitOrder := PinPointObject.MatchFitOrder;
    newImage.FullWidthHalfMax := PinPointObject.FullWidthHalfMax;
    newImage.ImageBackgroundMean := PinPointObject.ImageBackgroundMean;
    newImage.ImageBackgroundSigma := PinPointObject.ImageBackgroundSigma;
    newImage.MatchAvgResidual := PinPointObject.MatchAvgResidual;
    newImage.MatchRMSResidual := PinPointObject.MatchRMSResidual;

  end;

// close the image
  PinPoint_CloseImage;

end;



//******************************************************************************
// Close Image
//******************************************************************************
function PinPoint_CloseImage:boolean;
begin
// not using PP
  if not(Main_Form.UsePinPoint_CheckBox.checked) then
  begin
    result := true;
    exit;
  end;

// remove old one
  PinPointObject.DetachFITS;
  imageLoaded := false;

// I guess it worked
  result := true;

end;

//******************************************************************************
// Image Time (UTC observation time)
// "2018-10-05T07:01:54"
// 12345678901234567890
//          11111111112
//******************************************************************************
procedure ImageTime;
var
  s: string;
  year, month, day: word;
  hour, min, sec: word;
  td: TDateTime;

begin
  s := PinPointObject.ReadFitsValue('DATE-OBS');

// UTC date
  newImage.dateS := copy(s,2,10);
  year := StrToIntDef(copy(s,2,4),0);
  month := StrToIntDef(copy(s,7,2),0);
  day := StrToIntDef(copy(s,10,2),0);

// UTC time
  newImage.timeS := copy(s,13,8);
  hour := StrToIntDef(copy(s,13,2),0);
  min := StrToIntDef(copy(s,16,2),0);
  sec := StrToIntDef(copy(s,19,2),0);

// encode into UTC DateTime
  td := EncodeDate(year,month,day);
  td := td + EncodeTime(hour,min,sec,0);
  newImage.UTCDateTime := td;

// julian date
  newImage.jd := td + 2415018.5;

end;



//******************************************************************************
// Reset
//******************************************************************************
function PinPoint_Reset: boolean;
var
  n: integer;

begin
// init
  result := false;

// Use Faint Stars
  PinPointObject.UseFaintStars := Main_Form.UseFaintStars_CheckBox.checked;

// Use SExtractor
  PinPointObject.UseSExtractor := Main_Form.UseSExtractor_CheckBox.checked;

// Max Solve Time
  PinPointObject.MaxSolveTime := StrToFloat(Main_Form.maxSolveTime_MaskEdit.editText);

// Max Solve Stars
  PinPointObject.MaxSolveStars := StrToInt(Main_Form.maxSolveStars_MaskEdit.editText);

// Min Star Size
  PinPointObject.MinimumStarSize := StrToInt(Main_Form.MinStarSize_MaskEdit.editText);

// Min Match Stars
  PinPointObject.MinMatchStars := StrToInt(Main_Form.MinMatchStars_MaskEdit.editText);

// Sigma Above Mean
  PinPointObject.SigmaAboveMean := StrToFloat(Main_Form.SigmaAboveMean_MaskEdit.editText);

// Catalog
  case Main_Form.catalog_combobox.ItemIndex of
    0: PinPointObject.Catalog := 3;  // GSC v1.1
    1: PinPointObject.Catalog := 4;  // ESA
    2: PinPointObject.Catalog := 5;  // USNO A2
    3: PinPointObject.Catalog := 7;  // USNO B1
    4: PinPointObject.Catalog := 10; // USNO UCAC3
    5: PinPointObject.Catalog := 11; // USNO UCAC4
  else
    PlateSolveError := 'invalid catalog';
    exit;
  end;

// Cat Path
  PinPointObject.CatalogPath := wideString(Main_Form.catPath_DirectoryEdit.directory + '\');

// Cat Max Mag
  PinPointObject.CatalogMaximumMagnitude := StrToFloat(Main_Form.maxMag_MaskEdit.editText);

// Cat Min Mag
  PinPointObject.CatalogMinimumMagnitude := StrToFloat(Main_Form.CatMinMag_MaskEdit.editText);

// Cat Expansion
  PinPointObject.CatalogExpansion := StrToFloat(Main_Form.CatExpansion_MaskEdit.editText);

// H Plate Scale
  PinPointObject.ArcsecPerPixelHoriz := StrToFloat(Main_Form.hPlateScale_MaskEdit.editText);

// V Plate Scale
  PinPointObject.ArcsecPerPixelVert := StrToFloat(Main_Form.vPlateScale_MaskEdit.editText);

// Inner Aperture
  PinPointObject.InnerAperture := StrToFloat(Main_Form.InnerAperture_MaskEdit.editText);

// Outer Aperture
  PinPointObject.OuterAperture := StrToFloat(Main_Form.OuterAperture_MaskEdit.editText);

// Exclusion Border
  PinPointObject.ExclusionBorder := StrToInt(Main_Form.ExclusionBorder_MaskEdit.editText);

// Background Tile Size
  n := StrToInt(Main_Form.BackgroundTileSize_MaskEdit.editText);
  if n > 0 then
    PinPointObject.BackgroundTileSize := n;

// Centroid Algorithm
  PinPointObject.CentroidAlgorithm := Main_Form.CentroidAlgorithm_combobox.ItemIndex;

// Projection Type
  PinPointObject.ProjectionType := Main_Form.ProjectionType_ComboBox.itemIndex;

// done
  result := true;

end;


end.

