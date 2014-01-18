unit AndroidScreenGrabber;

interface

Uses
  System.Classes, FMX.Graphics, FMX.Objects, FMX.Types;

type
  TAndroidScreenGrabber = class(TFMXObject)
  type
    TOrientation = (Landscape, Portrait, InvertedLandscape, InvertedPortrait);
    TProcessing = record
      Busy : Boolean;
      Start : TDateTime;
      LastOrientation : TOrientation;
    end;
    TOnScreenShot = procedure(Image : TImage; Orientation : TOrientation) of object;
  private
    ProcessingData : TProcessing;
    Timer : TTimer;
    FadbPath : string;
    FScreenShotName : string;
    FBatchFileName : string;
    FScale: Double;
    FOnScreenShot: TOnScreenShot;
    FOrientation: TOrientation;
    procedure SetScale(const Value: Double);
    function CreateBatchFile: Boolean;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function ScreenShot: Boolean; // Grabs a screen print
    procedure OnTimer(Sender : TObject);
  public
    constructor Create(AOwner : TComponent; aScale : Double; adbPath : string); reintroduce;
    destructor Destroy; override;
  published
    property Scale: Double read FScale write SetScale;
    property Active : Boolean read GetActive write SetActive;
    property OnScreenShot : TOnScreenShot read FOnScreenShot write FOnScreenShot;
    property Orientation : TOrientation read FOrientation write FOrientation;
  end;

implementation

{ TAndroidScreenGrabber }
uses System.IOUtils, System.SysUtils, System.DateUtils, PNGImage,
  ShellAPI, WinAPI.Windows, FMX.Forms, DisplayUnit;

constructor TAndroidScreenGrabber.Create(AOwner: TComponent; aScale : Double; adbPath: string);
begin
  inherited Create(AOwner);
  FBatchFileName := '';
  FScreenShotName := '';

  Orientation := TOrientation.Portrait;

  FadbPath := adbPath;

  // Sanity check
  if aScale < 10 then
    FScale := 75
  else
    FScale := aScale;

  FBatchFileName := TPath.GetTempPath + 'AndroidScreenGabber.bat';
  FScreenShotName := TPath.GetTempPath  + 'AndroidScreenGabber.png';

  Timer := TTimer.Create(Self);
  Timer.OnTimer := Self.OnTimer;
  Timer.Interval := 100;
  Timer.Enabled := False;
end;

function TAndroidScreenGrabber.CreateBatchFile: Boolean;
// Could potentially add in here Check for platform and change command line file creation
// Should convert this to a platform defined interface at some point.
const
  Batchfile = '@echo off' + sLineBreak +
    'adb -d shell screencap -p /sdcard/androidscreen.png' + sLineBreak +
    'adb -d pull /sdcard/androidscreen.png %TEMP%' + sLineBreak +
    'adb -d shell rm /sdcard/androidscreen.png' + sLineBreak;
var
  bf : TStringList;
begin
  bf := TStringList.Create;
  try
    bf.Text := StringReplace(Batchfile, '%TEMP%', '"' + FScreenShotName + '"', [rfReplaceAll, rfIgnoreCase]);
    bf.Text := StringReplace(bf.Text, 'adb', '"' + FadbPath + '"', [rfReplaceAll, rfIgnoreCase]);
    bf.SaveToFile(FBatchFileName);
    Result := TFile.Exists(FBatchFileName);
  finally
    bf.Free;
  end;
end;

destructor TAndroidScreenGrabber.Destroy;
begin
  try
    Active := False;
    Timer.Free;
  except
    Timer := nil;
  end;
  if TFile.Exists(FBatchFileName) then
    TFile.Delete(FBatchFileName);
  if TFile.Exists(FScreenShotName) then
    TFile.Delete(FScreenShotName);
  inherited;
end;

function TAndroidScreenGrabber.GetActive: Boolean;
begin
  Result := Timer.Enabled;
end;

procedure TAndroidScreenGrabber.OnTimer(Sender: TObject);
var
  Image : TImage;
begin
  if not ProcessingData.Busy then begin
    ProcessingData.Start := Now;
    ProcessingData.Busy := True;
    if ScreenShot then begin

      Image := TImage.Create(nil);
      try
        Image.Bitmap.LoadFromFile(FScreenShotName);
        Image.Bitmap.Resize(
                            Trunc(Image.Bitmap.Width * Scale / 100),
                            Trunc(Image.Bitmap.Height * Scale / 100)
                            );

        {if (Image.Width > Image.Height) and (Orientation = TOrientation.Portrait) then
          Image.RotationAngle := 90
        else if (Image.Width < Image.Height) and (Orientation = TOrientation.Landscape) then
          Image.RotationAngle := -90;}

        if Assigned(FOnScreenShot) then
          FOnScreenShot(Image, Orientation);
      finally
        ProcessingData.Busy := False;
        Image.Free;
        TFile.Delete(FScreenShotName);
      end;
    end;
  end;
//  end else
//    if Assigned(FOnScreenShotWait) then
//      FOnScreenShotWait(MilliSecondsBetween(now, ProcessingData.Start) + Timer.Interval);
end;

function TAndroidScreenGrabber.ScreenShot: Boolean;
var
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  try
    if not FileExists(FBatchFileName) then
      CreateBatchFile;

    if FileExists(FBatchFileName) then begin
      FillChar(SEInfo, SizeOf(SEInfo), 0);
      SEInfo.cbSize := SizeOf(TShellExecuteInfo);
      with SEInfo do begin
        fMask := SEE_MASK_NOCLOSEPROCESS;
        Wnd := 0;//Application.Handle;
        lpFile := PChar(FBatchFileName);
        nShow := SW_HIDE;
      end;
      if ShellExecuteEx(@SEInfo) then begin
        repeat
          Application.ProcessMessages;
          sleep(10);
          GetExitCodeProcess(SEInfo.hProcess, ExitCode);
        until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
      end
      else
        raise Exception.Create('Error grabbing screen!');
    end;
  finally
    Result := TFile.Exists(FScreenShotName);
  end;
end;

procedure TAndroidScreenGrabber.SetActive(const Value: Boolean);
begin
  Timer.Enabled := Value;
end;

procedure TAndroidScreenGrabber.SetScale(const Value: Double);
begin
  if Value > 10 then
    FScale := Value;
end;

end.
