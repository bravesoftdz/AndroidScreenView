unit DisplayUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, AndroidScreenGrabber, FMX.Objects, FMX.Layouts, FMX.Edit;

type
  TForm1 = class(TForm)
    FileOpenDialog1: TOpenDialog;
    pnlMenu: TLayout;
    sbImage: TScrollBox;
    Image1: TImage;
    ImageLayout: TRectangle;
    tbZoom: TTrackBar;
    RadioButton5: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton8: TRadioButton;
    Button1: TButton;
    ToolBarFooter: TToolBar;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    StyleBook1: TStyleBook;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Switch1: TSwitch;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure rbScaleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Change(Sender: TObject);
    procedure RotateScreen(Sender: TObject);
    procedure tbZoomChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure OnScreenShot(ScreenImage : TImage; AOrientation : TAndroidScreenGrabber.TOrientation);
  public
    { Public declarations }
    AndroidScreenGrabber : TAndroidScreenGrabber;
  end;

var
  Form1: TForm1;

implementation

{$R *.FMX}

procedure TForm1.OnScreenShot(ScreenImage: TImage;  AOrientation : TAndroidScreenGrabber.TOrientation);
const
  FrameSize = 3;
var
  AHeight: Integer;
  AWidth: Integer;
  ControlsHeight: Integer;

  function ScrollBarWidth: Integer;
  begin
    Result := 13;//26;
  end;
begin
  Image1.Bitmap.Assign(ScreenImage.Bitmap);

  AHeight := Trunc(ScreenImage.Bitmap.Height);
  AWidth := Trunc(ScreenImage.Bitmap.Width);

  ControlsHeight := Trunc(pnlMenu.Height +pnlMenu.Margins.Top +pnlMenu.Margins.Bottom+
                          ToolBarFooter.Height+ToolBarFooter.Margins.Top+ToolBarFooter.Margins.Bottom+
                                         sbImage.Margins.Top+sbImage.Margins.Bottom);

  Image1.Height := AHeight;
  Image1.Width := AWidth;

  case AOrientation of
    Landscape        : Image1.RotationAngle := 90;
    Portrait         : Image1.RotationAngle := 0;
    InvertedLandscape: Image1.RotationAngle := -90;
    InvertedPortrait : Image1.RotationAngle := 180;
  end;


  case AOrientation of
    Landscape,
    InvertedLandscape : begin
                          ImageLayout.Height := AWidth + FrameSize*2;
                          ImageLayout.Width :=  AHeight + FrameSize*2;

                          Self.ClientHeight := Trunc(ImageLayout.Height + ControlsHeight);
                          Self.ClientWidth :=  Trunc(ImageLayout.Width + ScrollBarWidth);

                          Image1.RotationCenter.X := 0;
                          Image1.RotationCenter.Y := 0;
                          if AOrientation = InvertedLandscape then begin
                            Image1.Position.Y := ImageLayout.Height - FrameSize;
                            Image1.Position.X := FrameSize;
                          end else begin
                            Image1.Position.X := ImageLayout.Width - FrameSize;
                            Image1.Position.Y := FrameSize;
                          end;
                        end;
    Portrait,
    InvertedPortrait : begin
                          ImageLayout.Height := AHeight + FrameSize*2;
                          ImageLayout.Width := AWidth + FrameSize*2;

                          Self.ClientHeight := Trunc(ImageLayout.Height + ControlsHeight);
                          Self.ClientWidth := Trunc(ImageLayout.Width + ScrollBarWidth);

                          Image1.RotationCenter.X := 0.5;
                          Image1.RotationCenter.Y := 0.5;
                          Image1.Position.X := FrameSize;
                          Image1.Position.Y := FrameSize;
                        end;
  end;

  sbImage.ShowScrollBars :=  (ImageLayout.Width > sbImage.Width) or (ImageLayout.Height > sbImage.Height);
end;


procedure TForm1.RotateScreen(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1 : AndroidScreenGrabber.Orientation := TAndroidScreenGrabber.TOrientation.Portrait;
    2 : AndroidScreenGrabber.Orientation := TAndroidScreenGrabber.TOrientation.Landscape;
    3 : AndroidScreenGrabber.Orientation := TAndroidScreenGrabber.TOrientation.InvertedPortrait;
    4 : AndroidScreenGrabber.Orientation := TAndroidScreenGrabber.TOrientation.InvertedLandscape;
  end;
end;

procedure TForm1.tbZoomChange(Sender: TObject);
begin
  AndroidScreenGrabber.Scale := tbZoom.Value;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Image : TImage;
begin
  Image := TImage.Create(Self);
  try
    Image.Bitmap.Assign(Image1.Bitmap);
    if SaveDialog1.Execute then
      Image.Bitmap.SaveToFile(SaveDialog1.FileName);
  finally
    Image.Free;
  end;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  AndroidScreenGrabber.Active := Switch1.IsChecked;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AndroidScreenGrabber.Free;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if AndroidScreenGrabber.Active then begin
    if MessageDlg('Stop Viewer',TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],0) = mrYes then begin
      AndroidScreenGrabber.Active := False;
      Sleep(100);
    end else
      CanClose := False;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  adbPath : string;
begin
  adbPath := GetEnvironmentVariable('ANDROID_HOME') + PathDelim + 'platform-tools' + PathDelim + 'adb.exe';
  if not FileExists(adbPath) then begin
    // Current Beta default
    adbPath := 'C:\Users\Public\Documents\RAD Studio\12.0\PlatformSDKs\adt-bundle-windows-x86-20130522\sdk\platform-tools\adb.exe';
    if not FileExists(adbPath) then begin
      if FileOpenDialog1.Execute then
        adbPath := FileOpenDialog1.FileName
      else
        Application.Terminate;
    end;
  end;

  AndroidScreenGrabber := TAndroidScreenGrabber.Create(Self, tbZoom.Value, adbPath);
  AndroidScreenGrabber.OnScreenShot := OnScreenShot;
  if ParamCount > 0 then
    AndroidScreenGrabber.Active := True;
end;

procedure TForm1.rbScaleClick(Sender: TObject);
begin
  AndroidScreenGrabber.Scale := (Sender as TComponent).Tag;
end;

end.
