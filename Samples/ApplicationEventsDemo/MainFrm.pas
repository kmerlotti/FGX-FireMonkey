unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Platform,
  FGX.ApplicationEvents, FMX.Layouts, FMX.Memo, FMX.StdCtrls, FMX.MultiView, FMX.Controls.Presentation, FMX.ScrollBox;

type
  TFormMain = class(TForm)
    fgApplicationEvents: TfgApplicationEvents;
    mmLog: TMemo;
    MultiView: TMultiView;
    cbOnIdle: TCheckBox;
    GroupBox: TGroupBox;
    cbOnActionExecute: TCheckBox;
    cbOnActionUpdate: TCheckBox;
    cbOnException: TCheckBox;
    cbOnOrientationChanged: TCheckBox;
    cbOnStateChanged: TCheckBox;
    BtnClearLog: TButton;
    GroupBox1: TGroupBox;
    cbAllFormsCreated: TCheckBox;
    cbFormSizeChanged: TCheckBox;
    cbFormReleased: TCheckBox;
    Button1: TButton;
    cbMainFormChanged: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    cbMainFormCaptionChanged: TCheckBox;
    cbStyleChanged: TCheckBox;
    Button4: TButton;
    StyleBookResource: TStyleBook;
    procedure fgApplicationEventsActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure fgApplicationEventsActionUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure fgApplicationEventsException(Sender: TObject; E: Exception);
    procedure fgApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure fgApplicationEventsOrientationChanged(const AOrientation: TScreenOrientation);
    function fgApplicationEventsStateChange(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    procedure BtnClearLogClick(Sender: TObject);
    procedure fgApplicationEventsFormsCreated(Sender: TObject);
    procedure fgApplicationEventsFormSizeChanged(Sender: TObject; const AForm: TCommonCustomForm);
    procedure fgApplicationEventsFormReleased(Sender: TObject; const AForm: TCommonCustomForm);
    procedure Button1Click(Sender: TObject);
    procedure fgApplicationEventsMainFormChanged(Sender: TObject; const ANewForm: TCommonCustomForm);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure fgApplicationEventsMainFormCaptionChanged(Sender: TObject; const ANewForm: TCommonCustomForm;
      const ANewCaption: string);
    procedure fgApplicationEventsStyleChanged(Sender: TObject; const AScene: IScene; const AStyleBook: TStyleBook);
    procedure Button4Click(Sender: TObject);
  private
    procedure Log(const AFormat: string; Args: array of const); overload;
    procedure Log(const AFormat: string); overload;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses ChildFrm;

procedure TFormMain.BtnClearLogClick(Sender: TObject);
begin
  mmLog.Lines.Clear;
end;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  if FormChild = nil then
    FormChild := TFormChild.Create(Self);
  FormChild.Show;
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  if FormChild = nil then
    FormChild := TFormChild.Create(Self);
  if FormChild = Application.MainForm then
    Application.MainForm := Self
  else
    Application.MainForm := FormChild;
end;

procedure TFormMain.Button3Click(Sender: TObject);
begin
  if Application.MainForm <> nil then
    Application.MainForm.Caption := Random(100).ToString;
end;

procedure TFormMain.Button4Click(Sender: TObject);
begin
  if StyleBook = nil then
    StyleBook := StyleBookResource
  else
    StyleBook := nil;
end;

procedure TFormMain.fgApplicationEventsActionExecute(Action: TBasicAction; var Handled: Boolean);
begin
  if cbOnActionExecute.IsChecked then
    Log('OnActionExecute');
end;

procedure TFormMain.fgApplicationEventsActionUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  if cbOnActionUpdate.IsChecked then
    Log('OnActionUpdate');
end;

procedure TFormMain.fgApplicationEventsException(Sender: TObject; E: Exception);
begin
  if cbOnException.IsChecked then
    Log('OnException');
end;

procedure TFormMain.fgApplicationEventsFormReleased(Sender: TObject; const AForm: TCommonCustomForm);
begin
  if cbFormReleased.IsChecked then
    Log('OnFormReleased: name="%s"', [AForm.Name]);
end;

procedure TFormMain.fgApplicationEventsFormsCreated(Sender: TObject);
begin
  if cbAllFormsCreated.IsChecked then
    Log('OnFormsCreated');
end;

procedure TFormMain.fgApplicationEventsFormSizeChanged(Sender: TObject; const AForm: TCommonCustomForm);
begin
  if cbFormSizeChanged.IsChecked then
    Log('OnFormSizeChanged: name="%s" width=%d height=%d', [AForm.Name, AForm.Width, AForm.Height]);
end;

procedure TFormMain.fgApplicationEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if cbOnIdle.IsChecked then
    Log('OnIdle');
end;

procedure TFormMain.fgApplicationEventsMainFormCaptionChanged(Sender: TObject; const ANewForm: TCommonCustomForm;
  const ANewCaption: string);
begin
  if cbMainFormCaptionChanged.IsChecked then
    if ANewForm <> nil then
      Log('OnMainFormCaptionChanged: name="%s" caption="%s"', [ANewForm.Name, ANewForm.Caption])
    else
      Log('OnMainFormCaptionChanged: Main form removed', [])
end;

procedure TFormMain.fgApplicationEventsMainFormChanged(Sender: TObject; const ANewForm: TCommonCustomForm);
begin
  if cbMainFormChanged.IsChecked then
    if ANewForm <> nil then
      Log('OnMainFormChanged: NewForm="%s"', [ANewForm.Name])
    else
      Log('OnMainFormChanged: Main form removed', [])
end;

procedure TFormMain.fgApplicationEventsOrientationChanged(const AOrientation: TScreenOrientation);
var
  OrientationText: string;
begin
  if cbOnOrientationChanged.IsChecked then
  begin
    case AOrientation of
      TScreenOrientation.Portrait: OrientationText := 'Portrait';
      TScreenOrientation.Landscape: OrientationText := 'Landscape';
      TScreenOrientation.InvertedPortrait: OrientationText := 'InvertedPortrait';
      TScreenOrientation.InvertedLandscape: OrientationText := 'InvertedLandscape';
    end;
    Log('OnOrientationChanged=%s', [OrientationText]);
  end;
end;

function TFormMain.fgApplicationEventsStateChange(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
var
  StateText: string;
begin
  if cbOnStateChanged.IsChecked then
  begin
    case AAppEvent of
      TApplicationEvent.FinishedLaunching: StateText := 'FinishedLaunching';
      TApplicationEvent.BecameActive: StateText := 'BecameActive';
      TApplicationEvent.WillBecomeInactive: StateText := 'WillBecomeInactive';
      TApplicationEvent.EnteredBackground: StateText := 'EnteredBackground';
      TApplicationEvent.WillBecomeForeground: StateText := 'WillBecomeForeground';
      TApplicationEvent.WillTerminate: StateText := 'WillTerminate';
      TApplicationEvent.LowMemory: StateText := 'LowMemory';
      TApplicationEvent.TimeChange: StateText := 'TimeChange';
      TApplicationEvent.OpenURL: StateText := 'OpenURL';
    end;
    Log('OnApplicationEventsStateChanged: state=%s', [StateText]);
 end;
  Result := True;
end;

procedure TFormMain.fgApplicationEventsStyleChanged(Sender: TObject; const AScene: IScene;
  const AStyleBook: TStyleBook);
var
  FormName: string;
  StyleBookName: string;
begin
  if cbStyleChanged.IsChecked then
  begin
    if (AScene <> nil) and (AScene.GetObject is TCommonCustomForm) then
      FormName := Format(' name="%s"', [TCommonCustomForm(AScene.GetObject).Name]);
    if AStyleBook <> nil then
      StyleBookName:= Format(' resourceName="%s"', [AStyleBook.Name]);
    Log('Style Book Changed:' + FormName + StyleBookName);
  end;
end;

procedure TFormMain.Log(const AFormat: string);
begin
  mmLog.Lines.Add(Format('%s: ' + AFormat, [TimeToStr(Now)]));
end;

procedure TFormMain.Log(const AFormat: string; Args: array of const);
begin
  mmLog.Lines.Add(TimeToStr(Now) + Format(': ' + AFormat, Args));
end;

end.
