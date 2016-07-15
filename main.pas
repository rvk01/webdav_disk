unit main;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, uwebdav;

// http://www.webdav.org/specs/rfc2518.html
// http://www.webdelphi.ru/2012/04/synapse_webdav/
// http://www.webdelphi.ru/2012/07/yandeks/

// Properties of files and directories (PROPFIND)
// Changing the properties of a file or directory (PROPPATCH)
// Create a directory (MKCOL)
// File Copy (COPY)
// File Move (MOVE)
// Deleting a file (DELETE)
// Download File (PUT)
// File Download (GET)
// LOCK - Zet een slot op het object. WebDAV ondersteuning van exclusieve en gedeelde (gedeeld) lock
// OPEN - Unlock een bron


type

  { TMainForm }

  TMainForm = class(TForm)
    btUpload: TButton;
    edSubdirectory: TEdit;
    Label3: TLabel;
    ListView1: TListView;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    edLogin: TEdit;
    edPassword: TEdit;
    btRetrieve: TButton;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btRetrieveClick(Sender: TObject);
    procedure btUploadClick(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Resources: TWDResourceList;
    WebDAV: TWebDAVDrive;
    procedure Callback_up(Sender: TObject; const ContentLength, CurrentPos: int64);
    procedure Callback_down(Sender: TObject; const ContentLength, CurrentPos: int64);
  end;

var
  MainForm: TMainForm;

implementation

uses DateUtils, fgl;

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Resources := TWDResourceList.Create(True);
  WebDAV := TWebDAVDrive.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Resources.Free;
  WebDAV.Free;
end;

procedure TMainForm.btRetrieveClick(Sender: TObject);
var
  Xml: string;
  I: integer;
begin
  Progressbar1.Position := 0;
  WebDAV.Login := edLogin.Text;
  WebDAV.Password := edPassword.Text;
  WebDAV.Directory := edSubdirectory.Text;
  Resources.Clear;
  ListView1.Clear;
  Xml := WebDAV.PROPFIND(1, cWebDAVSubdir + WebDav.Directory);
  if Length(Trim(Xml)) > 0 then ParseWebDavResources(Xml, Resources);
  for I := 0 to Resources.Count - 1 do
  begin
    with ListView1.Items.Add do
    begin
      Caption := Resources[i].DisplayName;
      SubItems.Add(Resources[i].Href);
      //SubItems.Add(DateTimeToStr(Resources[i].CreationDate));
      //SubItems.Add(DateTimeToStr(Resources[i].Lastmodified));
      SubItems.Add(IntToStr(Resources[i].ContentLength));
      if Resources[i].Collection then
        SubItems.Add('yes')
      else
        SubItems.Add('no');
      SubItems.Add(IntToStr(Resources[i].StatusCode));
    end;
  end;
end;

procedure TMainForm.btUploadClick(Sender: TObject);
var
  Stream: TStream;
  Location: string;
begin
  Progressbar1.Position := 0;
  if OpenDialog1.Execute then
  begin
    WebDAV.Login := edLogin.Text;
    WebDAV.Password := edPassword.Text;
    WebDAV.Directory := edSubdirectory.Text;
    Stream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      Location := cWebDAVSubdir + WebDav.Directory;
      if Copy(Location, Length(Location), 1) <> '/' then Location := Location + '/';
      Location := Location + ExtractFileName(OpenDialog1.Filename);
      if WebDAV.Put(Location, Stream, @Callback_up) then
      begin
        ShowMessage('Uploaded correctly');
        btRetrieve.Click;
      end
      else
        ShowMessage('Error')
    finally
      Stream.Free;
    end;
  end;
end;

procedure TMainForm.ListView1DblClick(Sender: TObject);
var
  Res: TWDResource;
  Stream: TStream;
  Filename, Dir: string;
begin
  Progressbar1.Position := 0;
  if ListView1.ItemIndex > -1 then Res := Resources[ListView1.ItemIndex];
  if Res.Collection then
  begin
    Dir := WebDav.Directory + '/' + Res.DisplayName;
    if (Dir <> '') and (Dir[1] = '/') then System.Delete(Dir, 1, 1);
    edSubdirectory.Text := Dir;
    btRetrieve.Click;
    exit;
  end;
  Filename := ExtractFilePath(Application.ExeName) + Res.DisplayName;
  Stream := TFileStream.Create(Filename, fmCreate or fmOpenWrite);
  try
    if WebDAV.Get(Res.Href, Stream, @Callback_down) then
      Showmessage('Done');
  finally
    Stream.Free;
  end;
end;

procedure TMainForm.Callback_down(Sender: TObject; const ContentLength, CurrentPos: int64);
var
  Max, Pos: Int64;
begin
  Max := ContentLength;
  Pos := CurrentPos;
  if Max > High(Integer) then
  begin
    // TProgressBar.Max is an Integer so Int64 doesn't fit
    // We need to scale down
    Max := High(Integer);
    Pos := Trunc(CurrentPos * (Max / ContentLength));
  end;
  if Progressbar1.Max <> ContentLength then Progressbar1.Max := Max;
  Progressbar1.Position := Pos;
  Application.ProcessMessages;
end;

procedure TMainForm.Callback_up(Sender: TObject; const ContentLength, CurrentPos: int64);
var
  Max, Pos: Int64;
begin
  Max := ContentLength;
  Pos := CurrentPos;
  if Max > High(Integer) then
  begin
    // TProgressBar.Max is an Integer so Int64 doesn't fit
    // We need to scale down
    Max := High(Integer);
    Pos := Trunc(CurrentPos * (Max / ContentLength));
  end;
  if Progressbar1.Max <> ContentLength then Progressbar1.Max := Max;
  Progressbar1.Position := Pos;
  Application.ProcessMessages;
end;

end.
