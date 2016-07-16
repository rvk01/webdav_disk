unit main;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, DBCtrls, Grids, uwebdav;

// http://www.webdav.org/specs/rfc2518.html
// http://www.webdelphi.ru/2012/04/synapse_webdav/
// http://www.webdelphi.ru/2012/07/yandeks/

type

  { TMainForm }

  TMainForm = class(TForm)
    btUpload: TButton;
    btDelete: TButton;
    edSubdirectory: TEdit;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    edLogin: TEdit;
    edPassword: TEdit;
    btRetrieve: TButton;
    ProgressBar1: TProgressBar;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btRetrieveClick(Sender: TObject);
    procedure btUploadClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
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
  StringGrid1.RowCount := 1;
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
  StringGrid1.RowCount := 1;
  Xml := WebDAV.PROPFIND(1, cWebDAVSubdir + WebDav.Directory);
  if Length(Trim(Xml)) > 0 then
    ParseWebDavResources(Xml, Resources);
  StringGrid1.RowCount := Resources.Count + 1;
  for I := 0 to Resources.Count - 1 do
  begin
    StringGrid1.Cells[0, I] := Resources[i].DisplayName;
    StringGrid1.Cells[1, I] := Resources[i].Href;
    //StringGrid1.Cells[0, I] := DateTimeToStr(Resources[i].CreationDate);
    //StringGrid1.Cells[0, I] := DateTimeToStr(Resources[i].Lastmodified);
    StringGrid1.Cells[2, I] := IntToStr(Resources[i].ContentLength);
    if Resources[i].Collection then
      StringGrid1.Cells[3, I] := 'yes'
    else
      StringGrid1.Cells[3, I] := 'no';
    StringGrid1.Cells[4, I] := IntToStr(Resources[i].StatusCode);
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
      if Copy(Location, Length(Location), 1) <> '/' then
        Location := Location + '/';
      Location := Location + ExtractFileName(OpenDialog1.Filename);
      if WebDAV.Put(Location, Stream, @Callback_up) then
      begin
        ShowMessage('Uploaded');
        btRetrieve.Click;
      end
      else
        ShowMessage('Error')
    finally
      Stream.Free;
    end;
  end;
end;

procedure TMainForm.btDeleteClick(Sender: TObject);
var
  Res: TWDResource;
begin
  if StringGrid1.Row < 1 then exit;
  Res := Resources[StringGrid1.Row - 1];
  if not Res.Collection then
    if WebDAV.Delete(Res.Href) then
      btRetrieve.Click;
end;

procedure TMainForm.ListView1DblClick(Sender: TObject);
var
  Res: TWDResource;
  Stream: TStream;
  Filename, Dir: string;
begin
  if StringGrid1.Row < 1 then exit;
  Res := Resources[StringGrid1.Row - 1];
  if Res.Collection then
  begin
    Dir := WebDav.Directory + '/' + Res.DisplayName;
    if (Dir <> '') and (Dir[1] = '/') then
      System.Delete(Dir, 1, 1);
    edSubdirectory.Text := Dir;
    btRetrieve.Click;
    exit;
  end;
  Progressbar1.Position := 0;
  Filename := ExtractFilePath(Application.ExeName) + Res.DisplayName;
  Stream := TFileStream.Create(Filename, fmCreate or fmOpenWrite);
  try
    if WebDAV.Get(Res.Href, Stream, @Callback_down) then
      ShowMessage('Downloaded');
  finally
    Stream.Free;
  end;
end;

procedure TMainForm.Callback_down(Sender: TObject; const ContentLength, CurrentPos: int64);
var
  Max, Pos: int64;
begin
  Max := ContentLength;
  Pos := CurrentPos;
  if Max > High(integer) then
  begin
    // TProgressBar.Max is an Integer so Int64 doesn't fit
    // We need to scale down
    Max := High(integer);
    Pos := Trunc(CurrentPos * (Max / ContentLength));
  end;
  if Progressbar1.Max <> ContentLength then Progressbar1.Max := Max;
  Progressbar1.Position := Pos;
  Application.ProcessMessages;
end;

procedure TMainForm.Callback_up(Sender: TObject; const ContentLength, CurrentPos: int64);
var
  Max, Pos: int64;
begin
  Max := ContentLength;
  Pos := CurrentPos;
  if Max > High(integer) then
  begin
    // TProgressBar.Max is an Integer so Int64 doesn't fit
    // We need to scale down
    Max := High(integer);
    Pos := Trunc(CurrentPos * (Max / ContentLength));
  end;
  if Progressbar1.Max <> ContentLength then Progressbar1.Max := Max;
  Progressbar1.Position := Pos;
  Application.ProcessMessages;
end;

end.
