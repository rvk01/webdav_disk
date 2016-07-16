unit ufphttphelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, URIParser;

{ TFPHTTPClient override }
type
  TFPHTTPClient = class(fphttpclient.TFPHTTPClient)
  protected
    FOnDataSent: TDataEvent;
    procedure SendRequest(const AMethod: string; URI: TURI); override;
  public
    property OnDataSent: TDataEvent read FOnDataSent write FOnDataSent;
  end;

implementation
uses base64;

function CopyFromStreamToStream(Source, Destination: TStream; Count: int64;
  Callback: TDataEvent): int64;
var
  Buffer: Pointer;
  BufferSize, i: longint;
  MaxCount: Int64;
const
  MaxSize = $20000;
begin
  Result := 0;
  MaxCount := Count;
  if Count = 0 then
    Source.Position := 0;   // This WILL fail for non-seekable streams...
  BufferSize := MaxSize;
  if (Count > 0) and (Count < BufferSize) then
    BufferSize := Count;    // do not allocate more than needed
  GetMem(Buffer, BufferSize);
  try
    if Count = 0 then
      repeat
        i := Source.Read(buffer^, BufferSize);
        if i > 0 then
          Destination.WriteBuffer(buffer^, i);
        Inc(Result, i);
      until i < BufferSize
    else
      while Count > 0 do
      begin
        if Count > BufferSize then
          i := BufferSize
        else
          i := Count;
        Source.ReadBuffer(buffer^, i);
        Destination.WriteBuffer(buffer^, i);
        Dec(Count, i);
        Inc(Result, i);

        if Assigned(Callback) then
          Callback(nil, MaxCount, Result);

      end;
  finally
    FreeMem(Buffer);
  end;
end;

procedure TFPHTTPClient.SendRequest(const AMethod: string; URI: TURI);
const
  CRLF = #13#10;
var
  UN, PW, S, L: string;
  I: integer;
begin
  S := Uppercase(AMethod) + ' ' + GetServerURL(URI) + ' ' + 'HTTP/' + HTTPVersion + CRLF;
  UN := URI.Username;
  PW := URI.Password;
  if (UserName <> '') then
  begin
    UN := UserName;
    PW := Password;
  end;
  if (UN <> '') then
  begin
    S := S + 'Authorization: Basic ' + EncodeStringBase64(UN + ':' + PW) + CRLF;
    I := IndexOfHeader('Authorization');
    if I <> -1 then
      RequestHeaders.Delete(i);
  end;
  S := S + 'Host: ' + URI.Host;
  if (URI.Port <> 0) then
    S := S + ':' + IntToStr(URI.Port);
  S := S + CRLF;
  if Assigned(RequestBody) and (IndexOfHeader('Content-Length') = -1) then
    AddHeader('Content-Length', IntToStr(RequestBody.Size));
  for I := 0 to RequestHeaders.Count - 1 do
  begin
    l := RequestHeaders[i];
    if AllowHeader(L) then
      S := S + L + CRLF;
  end;
  S := S + CRLF;
  Socket.WriteBuffer(S[1], Length(S));
  if Assigned(RequestBody) then
    CopyFromStreamToStream(RequestBody, Socket, RequestBody.Size, Self.OnDataSent);
  //Socket.CopyFrom(RequestBody, RequestBody.Size);
end;

end.

