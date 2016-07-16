unit uwebdav;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, fphttpclient;

const
  //cWebDAVServer = 'https://webdav.yandex.ru/';
  cWebDAVServer = 'https://{login}.stackstorage.com/';
  cWebDAVSubdir = 'remote.php/webdav/';

type

  { TWDResource }

  TWDResource = class
  private
    FHref: string;
    FStatusCode: integer;
    FContentLength: int64;
    FCreationDate: TDateTime;
    FLastmodified: TDateTime;
    FDisplayName: string;
    FContentType: string;
    FCollection: boolean;
  public
    property Href: string read FHref write FHref;
    property StatusCode: integer read FStatusCode write FStatusCode;
    property ContentLength: int64 read FContentLength write FContentLength;
    property CreationDate: TDateTime read FCreationDate write FCreationDate;
    property Lastmodified: TDateTime read FLastmodified write FLastmodified;
    property DisplayName: string read FDisplayName write FDisplayName;
    property ContentType: string read FContentType write FContentType;
    property Collection: boolean read FCollection write FCollection;
  end;

  TWDResourceList = specialize TFPGObjectList<TWDResource>;

type
  TWebDAVDrive = class
  private
    FLogin: string;
    FPassword: string;
    FDirectory: string;
    function EncodeUTF8URI(const URI: string): string;
    function GetRequestURL(const Element: string; EncodePath: boolean = False): string;
  public
    constructor Create;
    destructor Destroy; override;
    property Login: string read FLogin write FLogin;
    property Password: string read FPassword write FPassword;
    property Directory: string read FDirectory write FDirectory;

    // GET - Download a file
    function GET(const ElementHref: string; var Stream: TStream; Callback: TDataEvent): boolean;
    // PUT - Upload a file
    function PUT(const ElementHref: string; var Stream: TStream; Callback: TDataEvent): boolean;
    // DELETE - Delete a file
    function Delete(const ElementHref: string): boolean;
    // PROPFIND - Properties of files and directories
    // COPY - Copy a file
    // not implemented
    // MOVE - Move a file
    // not implemented
    function PROPFIND(Depth: integer {0/1}; const Element: string): string;
    // MKCOL - Create a directory
    function MKCOL(const ElementPath: string): boolean;
    // PROPPATCH - Change the properties of a file or directory
    // not implemented
    // LOCK - Zet een slot op het object
    // not implemented
    // OPEN - Unlock een bron
    // not implemented

  end;

procedure ParseWebDavResources(const AXMLStr: string; var Resources: TWDResourceList);

implementation

uses Dialogs, laz2_XMLRead, laz2_DOM,
  ufphttphelper {ALWAYS LAST until fphttpclient is fixed};

{ TTWebDAVDrive }

constructor TWebDAVDrive.Create;
begin
  inherited;
end;

destructor TWebDAVDrive.Destroy;
begin
  inherited;
end;

type
  TSpecials = set of AnsiChar;

const
  URLSpecialChar: TSpecials =
    [#$00..#$20, '<', '>', '"', '%', '{', '}', '|', '\', '^', '[', ']', '`', #$7F..#$FF];

function TWebDAVDrive.EncodeUTF8URI(const URI: string): string;
var
  i: integer;
  char: AnsiChar;
begin
  Result := '';
  for i := 1 to length(URI) do
  begin
    if (URI[i] in URLSpecialChar) then
    begin
      for char in UTF8String(URI[i]) do
        Result := Result + '%' + IntToHex(Ord(char), 2);
    end
    else
      Result := Result + URI[i];
  end;
end;

function TWebDAVDrive.GetRequestURL(const Element: string; EncodePath: boolean): string;
var
  URI: string;
  WebDavStr: string;
begin
  WebDavStr := StringReplace(cWebDAVServer, '{login}', Login, []);
  if Length(Element) > 0 then
  begin
    URI := Element;
    if URI[1] = '/' then
      System.Delete(URI, 1, 1);
    if EncodePath then
      Result := WebDavStr + EncodeUTF8URI(URI)
    else
      Result := WebDavStr + URI;
  end
  else
    Result := WebDavStr + cWebDAVSubdir;
end;


function TWebDAVDrive.GET(const ElementHref: string; var Stream: TStream;
  Callback: TDataEvent): boolean;
var
  URL: string;
  HTTP: TFPHTTPClient;
begin
  Result := False;
  if not Assigned(Stream) then exit;
  URL := GetRequestURL(ElementHref, False);
  HTTP := TFPHTTPClient.Create(nil);
  try
    HTTP.UserName := Login;
    HTTP.Password := Password;
    HTTP.RequestHeaders.Add('Accept: */*');
    HTTP.RequestHeaders.Add('Connection: Close');
    HTTP.OnDataReceived := Callback;
    try
      HTTP.Get(URL, Stream);
      Result := True;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  finally
    HTTP.Free;
  end;
end;

function TWebDAVDrive.PUT(const ElementHref: string; var Stream: TStream; Callback: TDataEvent): boolean;
var
  URL, Rs: string;
  HTTP: TFPHTTPClient;
begin
  Result := False;
  if not Assigned(Stream) then Exit;
  URL := GetRequestURL(ElementHref, True);
  HTTP := TFPHTTPClient.Create(nil);
  try
    HTTP.RequestBody := Stream;
    HTTP.UserName := Login;
    HTTP.Password := Password;
    HTTP.RequestHeaders.Add('Accept: */*');
    HTTP.RequestHeaders.Add('Content-Type: application/binary');
    HTTP.RequestHeaders.Add('Connection: Close');
    HTTP.OnDataSent := Callback;
    Rs := HTTP.Put(URL);
    // ShowMessage(Rs); // created
    Result := True;
  finally
    HTTP.Free;
  end;
end;

function TWebDAVDrive.DELETE(const ElementHref: string): boolean;
var
  HTTP: TFPHTTPClient;
  SS: TStringStream;
begin
  Result := False;
  HTTP := TFPHTTPClient.Create(nil);
  SS := TStringStream.Create('');
  try
    HTTP.UserName := Login;
    HTTP.Password := Password;
    HTTP.RequestHeaders.Add('Accept: */*');
    HTTP.RequestHeaders.Add('Connection: Close');
    HTTP.HTTPMethod('DELETE', GetRequestURL(ElementHref), SS, [204]);
    Result := True;
    //Result := SS.Datastring;
  finally
    SS.Free;
    HTTP.Free;
  end;
end;

function TWebDAVDrive.MKCOL(const ElementPath: string): boolean;
var
  HTTP: TFPHTTPClient;
  SS: TStringStream;
begin
  Result := False;
  HTTP := TFPHTTPClient.Create(nil);
  SS := TStringStream.Create('');
  try
    HTTP.UserName := Login;
    HTTP.Password := Password;
    HTTP.RequestHeaders.Add('Accept: */*');
    HTTP.RequestHeaders.Add('Connection: Close');
    HTTP.HTTPMethod('MKCOL', GetRequestURL(ElementPath), SS, [201]);
    //Result := SS.Datastring;
  finally
    SS.Free;
    HTTP.Free;
  end;
end;

function TWebDAVDrive.PROPFIND(Depth: integer; const Element: string): string;
var
  HTTP: TFPHTTPClient;
  SS: TStringStream;
begin
  HTTP := TFPHTTPClient.Create(nil);
  SS := TStringStream.Create('');
  try
    HTTP.UserName := Login;
    HTTP.Password := Password;
    HTTP.RequestHeaders.Add('Accept: */*');
    HTTP.RequestHeaders.Add('Connection: Close');
    HTTP.RequestHeaders.Add('Depth: ' + IntToStr(Depth));
    HTTP.HTTPMethod('PROPFIND', GetRequestURL(Element), SS, [201, 207]);
    Result := SS.DataString;
  finally
    SS.Free;
    HTTP.Free;
  end;
end;

{ Support functions }

function SeparateLeft(const Value, Delimiter: string): string;
var
  x: integer;
begin
  x := Pos(Delimiter, Value);
  if x < 1 then
    Result := Value
  else
    Result := Copy(Value, 1, x - 1);
end;

function SeparateRight(const Value, Delimiter: string): string;
var
  x: integer;
begin
  x := Pos(Delimiter, Value);
  if x > 0 then
    x := x + Length(Delimiter) - 1;
  Result := Copy(Value, x + 1, Length(Value) - x);
end;

{ WebDavResources }

procedure ParseWebDavResources(const AXMLStr: string; var Resources: TWDResourceList);
var
  XMLDoc: TXMLDocument;
  ResponseNode, ChildNode, PropNodeChild, PropertyNode: TDOMNode;
  s, su, Value: string;

  procedure ReadXMLString(Value: string);
  var
    S: TStringStream;
  begin
    S := TStringStream.Create(Value);
    try
      ReadXMLFile(XMLDoc, S);
    finally
      S.Free;
    end;
  end;

begin
  // XMLDoc := TXMLDocument.Create; wordt in ReadXMLString gedaan
  //Showmessage(AXMLStr);
  ReadXMLString(AXMLStr); //XMLDoc.LoadFromXML(AXMLStr);
  try
    //if not XMLDoc.IsEmptyDoc then
    begin
      // Select the first node d: response
      ResponseNode := XMLDoc.DocumentElement.FirstChild;
      // ResponseNode:=XMLDoc.DocumentElement.ChildNodes.First;
      while Assigned(ResponseNode) do
      begin
        // Create a new resource record in the list

        Resources.Add(TWDResource.Create);

        // Iterate over the child nodes d: response
        ChildNode := ResponseNode.FirstChild;
        // ChildNode:=ResponseNode.ChildNodes.First;
        while Assigned(ChildNode) do
        begin
          if ChildNode.NodeName = 'd:href' then
            Resources.Last.Href := ChildNode.TextContent // .Text
          else
          // Find node with the resource properties
          if ChildNode.NodeName = 'd:propstat' then
          begin
            // Select the first child node, usually - it is d: status
            PropNodeChild := ChildNode.FirstChild; // .First;
            while Assigned(PropNodeChild) do
            begin
              // Read the status code
              if PropNodeChild.NodeName = 'd:status' then
              begin
                Value := PropNodeChild.TextContent; //.Text;
                s := Trim(SeparateRight(Value, ' '));
                su := Trim(SeparateLeft(s, ' '));
                Resources.Last.StatusCode := StrToIntDef(su, 0);
              end
              else
              // Find node d: prop - are passed on its child nodes
              if PropNodeChild.NodeName = 'd:prop' then
              begin
                PropertyNode := PropNodeChild.FirstChild; //.First;
                while Assigned(PropertyNode) do
                begin
                  if PropertyNode.NodeName = 'd:creationdate' then
                    Resources.Last.CreationDate := 0 // PropertyNode.TextContent not used
                  else if PropertyNode.NodeName = 'd:displayname' then
                    Resources.Last.DisplayName := PropertyNode.TextContent
                  else if PropertyNode.NodeName = 'd:getcontentlength' then
                    Resources.Last.ContentLength :=
                      StrToInt64(PropertyNode.TextContent)
                  else if PropertyNode.NodeName = 'd:getlastmodified' then
                    Resources.Last.Lastmodified :=
                      0  // DecodeRfcDateTime(PropertyNode.TextContent) Fri, 15 Jul 2016 08:33:34 GMT
                  else if PropertyNode.NodeName = 'd:resourcetype' then
                    Resources.Last.Collection := PropertyNode.ChildNodes.Count > 0;

                  // Select the next child node have d: prop
                  PropertyNode := PropertyNode.NextSibling;
                end;
              end;
              // Select the next child node have d: propstat
              PropNodeChild := PropNodeChild.NextSibling;
            end;
          end;
          // Select the next child node have d: response
          ChildNode := ChildNode.NextSibling;
        end;
        // Select the next node d: response
        ResponseNode := ResponseNode.NextSibling;
      end;
    end;
  finally
    XMLDoc.Free;
    XMLDoc := nil;
  end;
end;




end.
