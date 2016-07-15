unit uWebDAV;

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
  TWebDAVSend = class
  private
    FToken: ansistring;
    FLogin: string;
    FPassword: string;
    FDirectory: string;
    procedure SetLogin(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetToken;
    function EncodeUTF8URI(const URI: string): string;
    function GetRequestURL(const Element: string; EncodePath: boolean = False): string;
  public
    constructor Create;
    destructor Destroy; override;
    function PROPFIND(Depth: integer {0/1}; const Element: string): string;
    function MKCOL(const ElementPath: string): boolean;
    function Get(const ElementHref: string; var Response: TStream; Callback: TDataEvent): boolean;
    function Put(const ElementHref: string; var Response: TStream; Callback: TDataEvent): boolean;
    property Login: string read FLogin write SetLogin;
    property Password: string read FPassword write SetPassword;
    property Directory: string read FDirectory write FDirectory;
  end;

procedure ParseWebDavResources(const AXMLStr: string; var Resources: TWDResourceList);

implementation

uses Dialogs, base64, laz2_XMLRead, laz2_DOM;

{ TWebDAVSend }

constructor TWebDAVSend.Create;
begin
  inherited;
end;

destructor TWebDAVSend.Destroy;
begin
  inherited;
end;

type
  TSpecials = set of AnsiChar;

const
  URLSpecialChar: TSpecials = [#$00..#$20, '<', '>', '"', '%', '{', '}', '|', '\', '^', '[', ']', '`', #$7F..#$FF];

function TWebDAVSend.EncodeUTF8URI(const URI: string): string;
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

function TWebDAVSend.Get(const ElementHref: string; var Response: TStream; Callback: TDataEvent): boolean;
var
  URL: string;
  HTTP: TFPHTTPClient;
begin
  Result := false;
  if not Assigned(Response) then
    Exit;
  URL := GetRequestURL(ElementHref, false);
  HTTP := TFPHTTPClient.Create(nil);
  try
    HTTP.RequestHeaders.Add('Authorization: Basic ' + FToken);
    HTTP.RequestHeaders.Add('Accept: */*');
    HTTP.OnDataReceived := Callback;
    try
      HTTP.Get(URL, Response);
      Result := true;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  finally
    HTTP.Free;
  end;
end;

function TWebDAVSend.Put(const ElementHref: string; var Response: TStream; Callback: TDataEvent): boolean;
var
  URL, Rs: string;
  HTTP: TFPHTTPClient;
begin
  Result := false;
  if not Assigned(Response) then
    Exit;
  URL := GetRequestURL(ElementHref, true);
  HTTP := TFPHTTPClient.Create(nil);
  try
    HTTP.RequestBody := Response;
    HTTP.RequestHeaders.Add('Authorization: Basic ' + FToken);
    HTTP.RequestHeaders.Add('Accept: */*');
    HTTP.RequestHeaders.Add('Content-Type: application/binary');
    HTTP.RequestHeaders.Add('Connection: Close');
    Rs := HTTP.Put(URL);
    Showmessage(Rs);
    Result := true;
  finally
    HTTP.Free;
  end;
end;

function TWebDAVSend.GetRequestURL(const Element: string; EncodePath: boolean): string;
var
  URI: string;
  WebDavStr: string;
begin
  WebDavStr := StringReplace(cWebDAVServer, '{login}', Login, []);
  if Length(Element) > 0 then
  begin
    URI := Element;
    if URI[1] = '/' then
      Delete(URI, 1, 1);
    if EncodePath then
      Result := WebDavStr + EncodeUTF8URI(URI)
    else
      Result := WebDavStr + URI;
  end
  else
    Result := WebDavStr + cWebDAVSubdir;
end;



function TWebDAVSend.MKCOL(const ElementPath: string): boolean;
var
  HTTP: TFPHTTPClient;
  SS : TStringStream;
begin
  Result := False;
  HTTP := TFPHTTPClient.Create(nil);
  SS:=TStringStream.Create('');
  try
    HTTP.RequestHeaders.Add('Authorization: Basic ' + FToken);
    HTTP.RequestHeaders.Add('Accept: */*');
    HTTP.HTTPMethod('MKCOL', GetRequestURL(ElementPath), SS, [201]);
    //Result := SS.Datastring;
  finally
    SS.Free;
    HTTP.Free;
  end;
end;

function TWebDAVSend.PROPFIND(Depth: integer; const Element: string): string;
var
  HTTP: TFPHTTPClient;
  SS : TStringStream;
begin
  HTTP := TFPHTTPClient.Create(nil);
  SS:=TStringStream.Create('');
  try
    HTTP.RequestHeaders.Add('Authorization: Basic ' + FToken);
    HTTP.RequestHeaders.Add('Accept: */*');
    HTTP.RequestHeaders.Add('Depth: ' + IntToStr(Depth));
    HTTP.HTTPMethod('PROPFIND', GetRequestURL(Element), SS, [201, 207]);
    Result := SS.DataString;
  finally
    SS.Free;
    HTTP.Free;
  end;
end;

procedure TWebDAVSend.SetToken;
begin
  FToken := EncodeStringBase64(FLogin + ':' + FPassword);
end;

procedure TWebDAVSend.SetLogin(const Value: string);
begin
  FLogin := Value;
  SetToken;
end;

procedure TWebDAVSend.SetPassword(const Value: string);
begin
  FPassword := Value;
  SetToken;
end;


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
                    Resources.Last.Lastmodified := 0  // DecodeRfcDateTime(PropertyNode.TextContent) Fri, 15 Jul 2016 08:33:34 GMT
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
