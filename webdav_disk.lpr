program webdav_disk;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces,
  main in 'main.pas' {MainForm}, uwebdav;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
