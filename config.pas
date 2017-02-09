unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TConfigIndex = (
    cfWindowX,
    cfWindowY,
    cfWindowW,
    cfWindowH,
    cfConsoleWindowX,
    cfConsoleWindowY,
    cfConsoleWindowW,
    cfConsoleWindowH
  );

function ConfigGetStr(I: TConfigIndex): String;
function ConfigGetInt(I: TConfigIndex): Integer;
procedure ConfigSetStr(I: TConfigIndex; Value: String);
procedure ConfigSetInt(I: TConfigIndex; Value: Integer);
procedure ConfigSave;

implementation

var
  Ini: TIniFile = nil;

function GetSection(I: TConfigIndex): String;
begin
  case I of
  cfWindowX, cfWindowY, cfWindowW, cfWindowH,
  cfConsoleWindowX, cfConsoleWindowY, cfConsoleWindowW, cfConsoleWindowH:
    Result := 'UI';
  end;
end;

function GetKey(I: TConfigIndex): String;
begin
  case I of
  cfWindowX: Result := 'WindowX';
  cfWindowY: Result := 'WindowY';
  cfWindowW: Result := 'WindowW';
  cfWindowH: Result := 'WindowH';
  cfConsoleWindowX: Result := 'ConsoleWindowX';
  cfConsoleWindowY: Result := 'ConsoleWindowY';
  cfConsoleWindowW: Result := 'ConsoleWindowW';
  cfConsoleWindowH: Result := 'ConsoleWindowH';
  end;
end;

function GetDefault(I: TConfigIndex): String;
begin
  case I of
  cfWindowX: Result := '10';
  cfWindowY: Result := '10';
  cfWindowW: Result := '270';
  cfWindowH: Result := '500';
  cfConsoleWindowX: Result := '300';
  cfConsoleWindowY: Result := '10';
  cfConsoleWindowW: Result := '480';
  cfConsoleWindowH: Result := '340';
  end;
end;

function GetIniFileName: String;
var
  Dir: String;
begin
  Dir := GetUserDir + DirectorySeparator + '.config';
  if not DirectoryExists(Dir) then
    CreateDir(Dir);
  Result := Dir + DirectorySeparator + 'gitexplorer.ini';
end;

procedure ConfigLoad;
begin
  if not Assigned(Ini) then begin
    Ini := TIniFile.Create(GetIniFileName);
    Ini.CacheUpdates := True;
  end;
end;

function ConfigGetStr(I: TConfigIndex): String;
begin
  ConfigLoad;
  Result := Ini.ReadString(GetSection(I), GetKey(I), GetDefault(I));
end;

function ConfigGetInt(I: TConfigIndex): Integer;
begin
  ConfigLoad;
  Result := Ini.ReadInteger(GetSection(I), GetKey(I), StrToInt(GetDefault(I)));
end;

procedure ConfigSetStr(I: TConfigIndex; Value: String);
begin
  Ini.WriteString(GetSection(I), GetKey(I), Value);
end;

procedure ConfigSetInt(I: TConfigIndex; Value: Integer);
begin
  Ini.WriteInteger(GetSection(I), GetKey(I), Value);
end;

procedure ConfigSave;
begin
  Ini.UpdateFile;
end;

finalization
  if Assigned(Ini) then begin
    Ini.UpdateFile;
    Ini.Free;
  end;
end.

