unit FormProgRun;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  SynEditKeyCmds, process, ShellCtrls;

type

  { TFProgRun }

  TFProgRun = class(TForm)
    SynEdit1: TSynEdit;
    procedure FormCreate(Sender: TObject);
  private
    FProc: TProcess;
    procedure AppendLine(Line: String);
  public
    procedure Run(Node: TShellTreeNode; Cmd: String; Args: array of string);
  end;


  { TProcess2 }

  TProcess2 = class(TProcess)
    constructor Create(AOwner: TComponent; Path, Cmd: String; Args: array of String); reintroduce;
    procedure EnvUpdate(key, Value: String);
  end;


var
  FProgRun: TFProgRun;

implementation
uses
  FormMain;

{$R *.lfm}

{ TProcess2 }

constructor TProcess2.Create(AOwner: TComponent; Path, Cmd: String; Args: array of String);
var
  I: Integer;
  A: String;
begin
  inherited Create(AOwner);
  CurrentDirectory := Path;
  Options := [poNewProcessGroup];
  Executable := cmd;
  for A in args do begin
    Parameters.Add(A);
  end;
  for I := 0 to GetEnvironmentVariableCount -1 do begin
    Environment.Append(GetEnvironmentString(I));
  end;
  EnvUpdate('LANG', 'C');
  EnvUpdate('GIT_TERMINAL_PROMPT', '0');
end;

procedure TProcess2.EnvUpdate(key, Value: String);
var
  I: Integer;
begin
  Key += '='{%H-};
  for I := 0 to Environment.Count - 1 do begin
    if Pos(Key, Environment[I]) = 1 then begin
      Environment[I] := Key + Value;
      Exit;
    end;
  end;
  Environment.Append(Key + Value);
end;

{ TFProgRun }

procedure TFProgRun.FormCreate(Sender: TObject);
begin
  FProc := TProcess.Create(self);
end;

procedure TFProgRun.AppendLine(Line: String);
begin
  SynEdit1.Append(Line);
  SynEdit1.ExecuteCommand(ecEditorBottom, #0, nil);
  SynEdit1.ExecuteCommand(ecLineStart, #0, nil);
end;

procedure TFProgRun.Run(Node: TShellTreeNode; Cmd: String; Args: array of string);
var
  A: String;
begin
  if not Showing then
    Show;
  FProc.Executable := Cmd;
  FProc.Parameters.Clear;
  for A in Args do begin
    FProc.Parameters.Append(A);
  end;
end;

end.

