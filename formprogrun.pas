unit FormProgRun;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  SynEditKeyCmds, process, ShellCtrls, ExtCtrls, LockedQueue;

type
  TStringQueue = specialize TLockedQueue<String>;

  { TRunThread }

  TRunThread = class(TThread)
    procedure Execute; override;
  end;

  { TProcess2 }

  TProcess2 = class(TProcess)
    constructor Create(AOwner: TComponent; Path, Cmd: String; Args: array of String);
    procedure EnvUpdate(key, Value: String);
  end;

  { TFProgRun }

  TFProgRun = class(TForm)
    SynEdit1: TSynEdit;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FProc: TProcess2;
    FOutputQueue: TStringQueue;
    FRunThread: TRunThread;
    procedure AppendLine(Line: String);
  public
    procedure Run(Node: TShellTreeNode; Cmd: String; Args: array of string);
  end;



var
  FProgRun: TFProgRun;

implementation
uses
  FormMain;

{$R *.lfm}

{ TRunThread }

procedure TRunThread.Execute;
var
  P: TProcess;
  Line: String;
  ErrLine: String;
begin
  P := FProgRun.FProc;
  Line := '';
  ErrLine := '';
  repeat
    while P.Output.NumBytesAvailable > 0 do begin
      Line += Chr(P.Output.ReadByte);
      if RightStr(Line, Length(LineEnding)) = LineEnding then begin
        FProgRun.FOutputQueue.Put(Trim(Line));
      end;
    end;
    while P.Stderr.NumBytesAvailable > 0 do begin
      ErrLine += Chr(P.Stderr.ReadByte);
      if RightStr(ErrLine, Length(LineEnding)) = LineEnding then begin
        FProgRun.FOutputQueue.Put(Trim(ErrLine));
      end;
    end;
    Sleep(1);
  until Terminated;
end;

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
  FProc := TProcess2.Create(self, '', '', []);
  FProc.Options := FProc.Options + [poUsePipes, poNoConsole];
  FOutputQueue := TStringQueue.Create;
  FRunThread := TRunThread.Create(False);
end;

procedure TFProgRun.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  FRunThread.Terminate;
  FRunThread.WaitFor;
  FRunThread.Free;
  FOutputQueue.Free;
end;

procedure TFProgRun.Timer1Timer(Sender: TObject);
var
  Line: String;
begin
  while not FOutputQueue.IsEmpty do begin
    FOutputQueue.Get(Line);
    AppendLine(Line);
  end;
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
  FProc.Execute;
end;

end.

