unit FormProgRun;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  SynEditKeyCmds, process, ShellCtrls, ExtCtrls, LockedQueue, Pipes;

type
  TStringQueue = specialize TLockedQueue<String>;

  { TRunThread }

  TRunThread = class(TThread)
    ReadingOutputActive: Boolean;
    Node: TShellTreeNode;
    procedure Execute; override;
    procedure Print(Txt: String);
  end;

  { TProcess2 }

  TProcess2 = class(TProcess)
    constructor Create(AOwner: TComponent; Path, Cmd: String; Args: array of String);
    procedure EnvUpdate(key, Value: String);
  end;

  { TFProgRun }

  TFProgRun = class(TForm)
    SynEdit1: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AsyncOnOutputQueue;
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

  Procedure ReadAndPrintLines(var L: String; S: TInputPipeStream);
  var
    B: Char;
  begin
    while S.NumBytesAvailable > 0 do begin
      B := Char(S.ReadByte);
      if B = #10 then begin
        Print(TrimRight(L));
        L := '';
      end
      else begin
        L += B;
      end;
    end;
  end;

begin
  P := FProgRun.FProc;
  Line := '';
  ErrLine := '';

  repeat
    if ReadingOutputActive then begin
      ReadAndPrintLines(Line, P.Output);
      ReadAndPrintLines(ErrLine, P.Stderr);
      if (not P.Running) and (P.Output.NumBytesAvailable + P.Stderr.NumBytesAvailable = 0) then begin
        Print('----');
        ReadingOutputActive := False;
        FMain.QueueForImmediateUpdate(Node);
      end;
    end;
    Sleep(1);
  until Terminated;
end;

procedure TRunThread.Print(Txt: String);
begin
  FProgRun.FOutputQueue.Put(Txt);
  Queue(@FProgRun.AsyncOnOutputQueue);
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
  FRunThread.Terminate;
  FRunThread.WaitFor;
  FRunThread.Free;
  FOutputQueue.Free;
end;

procedure TFProgRun.AsyncOnOutputQueue;
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
  if FRunThread.ReadingOutputActive then
    exit;
  if not Showing then
    Show;
  FProc.CurrentDirectory := Node.FullFilename;
  FProc.Executable := Cmd;
  FProc.Parameters.Clear;
  for A in Args do begin
    FProc.Parameters.Append(A);
  end;
  FRunThread.Node := Node;
  FProc.Execute;
  FRunThread.ReadingOutputActive := True;
end;

end.

