unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  ComCtrls, ExtCtrls, process, gqueue, syncobjs;

const
  ICON_NORMAL   = 0;
  ICON_CHANGED  = 1;
  ICON_CONFLICT = 2;

  MILLISEC      = 1 / (24 * 60 * 60 * 1000);

type
  TNodeQueue = specialize TQueue<TShellTreeNode>;

  { TUpdateThread }

  TUpdateThread = class(TThread)
    procedure Execute; override;
  end;

  { TFMain }

  TFMain = class(TForm)
    ImageList1: TImageList;
    ShellTreeView1: TShellTreeView;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShellTreeView1Expanded(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeView1GetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeView1GetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    FQueueLock: TCriticalSection;
    FUpdateQueue: TNodeQueue;
    FUpdateThread: TUpdateThread;
    procedure QueryStatus(N: TShellTreeNode; QueueUpdate: Boolean);
    procedure AsyncQueryStatus(P: PtrInt);
    procedure UpdateAllNodes(Root: TTreeNode);
    function GitExe: String;
    { private declarations }
  public
    { public declarations }
  end;

var
  FMain: TFMain;

implementation
{$ifdef windows}
uses
  Windows;
{$endif}

procedure Print(Txt: String);
begin
  {$ifdef windows}
  OutputDebugString(PChar(Txt));
  {$else}
  Writeln(Txt);
  {$endif}
end;

function RunTool(Path: String; cmd: String; args: array of string; out ConsoleOutput: String): Boolean;
var
  P: TProcess;
  A: String;
  EndTime: TDateTime;
begin
  EndTime := Now + 5000 * MILLISEC;
  P := TProcess.Create(nil);
  P.CurrentDirectory := Path;
  P.Options := [poUsePipes, poNoConsole];
  P.Executable := cmd;
  P.Environment.Add('LANG=C');
  for A in args do
    P.Parameters.Add(A);
  P.Execute;
  repeat
    Sleep(1);
    while P.Output.NumBytesAvailable > 0 do
      ConsoleOutput += Chr(P.Output.ReadByte);
    if ThreadID = MainThreadID then
      Application.ProcessMessages;
    if Now > EndTime then
      P.Terminate(1);
  until not (P.Running or (P.Output.NumBytesAvailable > 0));
  Result := (P.ExitCode = 0);
  P.Free;
end;

{$R *.lfm}

{ TUpdateThread }

procedure TUpdateThread.Execute;
var
  Node: TShellTreeNode;
  O: String;
begin
  repeat
    if FMain.FUpdateQueue.Size() > 0 then begin
      FMain.FQueueLock.Acquire;
      Node := FMain.FUpdateQueue.Front();
      FMain.FUpdateQueue.Pop();
      FMain.FQueueLock.Release;
      Print('updating: ' + Node.ShortFilename);
      if RunTool(Node.FullFilename, Fmain.GitExe, ['remote', 'update'], O) then begin
        Application.QueueAsyncCall(@FMain.AsyncQueryStatus, PtrInt(Node));
      end;
    end
    else
      Sleep(1);
  until Terminated;
end;

{ TFMain }

procedure TFMain.ShellTreeView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  N: TTreeNode;

begin
  if Button = mbRight then begin
    N := ShellTreeView1.GetNodeAt(X, Y);
    if Assigned(N) then
       N.Selected := True;
  end;
end;

procedure TFMain.Timer1Timer(Sender: TObject);
var
  N: TShellTreeNode;
begin
  N := TShellTreeNode(ShellTreeView1.TopItem);
  while Assigned(N) do begin
    QueryStatus(N, True);
    N := TShellTreeNode(N.GetNext);
  end;
end;

procedure TFMain.QueryStatus(N: TShellTreeNode; QueueUpdate: Boolean);
var
  Path: String;
  O: String;
  Behind: Boolean = False;
  Ahead: Boolean = False;
  Clean: Boolean = False;
begin
  Path := N.FullFilename;
  if DirectoryExists(Path + DirectorySeparator + '.git') then begin
    if RunTool(Path, GitExe, ['status'], O) then begin
      N.Text := N.ShortFilename;
      if Pos('behind', O) > 0 then
        Behind := True;
      if Pos('ahead', O) > 0 then
        Ahead := True;
      if Pos('clean', O) > 0 then
        Clean := True;
      if Behind then
        N.Text := '[BEHIND] ' + N.Text;
      if Ahead then
        N.Text := '[AHEAD] ' + N.Text;
      if not Clean then
        N.Text := '[DIRTY] ' + N.Text;
    end;
    if Clean and not (Behind or Ahead) then begin
      N.Data := Pointer(ICON_NORMAL + 1);
    end
    else begin
      N.Data := Pointer(ICON_CHANGED + 1);
    end;

    if QueueUpdate then begin
      FQueueLock.Acquire;
      FUpdateQueue.Push(N);
      FQueueLock.Release;
    end;
  end;
end;

procedure TFMain.AsyncQueryStatus(P: PtrInt);
begin
  QueryStatus(TShellTreeNode(P), False);
end;

procedure TFMain.UpdateAllNodes(Root: TTreeNode);
var
  N: TShellTreeNode;
begin
  N := TShellTreeNode(Root.GetFirstChild);
  while Assigned(N) do begin
    QueryStatus(N, True);
    N := TShellTreeNode(N.GetNextSibling);
  end;
end;

function TFMain.GitExe: String;
begin
  {$ifdef windows}
  Result :=  'c:\Program Files (x86)\Git\bin\git.exe';
  {$else}
  Result :=  'git';
  {$endif}
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  UpdateAllNodes(ShellTreeView1.TopItem);
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  FQueueLock := syncobjs.TCriticalSection.Create;
  FUpdateQueue := TNodeQueue.Create;
  ShellTreeView1.Root := GetUserDir;
  FUpdateThread := TUpdateThread.Create(False);
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  FUpdateQueue.Free;
  FQueueLock.Free;
end;

procedure TFMain.ShellTreeView1Expanded(Sender: TObject; Node: TTreeNode);
begin
  UpdateAllNodes(Node);
end;

procedure TFMain.ShellTreeView1GetImageIndex(Sender: TObject; Node: TTreeNode);
var
  PI: PtrInt;
begin
  PI := PtrInt(Node.Data);
  if PI > 0 then begin
    Node.ImageIndex := PI - 1;
  end;
end;

procedure TFMain.ShellTreeView1GetSelectedIndex(Sender: TObject; Node: TTreeNode);
var
  PI: PtrInt;
begin
  PI := PtrInt(Node.Data);
  if PI > 0 then begin
    Node.SelectedIndex := PI - 1;
  end;
end;

end.

