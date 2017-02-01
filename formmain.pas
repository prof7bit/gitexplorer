unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  ComCtrls, ExtCtrls, Menus, process, gqueue, syncobjs;

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
    ImageList: TImageList;
    NodeMenu: TPopupMenu;
    TreeView: TShellTreeView;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure UpdateTimerTimer(Sender: TObject);
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
  AppTerminating: Boolean;

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
  ConsoleOutput := '';
  repeat
    Sleep(1);
    while P.Output.NumBytesAvailable > 0 do
      ConsoleOutput += Chr(P.Output.ReadByte);
    if ThreadID = MainThreadID then
      Application.ProcessMessages;
    if (Now > EndTime) or AppTerminating then
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
  until AppTerminating;
end;

{ TFMain }

procedure TFMain.UpdateTimerTimer(Sender: TObject);
var
  N: TShellTreeNode;
begin
  N := TShellTreeNode(TreeView.TopItem);
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
  UpdateAllNodes(TreeView.TopItem);
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  FQueueLock := syncobjs.TCriticalSection.Create;
  FUpdateQueue := TNodeQueue.Create;
  TreeView.Root := GetUserDir;
  FUpdateThread := TUpdateThread.Create(False);
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  AppTerminating := True;
  FUpdateThread.WaitFor;
  FUpdateQueue.Free;
  FQueueLock.Free;
end;

procedure TFMain.TreeViewExpanded(Sender: TObject; Node: TTreeNode);
begin
  UpdateAllNodes(Node);
end;

procedure TFMain.TreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
var
  PI: PtrInt;
begin
  PI := {%H-}PtrInt(Node.Data);
  if PI > 0 then begin
    Node.ImageIndex := PI - 1;
  end;
end;

procedure TFMain.TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
var
  PI: PtrInt;
begin
  PI := {%H-}PtrInt(Node.Data);
  if PI > 0 then begin
    Node.SelectedIndex := PI - 1;
  end;
end;

end.

