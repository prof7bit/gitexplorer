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
    MenuItemConsole: TMenuItem;
    MenuItemMeld: TMenuItem;
    MenuItemPull: TMenuItem;
    MenuItemCommit: TMenuItem;
    MenuItemPush: TMenuItem;
    NodeMenu: TPopupMenu;
    TreeView: TShellTreeView;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemConsoleClick(Sender: TObject);
    procedure MenuItemMeldClick(Sender: TObject);
    procedure MenuItemPullClick(Sender: TObject);
    procedure MenuItemPushClick(Sender: TObject);
    procedure NodeMenuPopup(Sender: TObject);
    procedure TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FQueueLock: TCriticalSection;
    FUpdateQueue: TNodeQueue;
    FUpdateThread: TUpdateThread;
    procedure QueueForUpdate(Node: TShellTreeNode);
    procedure QueryStatus(N: TShellTreeNode; RemoteUpdate: Boolean);
    procedure AsyncQueryStatus(P: PtrInt);
    procedure UpdateAllNodes(Root: TTreeNode);
    function GitExe: String;
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

procedure StartExe(Path: String; Cmd: String; args: array of string; Wait: Boolean);
var
  P: TProcess;
  I: Integer;
  A: String;
begin
  P := TProcess.Create(nil);
  P.CurrentDirectory := Path;
  P.Options := [poUsePipes, poNoConsole, poNewProcessGroup, poWaitOnExit];
  P.Executable := cmd;
  for I := 0 to GetEnvironmentVariableCount -1 do begin
    if Pos('LANG=', GetEnvironmentString(I)) = 0 then
      P.Environment.Append(GetEnvironmentString(I));
  end;
  P.Environment.Append('LANG=C');
  for A in args do begin
    P.Parameters.Add(A);
  end;
  P.Execute;
  P.Free;
end;

function RunTool(Path: String; cmd: String; args: array of string; out ConsoleOutput: String): Boolean;
var
  P: TProcess;
  A: String;
  I: Integer;
begin
  P := TProcess.Create(nil);
  P.CurrentDirectory := Path;
  P.Options := [poUsePipes, poNoConsole];
  P.Executable := cmd;
  for I := 0 to GetEnvironmentVariableCount -1 do begin
    if Pos('LANG=', GetEnvironmentString(I)) = 0 then
      P.Environment.Append(GetEnvironmentString(I));
  end;
  P.Environment.Append('LANG=C');
  for A in args do begin
    P.Parameters.Add(A);
  end;
  P.Execute;
  ConsoleOutput := '';
  repeat
    Sleep(1);
    if ThreadID = MainThreadID then
      Application.ProcessMessages;
    if AppTerminating then
      P.Terminate(1);
  until not P.Running;
  while P.Output.NumBytesAvailable > 0 do
    ConsoleOutput += Chr(P.Output.ReadByte);
  while P.Stderr.NumBytesAvailable > 0 do
    ConsoleOutput += Chr(P.Stderr.ReadByte);
  Result := (P.ExitCode = 0);
  P.Free;
  Print(ConsoleOutput);
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

procedure TFMain.QueueForUpdate(Node: TShellTreeNode);
begin
  FQueueLock.Acquire;
  FUpdateQueue.Push(Node);
  FQueueLock.Release;
end;

procedure TFMain.QueryStatus(N: TShellTreeNode; RemoteUpdate: Boolean);
var
  Path: String;
  O: String;
  Behind: Boolean = False;
  Ahead: Boolean = False;
  Dirty: Boolean = False;
  Conflict: Boolean = False;
  Diverged: Boolean = False;
begin
  Path := N.FullFilename;
  if DirectoryExists(Path + DirectorySeparator + '.git') then begin
    if RunTool(Path, GitExe, ['status'], O) then begin
      N.Text := N.ShortFilename;
      if Pos('Unmerged', O) > 0 then  Conflict := True;
      if Pos('behind', O) > 0 then    Behind := True;
      if Pos('ahead', O) > 0 then     Ahead := True;
      if Pos('diverged', O) > 0 then  Diverged := True;
      if Pos('Changes', O) > 0 then   Dirty := True;
      if Behind then    N.Text := '[BEHIND] ' + N.Text;
      if Ahead then     N.Text := '[AHEAD] ' + N.Text;
      if Diverged then  N.Text := '[DIVERGED] ' + N.Text;
      if Dirty then     N.Text := '[DIRTY] ' + N.Text;
      if Conflict then  N.Text := '[CONFLICT] ' + N.Text;
    end;

    if Conflict then
      N.Data := Pointer(ICON_CONFLICT + 1)
    else if Dirty or Behind or Ahead or Diverged then
      N.Data := Pointer(ICON_CHANGED + 1)
    else
      N.Data := Pointer(ICON_NORMAL + 1);

    if RemoteUpdate then
      QueueForUpdate(N);
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

procedure TFMain.MenuItemConsoleClick(Sender: TObject);
var
  Exe: String;
begin
  {$ifdef linux}
  Exe := 'x-terminal-emulator';
  {$endif}
  StartExe(TreeView.Path, Exe, [], False);
end;

procedure TFMain.MenuItemMeldClick(Sender: TObject);
begin
  StartExe(TreeView.Path, 'meld', ['.'], True);
  QueueForUpdate(TShellTreeNode(TreeView.Selected));
end;

procedure TFMain.MenuItemPullClick(Sender: TObject);
var
  OK: Boolean = False;
  O: String = '';
begin
  if RunTool(TreeView.Path, GitExe, ['stash'], O) then
    if RunTool(TreeView.Path, GitExe, ['pull', '--rebase'], O) then
      if RunTool(TreeView.Path, GitExe, ['stash', 'pop'], O) then
        OK := True;
  if not OK then
    MessageDlg('Error', O, mtError, [mbOK], 0);
  QueueForUpdate(TShellTreeNode(TreeView.Selected));
end;

procedure TFMain.MenuItemPushClick(Sender: TObject);
var
  O: String = '';
begin
  if not RunTool(TreeView.Path, GitExe, ['push'], O) then
    MessageDlg('Error', O, mtError, [mbOK], 0);
  QueueForUpdate(TShellTreeNode(TreeView.Selected));
end;

procedure TFMain.NodeMenuPopup(Sender: TObject);
var
  Conflict: Boolean;
  Git: Boolean;
begin
  Git := TreeView.Selected.Data <> nil;
  Conflict := (Pos('[CONFLICT]', TreeView.Selected.Text) > 0);
  MenuItemPull.Enabled := Git and not Conflict;
  MenuItemPush.Enabled := Git and not Conflict;
  MenuItemCommit.Enabled := Git and not Conflict;
  MenuItemMeld.Enabled := Git;
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

