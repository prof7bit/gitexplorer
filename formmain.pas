unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  ComCtrls, ExtCtrls, Menus, process, LazUTF8, FormProgRun,
  LockedQueue;

const
  ICON_NORMAL   = 0;
  ICON_CHANGED  = 1;
  ICON_CONFLICT = 2;

  MILLISEC      = 1 / (24 * 60 * 60 * 1000);

type
  TUpdaterQueue = specialize TLockedQueue<TShellTreeNode>;

  { TUpdateThread }

  TUpdateThread = class(TThread)
    procedure Execute; override;
  end;

  { TFMain }

  TFMain = class(TForm)
    ImageList: TImageList;
    MenuItemGitGui: TMenuItem;
    MenuItemGitk: TMenuItem;
    MenuItemConsole: TMenuItem;
    MenuItemMeld: TMenuItem;
    MenuItemPull: TMenuItem;
    NodeMenu: TPopupMenu;
    TreeView: TShellTreeView;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemConsoleClick(Sender: TObject);
    procedure MenuItemGitGuiClick(Sender: TObject);
    procedure MenuItemGitkClick(Sender: TObject);
    procedure MenuItemMeldClick(Sender: TObject);
    procedure MenuItemPullClick(Sender: TObject);
    procedure MenuItemPushClick(Sender: TObject);
    procedure NodeMenuPopup(Sender: TObject);
    procedure TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure QueueForUpdate(Node: TShellTreeNode);
    procedure QueueForImmediateUpdate(Node: TShellTreeNode);
  private
    FUpdaterInbox: TUpdaterQueue;
    FUpdateThread: TUpdateThread;
    procedure QueryStatus(N: TShellTreeNode; RemoteUpdate: Boolean);
    procedure AsyncQueryStatus(P: PtrInt);
    procedure UpdateAllNodes(Root: TTreeNode);
    function NodeHasTag(TagName: String): Boolean;
    function NodeIsDirty: Boolean;
    function NodeIsConflict: Boolean;
    function NodeIsGit: Boolean;
    function SelNode: TShellTreeNode;
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

function GetExe(Name: String): String;

  {$ifdef windows}
  function TryProgPaths(Path: String): String;
  begin
    Result := GetEnvironmentVariableUTF8('ProgramW6432') + Path;
    if FileExists(Result) then exit;
    Result := GetEnvironmentVariableUTF8('programfiles') + Path;
    if FileExists(Result) then exit;
    Result := GetEnvironmentVariableUTF8('programfilesx86') + Path;
    if FileExists(Result) then exit;
  end;
  {$endif}

  begin
  {$ifdef windows}
  if Name = 'git' then
    Result := TryProgPaths('\Git\bin\git.exe');
  if Name = 'gitk' then
    Result := TryProgPaths('\Git\bin\gitk');
  if Name = 'meld' then
    Result := TryProgPaths('\Meld\Meld.exe');
  {$else}
  Result :=  Name;
  {$endif}
end;

procedure AddToolsToPath;
{$ifdef windows}
var
  GitPath, MeldPath, PathVariable: String;
{$endif}
begin
  {$ifdef windows}
  GitPath := ExtractFileDir(GetExe('git'));
  MeldPath := ExtractFileDir(GetExe('meld'));
  PathVariable := GetEnvironmentVariableUTF8('PATH');
  PathVariable := GitPath + ';' + MeldPath + ';' + PathVariable;
  SetEnvironmentVariable('PATH', PChar(PathVariable));
  {$endif}
end;

procedure StartExe(Path: String; Cmd: String; Args: array of string; Wait: Boolean; Console: Boolean);
var
  P: TProcess;
begin
  P := TProcess2.Create(nil, Path, Cmd, Args);
  if Wait then
    P.Options := P.Options + [poWaitOnExit];
  if not Console then
    P.Options := P.Options + [poNoConsole];
  P.Execute;
  P.Free;
end;

function RunTool(Path: String; cmd: String; Args: array of string; out ConsoleOutput: String): Boolean;
var
  P: TProcess;
begin
  P := TProcess2.Create(nil, Path, Cmd, Args);
  P.Options := P.Options + [poUsePipes, poNoConsole];
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
end;

function RunGit(Node: TShellTreeNode; Args: array of String; out CmdOut: String): Boolean;
var
  Path: String;
begin
  Path := Node.FullFilename;
  Result := RunTool(Path, 'git', Args, CmdOut);
end;

{$R *.lfm}

{ TUpdateThread }

procedure TUpdateThread.Execute;
var
  Node: TShellTreeNode;
  O: String;
begin
  repeat
    if FMain.FUpdaterInbox.Get(Node, 100) then begin
      Print('update queue processing, halting update timer');
      FMain.UpdateTimer.Enabled := True;

      repeat
        Print('updating: ' + Node.ShortFilename);
        if RunTool(Node.FullFilename, 'git', ['remote', 'update'], O) then begin
          Application.QueueAsyncCall(@FMain.AsyncQueryStatus, PtrInt(Node));
        end;
      until not FMain.FUpdaterInbox.Get(Node, 100);

      Print('queue empty, resuming update timer');
      FMain.UpdateTimer.Enabled := True;
    end;
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
  FUpdaterInbox.Put(Node);
end;

procedure TFMain.QueueForImmediateUpdate(Node: TShellTreeNode);
begin
  FUpdaterInbox.PutFront(Node);
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

  procedure ApplyTag(Name: String);
  begin
    N.Text := '[' + Name + '] ' + N.Text;
  end;

  function HasWord(w: String): Boolean;
  begin
    Result := Pos(W, O) > 0;
  end;

  procedure ApplyIcon(IconIdex: Integer);
  begin
    N.Data := {%H-}Pointer(IconIdex + 1);
  end;

begin
  Path := N.FullFilename;
  if DirectoryExists(Path + DirectorySeparator + '.git') then begin
    TreeView.BeginUpdate;
    if RunGit(N, ['status'], O) then begin
      N.Text := N.ShortFilename;
      Conflict := HasWord('Unmerged');
      Behind   := HasWord('behind');
      Ahead    := HasWord('ahead');
      Diverged := HasWord('diverged');
      Dirty    := HasWord('Changes');
      if Behind then    ApplyTag('BEHIND');
      if Ahead then     ApplyTag('AHEAD');
      if Diverged then  ApplyTag('DIVERGED');
      if Dirty then     ApplyTag('DIRTY');
      if Conflict then  ApplyTag('CONFLICT');
    end;

    if Conflict then
      ApplyIcon(ICON_CONFLICT)
    else if Dirty or Behind or Ahead or Diverged then
      ApplyIcon(ICON_CHANGED)
    else
      ApplyIcon(ICON_NORMAL);

    if RemoteUpdate then
      QueueForUpdate(N);

    TreeView.EndUpdate;
    TreeView.Refresh;
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

function TFMain.NodeHasTag(TagName: String): Boolean;
begin
  Result := Pos('[' + TagName + ']', TreeView.Selected.Text) > 0;
end;

function TFMain.NodeIsDirty: Boolean;
begin
  Result := NodeHasTag('DIRTY');
end;

function TFMain.NodeIsConflict: Boolean;
begin
  Result := NodeHasTag('CONFLICT');
end;

function TFMain.NodeIsGit: Boolean;
begin
  Result := TreeView.Selected.Data <> nil;
end;

function TFMain.SelNode: TShellTreeNode;
begin
  Result := TShellTreeNode(TreeView.Selected);
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  UpdateAllNodes(TreeView.TopItem);
end;

procedure TFMain.MenuItemConsoleClick(Sender: TObject);
begin
  {$ifdef linux}
  StartExe(TreeView.Path, 'x-terminal-emulator', [], False, True);
  {$endif}
  {$ifdef windows}
  StartExe(TreeView.Path, 'sh.exe', ['--login', '-i'], True, True);
  {$endif}
end;

procedure TFMain.MenuItemGitGuiClick(Sender: TObject);
begin
  StartExe(TreeView.Path, 'git', ['gui'], True, False);
  QueueForImmediateUpdate(TShellTreeNode(TreeView.Selected));
end;

procedure TFMain.MenuItemGitkClick(Sender: TObject);
begin
  {$ifdef windows}
  StartExe(TreeView.Path, 'sh', [GetExe('gitk')], True, False);
  {$else}
  StartExe(TreeView.Path, 'gitk', [], True, False);
  {$endif}
  QueueForImmediateUpdate(TShellTreeNode(TreeView.Selected));
end;

procedure TFMain.MenuItemMeldClick(Sender: TObject);
begin
  StartExe(TreeView.Path, 'meld', [TreeView.Path], True, False);
  QueueForImmediateUpdate(TShellTreeNode(TreeView.Selected));
end;

procedure TFMain.MenuItemPullClick(Sender: TObject);
var
  OK: Boolean = False;
  O: String = '';
  N: TShellTreeNode;
begin
  N := TShellTreeNode(TreeView.Selected);
  (*
  if NodeIsDirty then begin
    if RunGit(SelNode, ['stash'], O) then
      if RunGit(SelNode, ['pull', '--rebase'], O) then
        if RunGit(SelNode, ['stash', 'pop'], O) then
          OK := True;
  end
  else begin
    if RunGit(SelNode, ['pull', '--rebase'], O) then
      OK := True;
  end;
  if not OK then
    MessageDlg('Error', O, mtError, [mbOK], 0);
  *)
  FProgRun.Run(N, 'git', ['pull', '--rebase']);
end;

procedure TFMain.MenuItemPushClick(Sender: TObject);
var
  O: String = '';
begin
  if not RunGit(SelNode, ['push'], O) then
    MessageDlg('Error', O, mtError, [mbOK], 0);
  QueueForImmediateUpdate(TShellTreeNode(TreeView.Selected));
end;

procedure TFMain.NodeMenuPopup(Sender: TObject);
var
  Conflict: Boolean;
  Git: Boolean;
begin
  Conflict := NodeIsConflict;
  Git := NodeIsGit;
  MenuItemPull.Enabled := Git and not Conflict;
  MenuItemGitk.Enabled := Git;
  MenuItemGitGui.Enabled := Git;
  MenuItemMeld.Enabled := Git;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  AddToolsToPath;
  FUpdaterInbox := TUpdaterQueue.Create;
  TreeView.Root := GetUserDir;
  FUpdateThread := TUpdateThread.Create(False);
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  AppTerminating := True;
  FUpdateThread.WaitFor;
  FUpdaterInbox.Free;
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

