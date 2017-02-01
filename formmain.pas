unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ShellTreeView1: TShellTreeView;
    procedure ShellTreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.ShellTreeView1MouseDown(Sender: TObject; Button: TMouseButton;
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

end.

