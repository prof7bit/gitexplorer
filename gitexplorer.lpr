program gitexplorer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormMain, FormProgRun, LockedQueue
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TFProgRun, FProgRun);
  Application.Run;
end.

