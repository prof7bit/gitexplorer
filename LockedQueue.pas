unit LockedQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gdeque, syncobjs;

type
  TQueueEventProc = procedure of object;

  { TLockedQueue }

  generic TLockedQueue<T> = class
    type
      TData = specialize TDeque<T>;
      PT = ^T;
    public
      OnDataAvailable: TQueueEventProc;
      constructor Create;
      destructor Destroy; override;
      function Get(out Element: T; Timeout: Cardinal): Boolean;
      function Get(out Element: T): Boolean;
      procedure Put(Element: T);
      procedure PutArray(FirstElement: PT; Count: Integer);
      procedure PutFront(Element: T);
      procedure Lock;
      procedure Unlock;
      function IsEmpty: Boolean;
      function WaitFor(Timeout: Cardinal): TWaitResult;
    private
      FData: TData;
      FLock: TCriticalSection;
      FEvent: TEvent;
      procedure CallOnDataSync;
      procedure CallOnDataAsync;
  end;


implementation

{ TTSQueue }

constructor TLockedQueue.Create;
begin
  FData := TData.Create();
  FLock := TCriticalSection.Create;
  FEvent := TSimpleEvent.Create;
end;

destructor TLockedQueue.Destroy;
begin
  FEvent.Free;
  FLock.Free;
  FData.Free;
  inherited Destroy;
end;

function TLockedQueue.Get(out Element: T; Timeout: Cardinal): Boolean;
begin
  Result := False;
  if WaitFor(Timeout) = wrSignaled then begin
    Lock;
    Result := Get(Element);
    Unlock;
  end;
end;

procedure TLockedQueue.Lock;
begin
  FLock.Acquire;
end;

procedure TLockedQueue.Unlock;
begin
  FLock.Release;
end;

procedure TLockedQueue.Put(Element: T);
begin
  Lock;
  FData.PushBack(Element);
  if FData.Size() = 1 then begin
    FEvent.SetEvent;
    CallOnDataSync;
  end;
  Unlock;
end;

procedure TLockedQueue.PutArray(FirstElement: PT; Count: Integer);
var
  I: Integer;
begin
  Lock;
  for I := 0 to Count - 1 do begin
    FData.PushBack(FirstElement[I]);
  end;
  if FData.Size() = Count then begin
    FEvent.SetEvent;
    CallOnDataSync;
  end;
  Unlock;
end;

procedure TLockedQueue.PutFront(Element: T);
begin
  Lock;
  FData.PushFront(Element);
  if FData.Size() = 1 then begin
    FEvent.SetEvent;
    CallOnDataSync;
  end;
  Unlock;
end;

function TLockedQueue.Get(out Element: T): Boolean;
begin
  Result := False;
  Lock;
  if not IsEmpty then begin
    Element := FData.Front();
    FData.PopFront();
    Result := True;
    if IsEmpty then
      FEvent.ResetEvent;
  end;
  Unlock;
end;

function TLockedQueue.IsEmpty: Boolean;
begin
  Result := FData.IsEmpty();
end;

function TLockedQueue.WaitFor(Timeout: Cardinal): TWaitResult;
begin
  Result := FEvent.WaitFor(Timeout);
end;

procedure TLockedQueue.CallOnDataSync;
begin
  if Assigned(OnDataAvailable) then
    TThread.Queue(nil, @CallOnDataAsync);
end;

procedure TLockedQueue.CallOnDataAsync;
begin
  if Assigned(OnDataAvailable) then
    OnDataAvailable;
end;

end.

