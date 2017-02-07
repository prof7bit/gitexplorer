{ Thread safe generic queue implementation with optional async notification
  of main thread, WaitWor, non-blocking or blocking Get() with Timeout, etc.

  Copyright (C) 2017 Bernd Kreuss <prof7bit@gmail.com>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
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
      procedure PutArray(FirstElement: PT; Count: SizeUint);
      procedure PutFront(Element: T);
      procedure Lock;
      procedure Unlock;
      function Size: SizeUint;
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
  if Size() = 1 then begin
    FEvent.SetEvent;
    CallOnDataSync;
  end;
  Unlock;
end;

procedure TLockedQueue.PutArray(FirstElement: PT; Count: SizeUint);
var
  I: Integer;
begin
  Lock;
  for I := 0 to Count - 1 do begin
    FData.PushBack(FirstElement[I]);
  end;
  if Size() = Count then begin
    FEvent.SetEvent;
    CallOnDataSync;
  end;
  Unlock;
end;

procedure TLockedQueue.PutFront(Element: T);
begin
  Lock;
  FData.PushFront(Element);
  if Size() = 1 then begin
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

function TLockedQueue.Size: SizeUint;
begin
  Result := FData.Size;
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

