(*****************************************************************************
The MIT License (MIT)

Copyright (c) 2020-2025 Laurent Meyer JsonX3@ea4d.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*****************************************************************************)
unit uJX3MiniPool;

{$DEFINE MINIPOOL}

interface
uses
    System.Generics.Collections
  , SyncObjs
  ;

type

  TJX3MiniPool = class(TObject)
  private
    FCache: TStack<TObject>;
    FLock: TCriticalSection;
    FSize: Integer;
    FGrow: Boolean;
    constructor Create(AGrow: Boolean); overload;
    procedure PreLoad<T:class, constructor>(ASize: Integer);
  public
    destructor Destroy; override;
    function Get<T:class, constructor>: T;
    procedure Put(AObj:TObject);
    class function GetInstance<T:class, constructor>(ASize: Integer; AGrow: Boolean = True): TJX3MiniPool;
  end;

implementation
uses
      SysUtils
    ;

constructor TJX3MiniPool.Create(AGrow: Boolean);
begin
  {$IFDEF MINIPOOL}
  FCache := TStack<TObject>.Create;
  FLock := TCriticalSection.Create;
  FGrow := AGrow;
  {$ENDIF}
end;

destructor TJX3MiniPool.Destroy;
begin
  {$IFDEF MINIPOOL}
  FLock.Enter;
  while FCache.Count > 0 do FCache.pop.Free;
  FCache.Free;
  FLock.Leave;
  FLock.Free;
  {$ENDIF}
end;

procedure TJX3MiniPool.PreLoad<T>(ASize: Integer);
begin
  {$IFDEF MINIPOOL}
  FSize := ASize;
  while FCache.Count < ASize do FCache.Push(T.Create);
  {$ENDIF}
end;

function TJX3MiniPool.Get<T>: T;
begin
  {$IFDEF MINIPOOL}
  FLock.Enter;
  try
    if FCache.Count = 0  then
      Result := T.Create
    else
      Result := T(FCache.Pop);
  finally
    FLock.Leave;
  end;
  {$ELSE}
    Result := T.Create;
  {$ENDIF}
end;

class function TJX3MiniPool.GetInstance<T>(ASize: Integer; AGrow: Boolean): TJX3MiniPool;
begin
  Result := TJX3MiniPool.Create(AGrow);
  {$IFDEF MINIPOOL}
  Result.PreLoad<T>(ASize);
  {$ENDIF}
end;

procedure TJX3MiniPool.Put(AObj:TObject);
begin
  {$IFDEF MINIPOOL}
  FLock.Enter;
  try
    if not FGrow and (FCache.Count >= FSize) then
      AObj.Free
    else
      FCache.Push(AObj);
  finally
    FLock.Leave;
  end;
  {$ELSE}
  AObj.Free;
  {$ENDIF}
end;

end.
