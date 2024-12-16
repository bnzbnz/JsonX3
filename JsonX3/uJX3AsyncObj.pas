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
unit uJX3AsyncObj;

interface
uses
    System.Generics.Collections
  ,  SyncObjs
  ;

type

  TJX3AsycObjectLoader = class(TObject)
  private
    FSize: integer;
    FThreshold: integer;
    FCache: TStack<TObject>;
    FLock: TCriticalSection;
    FIsFilling: Boolean;
  public
    constructor Create(ASize: integer; AThreshold: integer); overload;
    destructor Destroy; override;
    procedure Fill<T:class, constructor>;
    function Get<T:class, constructor>: TObject;
  end;

implementation
uses
     System.Threading
   ;

constructor TJX3AsycObjectLoader.Create(ASize: integer; AThreshold: integer);
begin
  FSize := ASize;
  FThreshold := AThreshold;
  FCache := TStack<TObject>.Create;
  FLock := TCriticalSection.Create;
  FIsFilling := False;
end;

destructor TJX3AsycObjectLoader.Destroy;
begin
  FLock.Enter;
  while FCache.Count > 0 do FCache.pop.Free;
  FCache.Free;
  FLock.Leave;
  FLock.Free;
end;

procedure TJX3AsycObjectLoader.Fill<T>;
var
  Buf: TArray<TObject>;
begin
  if FIsFilling then Exit;
  FIsFilling := True;
  TTask.Future<Boolean>(
    function:Boolean
    begin
      try
        SetLength(Buf, FSize - FCache.Count);
        for var i := 0 to FSize - FCache.Count -1 do Buf[i] := T.Create;
        FLock.Enter;
        for var i := 0 to FSize - FCache.Count -1 do FCache.Push(Buf[i]);
        FLock.Leave;
      finally
        FIsFilling := False;
        Result := False;
      end;
    end
  );
end;

function TJX3AsycObjectLoader.Get<T>: TObject;
begin
  While FCache.Count <= FThreshold do Fill<T>;
  While FCache.Count = 0 do ;
  FLock.Enter;
  Result := FCache.Pop;
  FLock.Leave;
end;

end.
