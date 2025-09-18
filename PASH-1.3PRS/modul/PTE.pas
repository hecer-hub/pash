unit PTE;

interface

uses
  crt,
  sysutils,
  trpo,
  glvar;

procedure RunPTE(const fileName: String);

implementation

var
  cursorX, cursorY: Integer;
  lines: array of string;
  currentFileName: String;
  
procedure redrawScreen;
var
  i: Integer;
begin
  clrscr;
  for i := 0 to High(lines) do
  begin
    gotoxy(1, i + 1);
    Write(lines[i]);
  end;

  gotoxy(1, ScreenHeight);
  Write('File: ', ExtractFileName(currentFileName), ' | Line: ', cursorY, ' Col: ', cursorX, ' | Ctrl+S Save | Ctrl+Q Exit');

  gotoxy(cursorX, cursorY);
end;

procedure loadFile;
var
  myFile: TextFile;
  line: String;
begin
  if FileExists(currentFileName) then
  begin
    SetLength(lines, 0);
    Assign(myFile, currentFileName);
    {$I-}
    Reset(myFile);
    {$I+}
    if IOResult = 0 then
    begin
      while not EOF(myFile) do
      begin
        Readln(myFile, line);
        SetLength(lines, High(lines) + 2);
        lines[High(lines)] := line;
      end;
      Close(myFile);
    end
    else
    begin
      SetLength(lines, 1);
      lines[0] := '';
    end;
  end
  else
  begin
    SetLength(lines, 1);
    lines[0] := '';
  end;
end;

procedure saveFile;
var
  myFile: TextFile;
  i: Integer;
begin
  Assign(myFile, currentFileName);
  Rewrite(myFile);
  for i := 0 to High(lines) do
  begin
    WriteLn(myFile, lines[i]);
  end;
  Close(myFile);
  trpoWriteLn('File saved.');
end;

procedure RunPTE(const fileName: String);
var
  key: Char;
begin
  currentFileName := glvar.currentPath + '/' + fileName;
  loadFile;
  
  cursorX := 1;
  cursorY := 1;

  repeat
    redrawScreen;
    key := readkey;

    case key of
      #0:
        begin
          key := readkey;
          case key of
            #75:
              if cursorX > 1 then Dec(cursorX);
            #77:
              if cursorX <= Length(lines[cursorY - 1]) + 1 then Inc(cursorX);
            #72:
              if cursorY > 1 then
              begin
                Dec(cursorY);
                if cursorX > Length(lines[cursorY - 1]) + 1 then
                  cursorX := Length(lines[cursorY - 1]) + 1;
              end;
            #80:
              if cursorY <= High(lines) then
              begin
                Inc(cursorY);
                if cursorX > Length(lines[cursorY - 1]) + 1 then
                  cursorX := Length(lines[cursorY - 1]) + 1;
              end;
          end;
        end;
      #8:
        if cursorX > 1 then
        begin
          Delete(lines[cursorY - 1], cursorX - 1, 1);
          Dec(cursorX);
        end
        else if (cursorY > 1) and (cursorX = 1) then
        begin
          cursorX := Length(lines[cursorY - 2]) + 1;
          lines[cursorY - 2] := lines[cursorY - 2] + lines[cursorY - 1];
          if cursorY <= High(lines) then
            move(lines[cursorY], lines[cursorY-1], (High(lines) - cursorY + 1) * sizeof(string));
          SetLength(lines, High(lines));
          Dec(cursorY);
        end;
      #13:
        begin
          SetLength(lines, High(lines) + 2);
          move(lines[cursorY - 1], lines[cursorY], (High(lines) - cursorY) * sizeof(string));
          lines[cursorY] := Copy(lines[cursorY - 1], cursorX, Length(lines[cursorY - 1]));
          SetLength(lines[cursorY - 1], cursorX - 1);
          Inc(cursorY);
          cursorX := 1;
        end;
      #19:
        saveFile;
      #17:
        key := #27;
      #27:
      else
        begin
          Insert(key, lines[cursorY - 1], cursorX);
          Inc(cursorX);
        end;
    end;
  until key = #27;
  clrscr;
end;
end.
