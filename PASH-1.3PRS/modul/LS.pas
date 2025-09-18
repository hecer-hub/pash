unit LS;

interface

uses
  trpo,
  glvar,
  sysutils;

procedure RunLS(const pathh: String);

implementation

procedure RunLS(const pathh: String);
var
  filePath: TSearchRec;
  searchPath: String;
begin
  if pathh = '' then
    searchPath := glvar.currentPath + '/*'
  else
    searchPath := pathh + '/*';

  if FindFirst(searchPath, faAnyFile, filePath) = 0 then
  begin
    repeat
      if (filePath.Name <> '.') and (filePath.Name <> '..') then
        trpoWriteLn(filePath.Name);
    until FindNext(filePath) <> 0;
    FindClose(filePath);
  end
  else
    trpoWriteLn('Folder empty or not found.');
end;

end.
