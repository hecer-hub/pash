program prl;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, BaseUnix;

const
  MAGIC_HEADER = 'PLE1';
  MAX_RUN_LENGTH = 255;
  ENTRY_TYPE_FILE = 0;
  ENTRY_TYPE_DIR = 1;

type
  TArchiveEntry = record
    EntryType: Byte;
    NameLength: Word;
    Name: string;
    FileSize: QWord;
    CompressedSize: QWord;
  end;
  
  TArchiveEntryArray = array of TArchiveEntry;

function CompressRLE(const Input: TBytes): TBytes;
var
  i, RunLength: Integer;
  CurrentByte: Byte;
  Output: TBytes;
  OutputSize: Integer;
begin
  if Length(Input) = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  
  SetLength(Output, Length(Input) * 2 + 100);
  OutputSize := 0;
  i := 0;
  
  while i < Length(Input) do
  begin
    CurrentByte := Input[i];
    RunLength := 1;
    
    while (i + RunLength < Length(Input)) and 
          (Input[i + RunLength] = CurrentByte) and 
          (RunLength < 255) do
      Inc(RunLength);
    
    if OutputSize + 2 >= Length(Output) then
      SetLength(Output, Length(Output) * 2);
      
    Output[OutputSize] := Byte(RunLength);
    Output[OutputSize + 1] := CurrentByte;
    Inc(OutputSize, 2);
    
    Inc(i, RunLength);
  end;
  
  SetLength(Output, OutputSize);
  Result := Output;
end;

function DecompressRLE(const Input: TBytes): TBytes;
var
  i, j, RunLength: Integer;
  CurrentByte: Byte;
  Output: TBytes;
  OutputSize: Integer;
begin
  if Length(Input) = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  
  SetLength(Output, Length(Input) * 128);
  OutputSize := 0;
  i := 0;
  
  while i < Length(Input) - 1 do
  begin
    RunLength := Input[i];
    CurrentByte := Input[i + 1];
    
    if RunLength = 0 then
    begin
      Break;
    end;
    
    if OutputSize + RunLength > Length(Output) then
      SetLength(Output, Length(Output) * 2);
    
    for j := 0 to RunLength - 1 do
    begin
      Output[OutputSize] := CurrentByte;
      Inc(OutputSize);
    end;
    
    Inc(i, 2);
  end;
  
  SetLength(Output, OutputSize);
  Result := Output;
end;

function ReadFileToBytes(const FileName: string): TBytes;
var
  FileStream: TFileStream;
begin
  try
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    try
      SetLength(Result, FileStream.Size);
      if FileStream.Size > 0 then
        FileStream.ReadBuffer(Result[0], FileStream.Size);
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error reading file "', FileName, '": ', E.Message);
      SetLength(Result, 0);
    end;
  end;
end;

procedure WriteBytesToFile(const FileName: string; const Data: TBytes);
var
  FileStream: TFileStream;
begin
  try
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      if Length(Data) > 0 then
        FileStream.WriteBuffer(Data[0], Length(Data));
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error writing file "', FileName, '": ', E.Message);
    end;
  end;
end;

procedure GetAllEntries(const BasePath, CurrentPath: string; var Entries: TArchiveEntryArray; var Count: Integer);
var
  SearchRec: TSearchRec;
  FullPath, RelativePath: string;
  BasePathNormalized: string;
begin
  BasePathNormalized := IncludeTrailingPathDelimiter(BasePath);
  
  if FindFirst(CurrentPath + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        FullPath := CurrentPath + SearchRec.Name;
        
        if Length(FullPath) >= Length(BasePathNormalized) then
          RelativePath := Copy(FullPath, Length(BasePathNormalized) + 1, MaxInt)
        else
          RelativePath := SearchRec.Name;
        
        if Count >= Length(Entries) then
          SetLength(Entries, Length(Entries) + 100);
        
        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          Entries[Count].Name := RelativePath;
          Entries[Count].NameLength := Length(RelativePath);
          Entries[Count].EntryType := ENTRY_TYPE_DIR;
          Entries[Count].FileSize := 0;
          Entries[Count].CompressedSize := 0;
          Inc(Count);
          
          WriteLn('  Found directory: ', RelativePath);
          
          GetAllEntries(BasePath, FullPath + DirectorySeparator, Entries, Count);
        end
        else
        begin
          Entries[Count].Name := RelativePath;
          Entries[Count].NameLength := Length(RelativePath);
          Entries[Count].EntryType := ENTRY_TYPE_FILE;
          Entries[Count].FileSize := SearchRec.Size;
          Entries[Count].CompressedSize := 0;
          Inc(Count);
          
          WriteLn('  Found file: ', RelativePath, ' (', SearchRec.Size, ' bytes)');
        end;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure CreateArchive(const SourcePath, ArchiveName: string);
var
  ArchiveStream: TFileStream;
  Entries: TArchiveEntryArray;
  EntryCount, i: Integer;
  FileData, CompressedData: TBytes;
  FullPath: string;
  Header: array[0..3] of Char;
  TotalOriginalSize, TotalCompressedSize: QWord;
begin
  WriteLn('Creating archive: ', ArchiveName);
  WriteLn('Source path: ', SourcePath);
  
  SetLength(Entries, 100);
  EntryCount := 0;
  TotalOriginalSize := 0;
  TotalCompressedSize := 0;
  
  GetAllEntries(SourcePath, IncludeTrailingPathDelimiter(SourcePath), Entries, EntryCount);
  
  if EntryCount = 0 then
  begin
    WriteLn('No files found in source path.');
    Exit;
  end;
  
  WriteLn('Found ', EntryCount, ' entries to archive.');
  
  try
    ArchiveStream := TFileStream.Create(ArchiveName, fmCreate);
    try
      Header := MAGIC_HEADER;
      ArchiveStream.WriteBuffer(Header, 4);
      
      ArchiveStream.WriteBuffer(EntryCount, SizeOf(EntryCount));
      
      for i := 0 to EntryCount - 1 do
      begin
        ArchiveStream.WriteBuffer(Entries[i].EntryType, SizeOf(Entries[i].EntryType));
        ArchiveStream.WriteBuffer(Entries[i].NameLength, SizeOf(Entries[i].NameLength));
        ArchiveStream.WriteBuffer(Entries[i].Name[1], Entries[i].NameLength);
        ArchiveStream.WriteBuffer(Entries[i].FileSize, SizeOf(Entries[i].FileSize));
        
        if Entries[i].EntryType = ENTRY_TYPE_FILE then
        begin
          FullPath := IncludeTrailingPathDelimiter(SourcePath) + Entries[i].Name;
          WriteLn('  Processing file: ', FullPath);
          
          if FileExists(FullPath) then
          begin
            FileData := ReadFileToBytes(FullPath);
            CompressedData := CompressRLE(FileData);
            
            Entries[i].CompressedSize := Length(CompressedData);
            TotalOriginalSize := TotalOriginalSize + Entries[i].FileSize;
            TotalCompressedSize := TotalCompressedSize + Entries[i].CompressedSize;
            
            WriteLn('    ', Entries[i].Name, ' (', Entries[i].FileSize, ' -> ', 
                    Entries[i].CompressedSize, ' bytes)');
            
            ArchiveStream.WriteBuffer(Entries[i].CompressedSize, SizeOf(Entries[i].CompressedSize));
            if Length(CompressedData) > 0 then
              ArchiveStream.WriteBuffer(CompressedData[0], Length(CompressedData));
          end
          else
          begin
            WriteLn('    Warning: File not found: ', FullPath);
            Entries[i].CompressedSize := 0;
            ArchiveStream.WriteBuffer(Entries[i].CompressedSize, SizeOf(Entries[i].CompressedSize));
          end;
        end
        else
        begin
          ArchiveStream.WriteBuffer(Entries[i].CompressedSize, SizeOf(Entries[i].CompressedSize));
          WriteLn('  Directory: ', Entries[i].Name);
        end;
      end;
      
    finally
      ArchiveStream.Free;
    end;
    
    WriteLn('Archive created successfully!');
    WriteLn('Total original size: ', TotalOriginalSize, ' bytes');
    WriteLn('Total compressed size: ', TotalCompressedSize, ' bytes');
    if TotalOriginalSize > 0 then
      WriteLn('Compression ratio: ', Format('%.2f%%', 
              [(TotalOriginalSize - TotalCompressedSize) / TotalOriginalSize * 100]));
    
  except
    on E: Exception do
    begin
      WriteLn('Error creating archive: ', E.Message);
    end;
  end;
end;

procedure ExtractArchive(const ArchiveName, DestPath: string);
var
  ArchiveStream: TFileStream;
  Entry: TArchiveEntry;
  EntryCount, i: Integer;
  Header: array[0..3] of Char;
  CompressedData, DecompressedData: TBytes;
  FullPath, DirPath: string;
begin
  WriteLn('Extracting archive: ', ArchiveName);
  WriteLn('Destination path: ', DestPath);
  
  try
    ArchiveStream := TFileStream.Create(ArchiveName, fmOpenRead);
    try
      WriteLn('Archive file size: ', ArchiveStream.Size, ' bytes');
      
      if ArchiveStream.Size < 4 then
      begin
        WriteLn('Error: Archive file too small.');
        Exit;
      end;
      
      ArchiveStream.ReadBuffer(Header, 4);
      if Header <> MAGIC_HEADER then
      begin
        WriteLn('Invalid archive format. Expected "', MAGIC_HEADER, '", got "', Header, '"');
        Exit;
      end;
      
      if ArchiveStream.Size < 8 then
      begin
        WriteLn('Error: Archive file corrupted (missing entry count).');
        Exit;
      end;
      
      ArchiveStream.ReadBuffer(EntryCount, SizeOf(EntryCount));
      WriteLn('Archive contains ', EntryCount, ' entries.');
      
      if EntryCount <= 0 then
      begin
        WriteLn('No entries in archive.');
        Exit;
      end;
      
      if not DirectoryExists(DestPath) then
        ForceDirectories(DestPath);
      
      for i := 0 to EntryCount - 1 do
      begin
        ArchiveStream.ReadBuffer(Entry.EntryType, SizeOf(Entry.EntryType));
        ArchiveStream.ReadBuffer(Entry.NameLength, SizeOf(Entry.NameLength));
        
        SetLength(Entry.Name, Entry.NameLength);
        ArchiveStream.ReadBuffer(Entry.Name[1], Entry.NameLength);
        
        ArchiveStream.ReadBuffer(Entry.FileSize, SizeOf(Entry.FileSize));
        ArchiveStream.ReadBuffer(Entry.CompressedSize, SizeOf(Entry.CompressedSize));
        
        FullPath := IncludeTrailingPathDelimiter(DestPath) + Entry.Name;
        
        if Entry.EntryType = ENTRY_TYPE_DIR then
        begin
          WriteLn('  Creating directory: ', Entry.Name);
          ForceDirectories(FullPath);
        end
        else
        begin
          WriteLn('  Extracting file: ', Entry.Name, ' (', Entry.CompressedSize, 
                  ' -> ', Entry.FileSize, ' bytes)');
          
          DirPath := ExtractFileDir(FullPath);
          if (DirPath <> '') and not DirectoryExists(DirPath) then
            ForceDirectories(DirPath);
          
          SetLength(CompressedData, Entry.CompressedSize);
          if Entry.CompressedSize > 0 then
          begin
            if ArchiveStream.Read(CompressedData[0], Entry.CompressedSize) < Entry.CompressedSize then
            begin
              WriteLn('Error: Unexpected end of file while reading compressed data.');
              Exit;
            end;
            
            DecompressedData := DecompressRLE(CompressedData);
            WriteBytesToFile(FullPath, DecompressedData);
          end;
        end;
      end;
      
    finally
      ArchiveStream.Free;
    end;
    
    WriteLn('Archive extracted successfully!');
    
  except
    on E: Exception do
    begin
      WriteLn('Error extracting archive: ', E.Message);
    end;
  end;
end;

procedure ShowUsage;
begin
  WriteLn('RLE Archiver v0.5');
  WriteLn('Usage:');
  WriteLn('  Create archive: ', ExtractFileName(ParamStr(0)), ' -c <source_path> <archive_name>');
  WriteLn('  Extract archive: ', ExtractFileName(ParamStr(0)), ' -x <archive_name> <dest_path>');
  WriteLn('');
  WriteLn('Examples:');
  WriteLn('  ', ExtractFileName(ParamStr(0)), ' -c /home/user/documents myarchive.rle');
  WriteLn('  ', ExtractFileName(ParamStr(0)), ' -x myarchive.rle /home/user/extracted');
end;


begin
  WriteLn('RLE Archiver for Free Pascal');
  WriteLn('============================');
  WriteLn;
  
  if ParamCount < 3 then
  begin
    ShowUsage;
    Exit;
  end;
  
  if ParamStr(1) = '-c' then
  begin

    if ParamCount <> 3 then
    begin
      WriteLn('Error: Invalid number of parameters for create operation.');
      ShowUsage;
      Exit;
    end;
    
    if not DirectoryExists(ParamStr(2)) then
    begin
      WriteLn('Error: Source path does not exist: ', ParamStr(2));
      Exit;
    end;
    
    CreateArchive(ParamStr(2), ParamStr(3));
  end
  else if ParamStr(1) = '-x' then
  begin

    if ParamCount <> 3 then
    begin
      WriteLn('Error: Invalid number of parameters for extract operation.');
      ShowUsage;
      Exit;
    end;
    
    if not FileExists(ParamStr(2)) then
    begin
      WriteLn('Error: Archive file does not exist: ', ParamStr(2));
      Exit;
    end;
    
    ExtractArchive(ParamStr(2), ParamStr(3));
  end
  else
  begin
    WriteLn('Error: Invalid operation. Use -c to create or -x to extract.');
    ShowUsage;
  end;
end.
