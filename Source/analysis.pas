unit analysis;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
    ExtCtrls, ComCtrls,
    FileUtil, functions, filter, LConvEncoding, LazUTF8;

type

    { TFormAnalysis }

    TFormAnalysis = class(TForm)
        ButtonFinish1: TButton;
        ButtonFinish2: TButton;
        EditService: TEdit;
        EditUser: TEdit;
        GroupBoxOracle: TGroupBox;
        LabelService: TLabel;
        LabelUser: TLabel;
        MemoSQL: TMemo;
        Panel1: TPanel;
        SG: TStringGrid;
        Splitter1: TSplitter;
        StatusBar: TStatusBar;
        procedure ButtonFinish1Click(Sender: TObject);
        procedure ButtonFinish2Click(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure SGDblClick(Sender: TObject);
    private

    public
       StartDir, csv: String;
       RegEx: Array of String;
       UseRegEx: Boolean;
       FieldList, FieldRecom: TStringList;
       ColCount, RowCount: Integer;
       data: Array of Array of String;
    end;

var
    FormAnalysis: TFormAnalysis;

implementation

{$R *.lfm}

{ TFormAnalysis }

procedure TFormAnalysis.FormShow(Sender: TObject);
var
    col, row: Integer;
begin
    // Сделать в ячейках пустое значение вместо нуля
    for col := 1 to SG.ColCount - 1 do
        for row := 1 to SG.RowCount - 1 do
            if SG.Cells[col, row] = '0' then SG.Cells[col, row] := '';
end;

procedure TFormAnalysis.SGDblClick(Sender: TObject);
var
    FieldName, FindType, FieldType, CheckResult: String;
    FieldId, col, row: Integer;
begin
    if SG.Cells[SG.Col, SG.Row] = '' then Exit;

    FieldName := SG.Cells[0, SG.Row];
    FindType := SG.Columns.Items[SG.Col - 1].Title.Caption;

    for col := 0 to ColCount - 1 do
        if FieldName.ToUpper = FieldList.Strings[col].ToUpper
            then FieldId := col;

    FormFilter.SG.Clear;
    FormFilter.SG.ColCount := ColCount;
    FormFilter.SG.RowCount := 1;

    for col := 0 to ColCount - 1 do
        FormFilter.SG.Cells[col, 0] := FieldList.Strings[col].ToUpper;

    case FindType of
        'INT':   FieldType := 'I';
        'FLOAT': FieldType := 'F';
        'DATE':  FieldType := 'D';
        'TEXT':  FieldType := 'S';
        'NULL':  FieldType := 'N';
        'ERR':   FieldType := 'E';
    end;

    for row := 0 to RowCount - 1 do begin
      if UseRegEx
          then CheckResult := CheckCellTypeRE(data[FieldId, row], RegEx)
          else CheckResult := CheckCellType(data[FieldId, row]);
      if CheckResult = FieldType then begin
            FormFilter.SG.RowCount := FormFilter.SG.RowCount + 1;
            for col := 0 to ColCount - 1 do
                FormFilter.SG.Cells[col, FormFilter.SG.RowCount - 1] := data[col, row];
        end;
    end;

    FormFilter.Show;
end;

procedure TFormAnalysis.ButtonFinish1Click(Sender: TObject);
var
    FileName, TableName: String;
    s: TStrings;
begin
    s := TStringList.Create;

    // Create file Run.bat
    FileName := 'Run1.bat';
    if FileExists(StartDir + 'Template\' + FileName) then begin
        s.LoadFromFile(StartDir + 'Template\' + FileName);
        s.Text := StringReplace(s.Text, '#username#', EditUser.Text, [rfReplaceAll]);
        s.Text := StringReplace(s.Text, '#service#', EditService.Text, [rfReplaceAll]);
        s.SaveToFile(StartDir + 'Run.bat');
    end;

    // Create file create.sql
    s.Clear;
    s.Text := MemoSQL.Lines.Text;
    s.Add('quit;');
    s.SaveToFile(StartDir + 'create.sql');

    // Create file loader.ctl
    FileName := 'loader.ctl';
    if FileExists(StartDir + 'Template\' + FileName) then begin
        s.LoadFromFile(StartDir + 'Template\' + FileName);
        s.Text := StringReplace(s.Text, '#csv#', csv, [rfReplaceAll]);
        TableName := GetTableName(MemoSQL.Lines);
        s.Text := StringReplace(s.Text, '#table#', TableName, [rfReplaceAll]);
        s.Text := StringReplace(s.Text, '#fields#', FieldList.DelimitedText, [rfReplaceAll]);
        s.SaveToFile(StartDir + FileName);
    end;

    s.Free;
    ShowMessage('Scripts for loading in Oracle are created!');
end;

procedure TFormAnalysis.ButtonFinish2Click(Sender: TObject);
var
    txt, FileName, TableName: String;
    s: TStrings;
    col, row: Integer;
begin
    s := TStringList.Create;

    // Create file Run.bat
    FileName := 'Run2.bat';
    if FileExists(StartDir + 'Template\' + FileName) then begin
        s.LoadFromFile(StartDir + 'Template\' + FileName);
        s.Text := StringReplace(s.Text, '#username#', EditUser.Text, [rfReplaceAll]);
        s.Text := StringReplace(s.Text, '#service#', EditService.Text, [rfReplaceAll]);
        s.SaveToFile(StartDir + 'Run.bat');
    end;

    // Create file load.sql
    FileName := 'load.sql';
    if FileExists(StartDir + 'Template\' + FileName) then
        CopyFile(StartDir + 'Template\' + FileName, StartDir + FileName);

    // Create file create.sql
    MemoSQL.Lines.SaveToFile(StartDir + 'create.sql');

    // Create file insert.sql
    FileName := 'insert.sql';
    TableName := GetTableName(MemoSQL.Lines);
    s.Clear;

    for row := 0 to RowCount - 1 do begin
        s.Add('INSERT INTO ' + TableName + ' (' + FieldList.DelimitedText + ')');
        txt := '';
        for col := 0 to ColCount - 1 do begin
            case FieldRecom[col] of
                'I': if Trim(data[col, row]) = '' then txt := txt + 'NULL' + ', ' else txt := txt + data[col, row] + ', ';
                'F': if Trim(data[col, row]) = '' then txt := txt + 'NULL' + ', ' else txt := txt + StringReplace(data[col, row], ',', '.', [rfReplaceAll]) + ', ';
                'D': if Trim(data[col, row]) = '' then txt := txt + 'NULL' + ', ' else txt := txt + '''' + data[col, row] + ''', ';
                'S': if Trim(data[col, row]) = '' then txt := txt + 'NULL' + ', ' else txt := txt + '''' + UTF8ToCP1251(StringReplace(data[col, row], '''', '’', [rfReplaceAll])) + ''', ';
            end;
        end;
        txt := Copy(txt, 1, Length(txt) - 2);
        s.Add('VALUES (' + txt + ');');
        s.Add('');
    end;

    s.SaveToFile(StartDir + FileName);

    s.Free;
    ShowMessage('Scripts for loading in Oracle are created!');
end;

end.
