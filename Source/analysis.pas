unit analysis;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
    ExtCtrls, ComCtrls, functions, data, filter;

type

    { TFormAnalysis }

    TFormAnalysis = class(TForm)
        ButtonFinish: TButton;
        EditService: TEdit;
        EditUser: TEdit;
        GroupBox1: TGroupBox;
        LabelService: TLabel;
        LabelUser: TLabel;
        Memo1: TMemo;
        Panel1: TPanel;
        SG: TStringGrid;
        Splitter1: TSplitter;
        StatusBar: TStatusBar;
        procedure ButtonFinishClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure SGDblClick(Sender: TObject);
    private

    public
       StartDir, csv: String;
       RegEx: Array of String;
       UseRegEx: Boolean;
       Fields: TStringList;
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
    for col := 0 to FormData.SG.ColCount - 1 do
        if FieldName.ToUpper = FormData.SG.Cells[col, 0].ToUpper
            then FieldId := col;

    FormFilter.SG.Clear;
    FormFilter.SG.ColCount := FormData.SG.ColCount;
    FormFilter.SG.RowCount := 1;
    for col := 0 to FormData.SG.ColCount - 1 do
        FormFilter.SG.Cells[col, 0] := FormData.SG.Cells[col, 0];

    case FindType of
        'INT':   FieldType := 'I';
        'FLOAT': FieldType := 'F';
        'DATE':  FieldType := 'D';
        'TEXT':  FieldType := 'S';
        'NULL':  FieldType := 'N';
        'ERR':   FieldType := 'E';
    end;

    for row := 1 to FormData.SG.RowCount - 1 do begin
      if UseRegEx
          then CheckResult := CheckCellTypeRE(FormData.SG.Cells[FieldId, row], RegEx)
          else CheckResult := CheckCellType(FormData.SG.Cells[FieldId, row]);
      if CheckResult = FieldType then begin
            FormFilter.SG.RowCount := FormFilter.SG.RowCount + 1;
            for col := 0 to FormData.SG.ColCount - 1 do
                FormFilter.SG.Cells[col, FormFilter.SG.RowCount - 1] := FormData.SG.Cells[col, row];
        end;
    end;

    FormFilter.Show;
end;

procedure TFormAnalysis.ButtonFinishClick(Sender: TObject);
var
    FileName, TableName: String;
    s: TStrings;
begin
    s := TStringList.Create;

    // Create file Run.bat
    FileName := 'Run.bat';
    if FileExists(StartDir + 'Template\' + FileName) then begin
        s.LoadFromFile(StartDir + 'Template\' + FileName);
        s.Text := StringReplace(s.Text, '#username#', EditUser.Text, [rfReplaceAll]);
        s.Text := StringReplace(s.Text, '#service#', EditService.Text, [rfReplaceAll]);
        s.SaveToFile(StartDir + FileName);
    end;

    // Create file create.sql
    Memo1.Lines.SaveToFile(StartDir + 'create.sql');

    // Create file loader.ctl
    FileName := 'loader.ctl';
    if FileExists(StartDir + 'Template\' + FileName) then begin
        s.LoadFromFile(StartDir + 'Template\' + FileName);
        s.Text := StringReplace(s.Text, '#csv#', csv, [rfReplaceAll]);
        TableName := GetTableName(Memo1.Lines);
        s.Text := StringReplace(s.Text, '#table#', TableName, [rfReplaceAll]);
        s.Text := StringReplace(s.Text, '#fields#', fields.DelimitedText, [rfReplaceAll]);
        s.SaveToFile(StartDir + FileName);
    end;

    s.Free;

    ShowMessage('Scripts for loading in Oracle are created!');
end;

end.
