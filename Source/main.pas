unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  EditBtn, ComCtrls, Buttons,
  IniFiles, SdfData, LConvEncoding, LazUTF8,
  analysis, map, data,
  functions, Fields;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonOpen: TButton;
    ButtonMap: TButton;
    ButtonAnalysis: TButton;
    CheckBoxAutoSize: TCheckBox;
    CheckBoxRegEx: TCheckBox;
    OpenDialog: TOpenDialog;
    SDF: TSdfDataSet;
    SG: TStringGrid;
    StatusBar: TStatusBar;
    procedure ButtonMapClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure CheckBoxAutoSizeChange(Sender: TObject);
    function CheckColType(col: Integer): TField;
    procedure ButtonAnalysisClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SGSelectCell(Sender: TObject; aCol, aRow: Integer;
        var CanSelect: Boolean);
    procedure read_ini(ConfigFile: String);
    procedure write_ini(ConfigFile: String);
    procedure ReadCSV;
    procedure Analysis;
  private

  public
    StartDir, Value, OraUser, OraService, OraTable, SQL: String;
    max_length, max_type: Array of Integer;
    ColRes, max_text, RegEx: Array of String;
    Fields: TStringList;
    Field: TField;
    Debug: Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.read_ini(ConfigFile: String);
var
    ini: TIniFile;
begin
    Debug := False;
    Form1.ButtonMap.Visible := False;

    if not FileExists(ConfigFile) then Exit;

    try
        ini := TIniFile.Create(ConfigFile);
            Form1.CheckBoxRegEx.Checked := ini.ReadBool('Main', 'UseRegEx', False);
            if ini.ReadBool('Main', 'Maximize', False) then Form1.WindowState := wsMaximized;
            Form1.ButtonMap.Visible := ini.ReadBool('Main', 'Map', False);
            Debug := ini.ReadBool('Main', 'Debug', False);
            SetLength(RegEx, 3);
            RegEx[0] := ini.ReadString('RegEx', 'RE_Number', '');
            RegEx[1] := ini.ReadString('RegEx', 'RE_Float', '');
            RegEx[2] := ini.ReadString('RegEx', 'RE_Date', '');
            OraUser := ini.ReadString('Oracle', 'User', '');
            OraService := ini.ReadString('Oracle', 'Service', '');
        ini.Free;
    except

    end;
end;

procedure TForm1.write_ini(ConfigFile: String);
var
    ini: TIniFile;
begin
    try
        ini := TIniFile.Create(ConfigFile);
            ini.WriteBool('Main', 'UseRegEx', CheckBoxRegEx.Checked);
            if Form1.WindowState = wsMaximized
                then ini.WriteBool('Main', 'Maximize', True)
                else ini.WriteBool('Main', 'Maximize', False);
            ini.WriteString('Oracle', 'User', OraUser);
            ini.WriteString('Oracle', 'Service', OraService);
        ini.Free;
    except

    end;
end;


function TForm1.CheckColType(col: Integer): TField;
var
    row, records: Integer;
    s, res: String;
begin
    records := Form1.SG.RowCount - 1;

    for row := 1 to Form1.SG.RowCount - 1 do begin
        s := Form1.SG.Cells[col, row];
        if length(s) > Field.length then Field.length := length(s);

        if Form1.CheckBoxRegEx.Checked
            then res := CheckCellTypeRE(s, RegEx)
            else res := CheckCellType(s);

        case res of
            'S': inc(Field.k_string);
            'I': inc(Field.k_int);
            'F': inc(Field.k_float);
            'D': inc(Field.k_date);
            'N': inc(Field.k_null);
            'E': inc(Field.k_error);
        end;

        FormMap.SG.Cells[col, row] := res;
    end;

    if records = Field.k_string + Field.k_null then Field.recommend := 'S';
    if records = Field.k_float + Field.k_int + Field.k_null then Field.recommend := 'F';
    if records = Field.k_int + Field.k_null then Field.recommend := 'I';
    if records = Field.k_date + Field.k_null then Field.recommend := 'D';
    if records = Field.k_null then Field.recommend := 'S';

    Result := Field;
end;

procedure TForm1.ButtonMapClick(Sender: TObject);
begin
    FormMap.Show;
end;

procedure TForm1.ButtonOpenClick(Sender: TObject);
begin
    if Debug
        then OpenDialog.FileName := StartDir + 'test.csv'
        else OpenDialog.Execute;

    if FileExists(OpenDialog.FileName) then ReadCSV;
    if (SG.RowCount > 1) and (SG.ColCount > 1) then begin
        ButtonAnalysis.Enabled := True;
        CheckBoxRegEx.Visible := True;
        CheckBoxAutoSize.Visible := True;
    end else begin
        ButtonAnalysis.Enabled := False;
        CheckBoxRegEx.Visible := False;
        CheckBoxAutoSize.Visible := False;
    end;
end;

procedure TForm1.CheckBoxAutoSizeChange(Sender: TObject);
var
    col: Integer;
begin
    if CheckBoxAutoSize.Checked
        then SG.AutoSizeColumns
        else begin
            for col := 0 to SG.ColCount - 1 do
                SG.ColWidths[col] := SG.DefaultColWidth;
        end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    Form1.Caption := 'CSV to ORA v.0.90';
    StartDir := ExtractFilePath(ParamStr(0));
    OpenDialog.InitialDir := StartDir;
    read_ini(StartDir + 'config.ini');
    Fields := TStringList.Create;
end;

procedure TForm1.SGSelectCell(Sender: TObject; aCol, aRow: Integer;
    var CanSelect: Boolean);
begin
    CanSelect := True;
    StatusBar.Panels.Items[1].Text := SG.Cells[aCol, aRow];
end;

procedure TForm1.ReadCSV;
var
    col, row: Integer;
begin
    if SDF.Active then Exit;

    OraTable := ChangeFileExt(ExtractFileName(OpenDialog.FileName.ToUpper), '');
    SG.Visible := False;
    SG.Clean;

    // SDFDataSet Options
    SDF.FileName := OpenDialog.FileName;
    SDF.ReadOnly := True;
    SDF.Delimiter := ';';
    SDF.FirstLineAsSchema := True;
    SDF.AllowMultiLine := True;
    SDF.Active := True;

    SG.ColCount := SDF.FieldCount;
    FormData.SG.ColCount := SDF.FieldCount;
    SetLength(max_length, SDF.FieldCount);
    SetLength(max_type, SDF.FieldCount);
    SetLength(max_text, SDF.FieldCount);

    SDF.Fields.GetFieldNames(Fields);

    // Инициализация значений
    for col := 0 to Fields.Count - 1 do begin
        SG.Cells[col, 0] := Fields.Strings[col];
        FormData.SG.Cells[col, 0] := Fields.Strings[col];
        max_length[col] := 0;
        max_type[col] := 0;
        max_text[col] := '';
    end;

    row := 1;
    while (not SDF.EOF) do begin
        SG.RowCount := row + 1;
        for col := 0 to SDF.FieldCount - 1 do begin
            Value := CP1251ToUTF8(SDF.Fields.Fields[col].AsString);
            SG.Cells[col, row] := Value;
            if (Length(Value)) > max_length[col] then begin
                max_length[col] := Length(Value);
                max_text[col] := Value;
            end;
        end;

        // Если вся строка содержит пустые ячейки то удалить ее
        if Trim(SG.Rows[row].Text) = '' then
            SG.DeleteRow(row);

        SDF.Next;
        inc(row);
    end;

    FormData.SG.RowCount := SG.RowCount;

    for col := 0 to SG.ColCount - 1 do
        for row := 1 to SG.RowCount - 1 do
            FormData.SG.Cells[col, row] := SG.Cells[col, row];

    SDF.Active := False;
    SG.Visible := True;

    StatusBar.Panels.Items[0].Text := 'Fields: ' + IntToStr(SG.ColCount) + ', Records: ' + IntToStr(SG.RowCount - 1);
    FormAnalysis.StatusBar.Panels[0].Text := StatusBar.Panels[0].Text;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    write_ini(StartDir + 'config.ini');
end;

procedure TForm1.Analysis;
var
    FieldName: String;
    col: Integer;
    StartTime, StopTime, WorkTime: cardinal;
begin
    StartTime := GetTickCount64;
    WorkTime := 0;

    Field := TField.Create;

    SQL := 'CREATE TABLE ' + OraTable.ToLower + chr(13)+chr(10) + '(' + chr(13)+chr(10);
    FormMap.SG.ColCount := SG.ColCount;
    FormMap.SG.RowCount := SG.RowCount;
    FormMap.SG.Rows[0] := SG.Rows[0];
    FormAnalysis.SG.RowCount := SG.ColCount + 1;

    FormMap.SG.Visible := False;
    FormAnalysis.SG.ColWidths[0] := 80;

    for col := 0 to SG.ColCount - 1 do begin
        FieldName := SG.Cells[col, 0].ToLower;
        Field.default;
        Field := CheckColType(col);
        FormAnalysis.SG.Cells[0, col + 1] := FieldName.ToUpper;
        FormAnalysis.SG.Cells[1, col + 1] := IntToStr(Field.k_int);
        FormAnalysis.SG.Cells[2, col + 1] := IntToStr(Field.k_float);
        FormAnalysis.SG.Cells[3, col + 1] := IntToStr(Field.k_date);
        FormAnalysis.SG.Cells[4, col + 1] := IntToStr(Field.k_string);
        FormAnalysis.SG.Cells[5, col + 1] := IntToStr(Field.k_null);
        FormAnalysis.SG.Cells[6, col + 1] := IntToStr(Field.k_error);

        case Field.recommend of
            'I': SQL := SQL + '    ' + FieldName + ' NUMBER(' + IntToStr(Field.length) + '),' + chr(13)+chr(10);
            'F': SQL := SQL + '    ' + FieldName + ' NUMBER,' + chr(13)+chr(10);
            'D': SQL := SQL + '    ' + FieldName + ' DATE,' + chr(13)+chr(10);
            'S': SQL := SQL + '    ' + FieldName + ' VARCHAR2(' + IntToStr(Field.length) + '),' + chr(13)+chr(10);
        end;
    end;

    FormMap.SG.Visible := True;

    FormAnalysis.StatusBar.Panels.Items[1].Text := 'Analysis time: ' + IntToStr(WorkTime) + ' ms / ' + IntToStr(Round(WorkTime/1000)) + ' sec';

    SQL := copy(SQL, 0, Length(SQL) - 3);
    SQL := SQL + chr(13)+chr(10) + ');';
    SQL := SQL + chr(13)+chr(10) + chr(13)+chr(10);
    SQL := SQL + 'quit;';

    FormAnalysis.Memo1.Text := SQL;

    Field.Free;
    StopTime := GetTickCount64;
    WorkTime := StopTime - StartTime;
end;

procedure TForm1.ButtonAnalysisClick(Sender: TObject);
begin
    Analysis;
    FormAnalysis.UseRegEx := CheckBoxRegEx.Checked;
    FormAnalysis.RegEx := RegEx;
    FormAnalysis.StartDir := StartDir;
    FormAnalysis.csv := ExtractFileName(SDF.FileName);
    FormAnalysis.Fields := Fields;
    FormAnalysis.EditUser.Text := OraUser;
    FormAnalysis.EditService.Text := OraService;

    FormAnalysis.ShowModal;

    OraUser := FormAnalysis.EditUser.Text;
    OraService := FormAnalysis.EditService.Text;
end;

end.