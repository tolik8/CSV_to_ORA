unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  EditBtn, ComCtrls, Buttons,
  IniFiles, SdfData, LConvEncoding, LazUTF8,
  analysis,
  functions, Fields;

type
  TAaString = Array of Array of String;

  { TForm1 }

  TForm1 = class(TForm)
    ButtonOpen: TButton;
    ButtonAnalysis: TButton;
    CheckBoxAutoSize: TCheckBox;
    CheckBoxRegEx: TCheckBox;
    OpenDialog: TOpenDialog;
    SDF: TSdfDataSet;
    SG: TStringGrid;
    StatusBar: TStatusBar;
    procedure ButtonOpenClick(Sender: TObject);
    procedure CheckBoxAutoSizeChange(Sender: TObject);
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
    StartDir, OraUser, OraService, OraTable, SQL, DebugCSV: String;
    ColCount, RowCount: Integer;
    RegEx: Array of String;
    FieldList: TStringList;
    Field: TField;
    Debug: Boolean;
    data: TAaString;
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

    if not FileExists(ConfigFile) then Exit;

    try
        ini := TIniFile.Create(ConfigFile);
            Form1.CheckBoxRegEx.Checked := ini.ReadBool('Main', 'UseRegEx', False);
            if ini.ReadBool('Main', 'Maximize', False) then Form1.WindowState := wsMaximized;
            Debug := ini.ReadBool('Main', 'Debug', False);
            DebugCSV := ini.ReadString('Main', 'DebugCSV', 'test.csv');
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

procedure TForm1.ButtonOpenClick(Sender: TObject);
begin
    if Debug and FileExists(StartDir + DebugCSV) then begin
        OpenDialog.FileName := StartDir + DebugCSV;
        ReadCSV;
    end else begin
        OpenDialog.Execute;
        if FileExists(OpenDialog.FileName) then ReadCSV;
    end;

    if (RowCount > 1) and (ColCount > 1) then begin
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
            for col := 0 to ColCount - 1 do
                SG.ColWidths[col] := SG.DefaultColWidth;
        end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    Form1.Caption := 'CSV to ORA v.0.91';
    StartDir := ExtractFilePath(ParamStr(0));
    OpenDialog.InitialDir := StartDir;
    read_ini(StartDir + 'config.ini');
    FieldList := TStringList.Create;
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
    //

    // Получить список с именами полей
    SDF.Fields.GetFieldNames(FieldList);

    // Установить количество полей
    ColCount := SDF.FieldCount;
    SG.ColCount := ColCount;
    FormAnalysis.ColCount := ColCount;
    FormAnalysis.SG.RowCount := ColCount + 1;

    for col := 0 to ColCount - 1 do
        SG.Cells[col, 0] := FieldList.Strings[col];

    SG.RowCount := SDF.RecordCount + 1;
    row := 1;

    while (not SDF.EOF) do begin

        for col := 0 to SDF.FieldCount - 1 do
            SG.Cells[col, row] := CP1251ToUTF8(SDF.Fields.Fields[col].AsString);

        // Если вся строка содержит пустые ячейки то удалить ее
        if Trim(SG.Rows[row].Text) = '' then SG.DeleteRow(row);

        SDF.Next;
        inc(row);
    end;

    // Установить количество строк
    RowCount := SG.RowCount - 1;
    FormAnalysis.RowCount := RowCount;

    // Копия в массив data (для передачи в форму FormAnalysis)
    SetLength(data, ColCount, RowCount);

    for col := 0 to ColCount - 1 do
        for row := 0 to RowCount - 1 do
            data[col, row] := SG.Cells[col, row + 1];

    FormAnalysis.data := data;
    //

    SDF.Active := False;
    SG.Visible := True;

    StatusBar.Panels.Items[0].Text := 'Fields: ' + IntToStr(ColCount) + ', Records: ' + IntToStr(RowCount);
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

    FormAnalysis.SG.RowCount := ColCount + 1;
    FormAnalysis.SG.ColWidths[0] := 80;

    for col := 0 to ColCount - 1 do begin
        FieldName := SG.Cells[col, 0].ToLower;
        Field.default;
        Field := CheckColType(data[col], Field, RegEx, CheckBoxRegEx.Checked);
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

    SQL := copy(SQL, 0, Length(SQL) - 3);
    SQL := SQL + chr(13)+chr(10) + ');';
    SQL := SQL + chr(13)+chr(10) + chr(13)+chr(10);
    SQL := SQL + 'quit;';

    FormAnalysis.MemoSQL.Text := SQL;

    Field.Free;
    StopTime := GetTickCount64;
    WorkTime := StopTime - StartTime;
    FormAnalysis.StatusBar.Panels.Items[1].Text := 'Analysis time: ' + IntToStr(WorkTime) + ' ms / ' + IntToStr(Round(WorkTime/1000)) + ' sec';
end;

procedure TForm1.ButtonAnalysisClick(Sender: TObject);
begin
    Analysis;
    FormAnalysis.UseRegEx := CheckBoxRegEx.Checked;
    FormAnalysis.RegEx := RegEx;
    FormAnalysis.StartDir := StartDir;
    FormAnalysis.csv := ExtractFileName(SDF.FileName);
    FormAnalysis.FieldList := FieldList;
    FormAnalysis.EditUser.Text := OraUser;
    FormAnalysis.EditService.Text := OraService;

    FormAnalysis.ShowModal;

    OraUser := FormAnalysis.EditUser.Text;
    OraService := FormAnalysis.EditService.Text;
end;

end.
