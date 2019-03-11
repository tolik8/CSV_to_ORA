unit functions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, RegExpr;

function CheckCellType(input: String): String;
function CheckCellTypeRE(input: String; RegEx: Array of String): String;
function GetTableName(input: TStrings): String;

implementation

// Функция для проверки содержимого ячейки
// Возвращает первую букву от Integer, Float, Date, String, Null, Error
function CheckCellType(input: String): String;
var
    dt: TDateTime;
    num1: Int64;
    num2: Extended;
begin
    try
        Result := 'S';
        //input := StringReplace(input, ' ', '', [rfReplaceAll]);

        if input = '' then begin
            Result := 'N';
            Exit;
        end;

        if (TryStrToDate(input, dt)) and (Length(input) = 10) then Result := 'D';
        if TryStrToFloat(input, num2) then Result := 'F';
        if TryStrToInt64(input, num1) then Result := 'I';
    except
        Result := 'E';
    end;
end;

// Функция для проверки содержимого ячейки (использует регулярные выражения)
// Возвращает первую букву от Integer, Float, Date, String, Null, Error
function CheckCellTypeRE(input: String; RegEx: Array of String): String;
var
    re: TRegExpr;
begin
    try
        Result := 'S';
        //input := StringReplace(input, ' ', '', [rfReplaceAll]);

        if input = '' then begin
            Result := 'N';
            Exit;
        end;

        re := TRegExpr.Create;

            //'^(0[1-9]|[12][0-9]|3[01])[- /.](0[1-9]|1[012])[- /.](19|20)\d\d$'
            re.Expression := RegEx[2];
            if re.Exec(input) then begin
                Result := 'D';
                Exit;
            end;

            //'^[+-]?[0-9]+$'
            re.Expression := RegEx[0];
            if re.Exec(input) then begin
                Result := 'I';
                Exit;
            end;

            //'^[+-]?[0-9]*[,.]*[0-9]+$'
            re.Expression := RegEx[1];
            if re.Exec(input) then Result := 'F';

        re.Free;
    except
        Result := 'E';
    end;
end;

// Функция возвращает имя таблицы из скрипта CREATE TABLE
function GetTableName(input: TStrings): String;
var
    i: Integer;
    Line: TStringList;
begin
    Result := '';
    Line := TStringList.Create;
    for i := 0 to input.Count do begin
        Line.Delimiter := ' ';
        Line.DelimitedText := input.Strings[i].ToUpper;
        try
            if (Line.Strings[0] = 'CREATE') and (Line.Strings[1] = 'TABLE')
                then Result := Line.Strings[2].ToLower;
        except
        end;
    end;
    Line.Free;
end;

end.

