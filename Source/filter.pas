unit filter;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type

    { TFormFilter }

    TFormFilter = class(TForm)
        Memo1: TMemo;
        SG: TStringGrid;
        procedure SGSelectCell(Sender: TObject; aCol, aRow: Integer;
            var CanSelect: Boolean);
    private

    public

    end;

var
    FormFilter: TFormFilter;

implementation

{$R *.lfm}

{ TFormFilter }

procedure TFormFilter.SGSelectCell(Sender: TObject; aCol, aRow: Integer;
    var CanSelect: Boolean);
begin
    CanSelect := True;
    Memo1.Text := SG.Cells[aCol, aRow];
end;

end.

