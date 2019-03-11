unit map;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids;

type

    { TFormMap }

    TFormMap = class(TForm)
        SG: TStringGrid;
    private

    public

    end;

var
    FormMap: TFormMap;

implementation

{$R *.lfm}

end.

