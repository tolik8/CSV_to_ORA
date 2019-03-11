unit data;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids;

type

    { TFormData }

    TFormData = class(TForm)
        SG: TStringGrid;
    private

    public

    end;

var
    FormData: TFormData;

implementation

{$R *.lfm}

end.

