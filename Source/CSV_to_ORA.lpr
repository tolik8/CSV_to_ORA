program CSV_to_ORA;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sdflaz, main, functions, analysis, filter;

{$R *.res}

begin
    RequireDerivedFormResource := True;
    Application.Scaled := True;
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
        Application.CreateForm(TFormAnalysis, FormAnalysis);
        Application.CreateForm(TFormFilter, FormFilter);
    Application.Run;
end.

