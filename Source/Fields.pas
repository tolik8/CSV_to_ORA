unit Fields;
 
interface
 
type
  TField = class(TObject)
  public
    length, k_int, k_float, k_date, k_string, k_null, k_error: Integer;
    recommend: String;
    constructor Create; overload;
    procedure default;
  end;
 
implementation

constructor TField.Create;
begin
    inherited;
    default;
end;

procedure TField.default;
begin
    length := 0;
    k_int := 0;
    k_float := 0;
    k_date := 0;
    k_string := 0;
    k_null := 0;
    k_error := 0;
    recommend := 'S';
end;

end.
