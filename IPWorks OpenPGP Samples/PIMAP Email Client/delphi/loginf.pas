unit loginf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormLogin = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EditUser: TEdit;
    EditPassword: TEdit;
    EditIMAPServer: TEdit;
    Button1: TButton;
    Button2: TButton;
    EditIMAPPort: TEdit;
    Label4: TLabel;

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormLogin: TFormLogin;

implementation

{$R *.DFM}



end.
