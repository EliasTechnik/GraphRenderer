program GraphRenderer2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, JsonTools, CustApp, unit_graph;

type

  { TGraphRenderer }

  TGraphRenderer = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TGraphRenderer }

procedure TGraphRenderer.DoRun;
var
  ErrorMsg: String;
  prog:tgraph;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  ErrorMsg:=CheckOptions('l', 'loadfile');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('l', 'loadfile') then begin
    WriteLn('Loading JSON...');
    prog:=tgraph.create();
    prog.loadConfigFromJSON('config.json');
    //prog.loadFromJSONFile(paramStr(2));
    //prog.renderImageToFile('testimage.png');


    //wait to show output
    ReadLn();
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TGraphRenderer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TGraphRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TGraphRenderer.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h to | show this info');
  writeln('Usage: ', ExeName, ' -l <filepath to json> | to load Graph');
end;

var
  Application: TGraphRenderer;
begin
  Application:=TGraphRenderer.Create(nil);
  Application.Title:='GraphRenderer';
  Application.Run;
  Application.Free;
end.

