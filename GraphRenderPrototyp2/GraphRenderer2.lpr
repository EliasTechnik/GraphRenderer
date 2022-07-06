program GraphRenderer2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, JsonTools, CustApp, unit_graph, unit_paint, unit_graphmanager;

type

  { TGraphRenderer }

  TGraphRenderer = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function wantHelp:boolean; //true if user wants help
    procedure displayHelp;     //prints help page
    function getParam(key:string;shortkey:string;out value:string):boolean; //true if param is given
    procedure translateCoordiantes(coordinateFile:string; gm:tgraph);
  end;

{ TGraphRenderer }

procedure TGraphRenderer.DoRun;
var
  ErrorMsg: String;
  prog:tgraph;
  v:string;
  keepalive:boolean;
  outputpath:string;
  j:tjsonnode;
begin
  keepalive:=true;
  if paramcount>1 then begin
    if wantHelp then displayHelp
    else begin
      //programcode
      prog:=tgraph.create();
      if getParam('-a','-automatic',v) then keepalive:=false;
      if getParam('-c','-config',v) then begin
        Writeln('# Loading config "'+v+'"...');
        prog.loadConfigFromJSON(v); //config.json
      end
      else begin
        v:='config.json';
        WriteLn('# Using default config: "'+v+'"');
        prog.loadConfigFromJSON(v);
      end;
      if getParam('-s','-save',v) then begin
        Writeln('# Output path: '+v);
        outputpath:=v;
      end
      else begin
          outputpath:='';
          Writeln('# No output path was given. Using current directory instead.');
      end;
      if getParam('-t','-translate',v) then begin
         Writeln('# Entered translation mode. Loading coordinates from: "'+v+'"');
         translateCoordiantes(v,prog);
      end
      else begin
        if getParam('-l','-load',v) then begin
          WriteLn('# Loading JSON Graph data from "'+v+'"');
          prog.loadFromJSONFile(v);
          WriteLn('# Render OCM Chunks...');
          prog.render(outputpath,'subchunk_');
        end
        else begin
          WriteLn('# Error: No graph data was found.');
        end;
      end;
    end;
  end
  else begin
    displayHelp;
  end;
  if keepalive then begin
      WriteLn('Press any key to exit...');
      Readln();
  end;
  Terminate;
  Exit;
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

function TGraphRenderer.wantHelp: boolean;
var i:integer;
begin
  // Prüft alle übergebenen Parameter, ob einer von Ihnen die Hilfe aufruft
  // ParamStr(0) enthält immer Pfad und Dateiname
  for i := 1 to ParamCount do
  begin
    // Wandelt den Parameter in Kleinbuchstaben um
    case LowerCase(ParamStr(i)) of
      '-?', '--?', '/?':
        Result := True;
      '-h', '--h', '/h':
        Result := True;
      '-help', '--help', '/help':
        Result := True;
      '-hilfe', '--hilfe', '/hilfe':
        Result := True;
      else
        Result := False;
    end;
  end;
end;

procedure TGraphRenderer.displayHelp;
begin
  writeln('Usage: ', extractfilename(ExeName), '   -h  or  -help   | to show this info');
  writeln('Usage: ', extractfilename(ExeName), '   -l <filepath to json> or  -load <filepath to json> | to load Graph');
  writeln('Usage: ', extractfilename(ExeName), '   -c <filepath to config> or  -config <filepath to config> | to use custom cofigfile');
  writeln('Usage: ', extractfilename(ExeName), '   -a  or  -automatic   | the program runs without user input');
  writeln('Usage: ', extractfilename(ExeName), '   -s <path to outputfolder> or  -save <path to outputfolder> | specifies the folder where the subchunks are saved');
  writeln('Usage: ', extractfilename(ExeName), '   -t <path to WGS84 JSON file> or -translate <path to WGS84 JSON file> | a set of WGS84 cooordinates are loaded and converted and printed out on the console');
end;

function TGraphRenderer.getParam(key: string; shortkey: string; out
  value: string): boolean;
var i:integer;
begin
  result:=false;
  for i:=1 to paramcount do begin
    if (LowerCase(ParamStr(i))=key) or(LowerCase(ParamStr(i))=shortkey) then begin
       result:=true;
       if (i+1)<=paramcount then value:=ParamStr(i+1)
       else value:='';
    end;
  end;
end;

procedure TGraphRenderer.translateCoordiantes(coordinateFile: string; gm: tgraph
  );
var j,ja:tjsonnode;
    i:integer;
    gps:twgs84;
    carthesian:t3d;
    loc:t2d;
begin
  j:=tjsonnode.Create;
  j.LoadFromFile(coordinateFile);
  ja:=j.AsArray;
  if gm.pConfigured then begin
    for i:=0 to ja.Count-1 do begin
      DefaultFormatSettings.DecimalSeparator := '.';    //change to decimal point
      gps.lat:=strtofloat(ja.Child(i).Find('lat').AsString);
      gps.lon:=strtofloat(ja.Child(i).Find('lon').AsString);
      carthesian:=gpsto3d(gps,gm.pWGS84Params);
      //loc:=
    end;
  end;


end;

var
  Application: TGraphRenderer;
begin
  Application:=TGraphRenderer.Create(nil);
  Application.Title:='GraphRenderer2';
  Application.Run;
  Application.Free;
end.

