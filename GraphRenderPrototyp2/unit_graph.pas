unit unit_graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsontools;

type tWGS84=record
     lat:double;
     lon:double;
end;

type tSphere=record
     radius:double;
     polarangle:double;
     azimuthalangle:double;
end;

type tPolar=record
     radius:double;
     phi:double;
end;

type tCarthesian=record
     x:QWord;
     y:QWord;
     resolution:double;
end;

type tsurface=(sASPHALT,sPAVED,sUNKNOWN);


type

{ tGraphNode }

 tGraphNode=class
  private
    gps:tWGS84;
    pointZero:tWGS84;
    id:longint;
  public
    constructor create(_id:longint;_gps:tWGS84);
    function get_carthesian:tcarthesian;
end;

type tGraph=class
  private
    ways:tlist;
  public
    constructor create();
    procedure loadFromJSONFile(path:string);
end;

type

{ tWay }

 tWay=class
  private
    id:longint;
    nodes:tlist;
    surface:tsurface;
    width:double;
    maxspeed:double;
  public
    constructor create(_id:longint);
    procedure add_node(_node:tGraphNode);
end;
implementation

{ tWay }

constructor tWay.create(_id: longint);
begin
  id:=_id;
  nodes:=tlist.Create;
end;

procedure tWay.add_node(_node: tGraphNode);
begin
  nodes.Add(_node);
end;

{ tGraph }

constructor tGraph.create();
begin
  ways:=tlist.Create;
end;

procedure tGraph.loadFromJSONFile(path: string);
var j,graph,way,nd:tjsonnode;
    wi,nodes:integer;
    w:tway;
    wgs84:twgs84;
begin
  j:=tjsonnode.Create;
  j.LoadFromFile(path);
  if j.find('graph/way',graph) then begin
     graph:=graph.AsArray;
     for wi:=0 to graph.Count-1 do begin
        way:=graph.Child(wi);
        w:=tway.create(strtoint(way.Find('@id').AsString));
        nd:=way.Find('nd');
        for nodes:=0 to nd.Count-1 do begin
            //writeLn(nd.Child(nodes).Find('@lat').AsString);
            DefaultFormatSettings.DecimalSeparator := '.';    //change to decimal point
            wgs84.lat:=strtofloat(nd.Child(nodes).Find('@lat').AsString);
            wgs84.lon:=strtofloat(nd.Child(nodes).Find('@lon').AsString);
            writeln('Node '+floattostr(wgs84.lat)+' '+floattostr(wgs84.lon)+' added');
            w.add_node(tGraphNode.create(strtoint(nd.Child(nodes).Find('@id').AsString),wgs84));
        end;
        //TODO: parse tag (surface, width, maxspeed
        ways.Add(w);
     end;
  end;

end;

{ tGraphNode }

constructor tGraphNode.create(_id: longint; _gps: tWGS84);
begin
  gps:=_gps;
  id:=_id;
end;

function tGraphNode.get_carthesian: tcarthesian;
begin
  //convert to radians

  //convert gps to sphere

  //convert pointZero to sphere

  //project sphere on plane

  //convert polar to carthesian

  //return carthesian
end;

end.

