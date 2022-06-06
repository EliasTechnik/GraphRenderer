unit unit_graphmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsontools, FPImage, FPCanvas, FPImgCanv,
     FPWritePNG, graphics, math, unit_graph, unit_paint;

type

  { tGraph }

 tGraph=class
  private
    ways:tlist;
    map_corners:array [0..3] of t3d;
    map_borders:array [0..3] of tray;
    pixelwidth:double; //width of one pixel in m
    origin:twgs84;
    base:t3d;
    plane:tTangentPlane;
    radius:double;
    baseunit:string;
    scalefactor:integer;
    mp:tmappainter;
    procedure compute_edge(n,w,e,s:double);
    function get_baseunit: string;
    function get_pixelwidth: double;
    function get_scalefactor: integer;
  public
    constructor create();
    procedure loadFromJSONFile(path:string);
    procedure renderImageToFile(path:string);
    procedure loadConfigFromJSON(path:string);
    property pPixelwidth:double read get_pixelwidth;
    property pScalefactor:integer read get_scalefactor;
    property pBaseunit:string read get_baseunit;
end;

implementation

{ tGraph }

procedure tGraph.compute_edge(n, w, e, s: double);
var gps:twgs84;
    ray:tray;
begin
  gps.lat:=n;
  gps.lon:=w;
  ray:=getray(base,gpsto3d(gps,radius));
  map_corners[0]:=get3donplane(ray,plane);      //north-west
  gps.lat:=n;
  gps.lon:=e;
  ray:=getray(base,gpsto3d(gps,radius));
  map_corners[1]:=get3donplane(ray,plane);      //north-east
  gps.lat:=s;
  gps.lon:=e;
  ray:=getray(base,gpsto3d(gps,radius));
  map_corners[2]:=get3donplane(ray,plane);      //south-east
  gps.lat:=s;
  gps.lon:=w;
  ray:=getray(base,gpsto3d(gps,radius));
  map_corners[3]:=get3donplane(ray,plane);      //south-west

  Writeln('# The Plane NW corners are at ('+floattostr(map_corners[0].x)+'|'+floattostr(map_corners[0].y)+'|'+floattostr(map_corners[0].z)+')');
  Writeln('# The Plane NE corners are at ('+floattostr(map_corners[1].x)+'|'+floattostr(map_corners[1].y)+'|'+floattostr(map_corners[1].z)+')');
  Writeln('# The Plane SE corners are at ('+floattostr(map_corners[2].x)+'|'+floattostr(map_corners[2].y)+'|'+floattostr(map_corners[2].z)+')');
  Writeln('# The Plane SW corners are at ('+floattostr(map_corners[3].x)+'|'+floattostr(map_corners[3].y)+'|'+floattostr(map_corners[3].z)+')');

  map_borders[0]:=getray(map_corners[0],map_corners[1]);   //north
  map_borders[1]:=getray(map_corners[1],map_corners[2]);   //east
  map_borders[2]:=getray(map_corners[3],map_corners[2]);   //south
  map_borders[3]:=getray(map_corners[0],map_corners[3]);   //west

end;

function tGraph.get_baseunit: string;
begin
  result:=baseunit;
end;

function tGraph.get_pixelwidth: double;
begin
  result:=pixelwidth;
end;

function tGraph.get_scalefactor: integer;
begin
  result:=scalefactor;
end;

constructor tGraph.create();
begin
  ways:=tlist.Create;
end;

procedure tGraph.loadFromJSONFile(path: string);
var j,graph,way,nd,tag:tjsonnode;
    wi,nodes,t:integer;
    w:tway;
    k,v:string;
    wgs84:twgs84;
begin
  j:=tjsonnode.Create;
  j.LoadFromFile(path);
  if j.find('graph/way',graph) then begin
     graph:=graph.AsArray;
     for wi:=0 to graph.Count-1 do begin
        way:=graph.Child(wi);
        w:=tway.create(strtoint(way.Find('@id').AsString),self);
        nd:=way.Find('nd');
        for nodes:=0 to nd.Count-1 do begin
            DefaultFormatSettings.DecimalSeparator := '.';    //change to decimal point
            wgs84.lat:=strtofloat(nd.Child(nodes).Find('@lat').AsString);
            wgs84.lon:=strtofloat(nd.Child(nodes).Find('@lon').AsString);
            //writeln('Node '+floattostr(wgs84.lat)+' '+floattostr(wgs84.lon)+' added');
            w.add_node(tGraphNode.create(strtoint(nd.Child(nodes).Find('@id').AsString),wgs84));
        end;
        tag:=way.Find('tag');
        for t:=0 to tag.count-1 do begin
           k:=tag.child(t).Find('@k').AsString;
           v:=tag.child(t).Find('@v').AsString;
           if k='width' then w.pWidth:=strtofloat(v);
           //TODO: parse other tags (surface, maxspeed, ...
        end;
        ways.Add(w);
     end;
  end;
end;

procedure tGraph.renderImageToFile(path: string);
var
    i,j,x,y,width,height:integer;
    w:tway;
    p:t3d;
    canvas : TFPCustomCanvas;
    image : TFPCustomImage;
    writer : TFPCustomImageWriter;
    n:tgraphnode;
begin
  //generate 3d coordiantes
  n:=tway(ways.Items[0]).Nodes[0];
  for i:=0 to ways.Count-1 do begin
   w:=tway(ways.Items[i]);
   for j:=0 to w.Count-1 do begin
    w.Nodes[j].project(plane,base,radius);
    Writeln('distance to prev. node:'+floattostr(distance3d(n.p3d,w.Nodes[j].p3d))+baseunit);
    n:=w.Nodes[j];
   end;
  end;

  //build chunkmap
  mp.create(1000);  //1000x1000 for now -->better: get it from config file or command input

  //save chunks image

  width:=round(distance3d(map_corners[0],map_corners[1])*(1/pixelwidth));
  height:=round(distance3d(map_corners[0],map_corners[3])*(1/pixelwidth));
  Writeln('Output canvas dimmension (w,h): '+inttostr(width)+','+inttostr(height));

end;

procedure tGraph.loadConfigFromJSON(path: string);
var j,res:tjsonnode;
    north,west,south,east:double;
begin
    j:=tjsonnode.Create;
    j.LoadFromFile(path);
    DefaultFormatSettings.DecimalSeparator := '.';    //change to decimal point
    if j.Find('WGS84/unit',res) then begin
       baseunit:=res.AsString;
       Writeln('# Baseunit: '+baseunit);
       if baseunit='km' then scalefactor:=100;
       if baseunit='m' then scalefactor:=1;
    end
    else baseunit:='m';
    if j.Find('WGS84/radius',res) then begin
       radius:=strtofloat(res.AsString);
       Writeln('# Loaded radius: '+floattostr(radius)+baseunit);
    end
    else radius:=earth_radius;
    if j.Find('plane/origin/WGS84/lat',res) then begin
       origin.lat:=strtofloat(res.AsString);
       if j.Find('plane/origin/WGS84/lon',res) then begin
          origin.lon:=strtofloat(res.AsString);
          Writeln('# Loaded origin: lat: '+floattostr(origin.lat)+'° lon: '+floattostr(origin.lon)+'°');
          plane:=genplane(gpsto3d(origin,radius));

          Writeln('# Inverted Origin: polar: '+floattostr(invertpoint(gpstosphere(origin,radius)).polarangle)+'° azimut: '+floattostr(invertpoint(gpstosphere(origin,radius)).azimuthalangle)+'°');
          base:=sphereto3d(invertPoint(gpstosphere(origin,radius)));
          Writeln('# The Plane Origin is at ('+floattostr(gpsto3d(origin,radius).x)+'|'+floattostr(gpsto3d(origin,radius).y)+'|'+floattostr(gpsto3d(origin,radius).z)+')');
          Writeln('# The Plane is (x: '+floattostr(plane.normal.x)+'|y: '+floattostr(plane.normal.y)+'|z: '+floattostr(plane.normal.z)+')');
          Writeln('# The Projection Base is at ('+floattostr(base.x)+'|'+floattostr(base.y)+'|'+floattostr(base.z)+')');
       end;
    end;
    if j.Find('output/pixelwidth',res) then begin
       pixelwidth:=strtofloat(res.AsString);
       Writeln('# Set pixelwidth to '+floattostr(pixelwidth)+' m');
    end;
    if j.Find('plane/edge/WGS84/north_lat',res) then north:=strtofloat(res.AsString);
    if j.Find('plane/edge/WGS84/south_lat',res) then south:=strtofloat(res.AsString);
    if j.Find('plane/edge/WGS84/west_lon',res) then west:=strtofloat(res.AsString);
    if j.Find('plane/edge/WGS84/east_lon',res) then east:=strtofloat(res.AsString);

    compute_edge(north,west,east,south);

end;

end.

