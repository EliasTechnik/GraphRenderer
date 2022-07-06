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
    wgs84:twgs84params;
    baseunit:string;
    scalefactor:integer;
    subchunksize:double;
    mp:tmappainter;
    jconfig:tjsonnode;
    configured:boolean; //true if the manager is configured
    procedure compute_edge(n,w,e,s:double);
    function get_baseunit: string;
    function get_configured: boolean;
    function get_pixelwidth: double;
    function get_scalefactor: integer;
  public
    constructor create();
    procedure loadFromJSONFile(path:string);
    procedure render(path:string;prefix:string);  //renders the chunks to a specified ouput folder
    procedure loadConfigFromJSON(path:string);
    property pPixelwidth:double read get_pixelwidth;
    property pScalefactor:integer read get_scalefactor;
    property pBaseunit:string read get_baseunit;
    property pConfigured:boolean read get_configured;
    function translateSingleCoordiante(gps:tWGS84):t2d;  //translates one wgs84 coordinate into 2D
end;

implementation

{ tGraph }

procedure tGraph.compute_edge(n, w, e, s: double);
var gps:twgs84;
    ray:tray;
begin
  gps.lat:=n;
  gps.lon:=w;
  ray:=getray(base,gpsto3d(gps,wgs84));
  map_corners[0]:=get3donplane(ray,plane);      //north-west
  gps.lat:=n;
  gps.lon:=e;
  ray:=getray(base,gpsto3d(gps,wgs84));
  map_corners[1]:=get3donplane(ray,plane);      //north-east
  gps.lat:=s;
  gps.lon:=e;
  ray:=getray(base,gpsto3d(gps,wgs84));
  map_corners[2]:=get3donplane(ray,plane);      //south-east
  gps.lat:=s;
  gps.lon:=w;
  ray:=getray(base,gpsto3d(gps,wgs84));
  map_corners[3]:=get3donplane(ray,plane);      //south-west

  Writeln('# The Plane NW corner are at ('+floattostr(map_corners[0].x)+'|'+floattostr(map_corners[0].y)+'|'+floattostr(map_corners[0].z)+')');
  Writeln('# The Plane NE corner are at ('+floattostr(map_corners[1].x)+'|'+floattostr(map_corners[1].y)+'|'+floattostr(map_corners[1].z)+')');
  Writeln('# The Plane SE corner are at ('+floattostr(map_corners[2].x)+'|'+floattostr(map_corners[2].y)+'|'+floattostr(map_corners[2].z)+')');
  Writeln('# The Plane SW corner are at ('+floattostr(map_corners[3].x)+'|'+floattostr(map_corners[3].y)+'|'+floattostr(map_corners[3].z)+')');

  map_borders[0]:=getray(map_corners[0],map_corners[1]);   //north
  map_borders[1]:=getray(map_corners[1],map_corners[2]);   //east
  map_borders[2]:=getray(map_corners[3],map_corners[2]);   //south
  map_borders[3]:=getray(map_corners[0],map_corners[3]);   //west

end;

function tGraph.get_baseunit: string;
begin
  result:=baseunit;
end;

function tGraph.get_configured: boolean;
begin
  result:=configured;
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
  configured:=false;
end;

procedure tGraph.loadFromJSONFile(path: string);
var j,graph,way,nd,tag:tjsonnode;
    wi,nodes,t:integer;
    w:tway;
    k,v:string;
    gps:twgs84;
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
            gps.lat:=strtofloat(nd.Child(nodes).Find('@lat').AsString);
            gps.lon:=strtofloat(nd.Child(nodes).Find('@lon').AsString);
            //writeln('Node '+floattostr(wgs84.lat)+' '+floattostr(wgs84.lon)+' added');
            w.add_node(tGraphNode.create(strtoint(nd.Child(nodes).Find('@id').AsString),gps));
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

procedure tGraph.render(path: string; prefix: string);
var
    i,j,x,y,width,height:integer;
    w:tway;
    p:t3d;
    canvas : TFPCustomCanvas;
    image : TFPCustomImage;
    writer : TFPCustomImageWriter;
    n:tgraphnode;
    outnode,context:tjsonnode;
begin
  //generate 3d coordiantes
  n:=tway(ways.Items[0]).Nodes[0];
  for i:=0 to ways.Count-1 do begin
   w:=tway(ways.Items[i]);
   for j:=0 to w.Count-1 do begin
    w.Nodes[j].project(plane,base,wgs84);
    //Writeln('distance to prev. node:'+floattostr(distance3d(n.p3d,w.Nodes[j].p3d))+baseunit);
    n:=w.Nodes[j];
   end;
  end;

  //build chunkmap
  mp:=tmappainter.create(round(subchunksize*(1/pixelwidth)));

  //render
  for i:=0 to ways.Count-1 do begin
   w:=tway(ways.items[i]);
   w.projecttoplane(map_borders[0],map_borders[3],pixelwidth*scalefactor); //maybe switch borders in case of rotated map
   mp.paintWay(w);
  end;

  //save chunks to folder
  outnode:=mp.saveToFolder(path,prefix);
  context:=outnode.Add('context');
  context.Force('WGS84/semi-major-axis').AsString:=jconfig.Find('WGS84/semi-major-axis').AsString;
  context.Force('WGS84/semi-minor-axis').AsString:=jconfig.Find('WGS84/semi-minor-axis').AsString;
  context.Force('WGS84/baseunit').AsString:=jconfig.Find('WGS84/baseunit').AsString;
  context.Force('plane/origin/WGS84/lat').AsString:=jconfig.Find('plane/origin/WGS84/lat').AsString;
  context.Force('plane/origin/WGS84/lon').AsString:=jconfig.Find('plane/origin/WGS84/lon').AsString;
  context.Force('plane/edge/WGS84/north_lat').AsString:=jconfig.Find('plane/edge/WGS84/north_lat').AsString;
  context.Force('plane/edge/WGS84/south_lat').AsString:=jconfig.Find('plane/edge/WGS84/south_lat').AsString;
  context.Force('plane/edge/WGS84/west_lon').AsString:=jconfig.Find('plane/edge/WGS84/west_lon').AsString;
  context.Force('plane/edge/WGS84/east_lon').AsString:=jconfig.Find('plane/edge/WGS84/east_lon').AsString;
  context.Force('output/pixelwidth').AsString:=jconfig.Find('output/pixelwidth').AsString;
  context.Force('output/subchunksize').AsString:=jconfig.Find('output/subchunksize').AsString;
  outnode.SaveToFile(path+'subchunk_index.json');
end;

procedure tGraph.loadConfigFromJSON(path: string);
var res:tjsonnode;
    north,west,south,east:double;
begin
    jconfig:=tjsonnode.Create;
    jconfig.LoadFromFile(path);
    DefaultFormatSettings.DecimalSeparator := '.';    //change to decimal point
    if jconfig.Find('WGS84/unit',res) then begin
       baseunit:=res.AsString;
       Writeln('# Baseunit: '+baseunit);
       if baseunit='km' then scalefactor:=100;
       if baseunit='m' then scalefactor:=1;
    end
    else begin
     baseunit:='m';
     scalefactor:=1;
    end;
    if jconfig.Find('WGS84/semi-major-axis',res) then begin
       wgs84.semi_major_axis:=strtofloat(res.AsString);
       Writeln('# Loaded semi-major-axis: '+floattostr(wgs84.semi_major_axis)+baseunit);
    end;
    if jconfig.Find('WGS84/semi-minor-axis',res) then begin
       wgs84.semi_minor_axis:=strtofloat(res.AsString);
       Writeln('# Loaded semi-minor-axis: '+floattostr(wgs84.semi_minor_axis)+baseunit);
    end;
    wgs84.eccentricity_square:=(power(wgs84.semi_major_axis,2)-power(wgs84.semi_minor_axis,2))/(power(wgs84.semi_major_axis,2));
    Writeln('# Computed eccentricity_square: '+floattostr(wgs84.eccentricity_square));
    if jconfig.Find('plane/origin/WGS84/lat',res) then begin
       origin.lat:=strtofloat(res.AsString);
       if jconfig.Find('plane/origin/WGS84/lon',res) then begin
          origin.lon:=strtofloat(res.AsString);
          Writeln('# Loaded origin: lat: '+floattostr(origin.lat)+'° lon: '+floattostr(origin.lon)+'°');
          plane:=genplane(gpsto3d(origin,wgs84));
          base:=invertPoint(gpsto3d(origin,wgs84));
          Writeln('# The Plane Origin is at ('+floattostr(gpsto3d(origin,wgs84).x)+'|'+floattostr(gpsto3d(origin,wgs84).y)+'|'+floattostr(gpsto3d(origin,wgs84).z)+')');
          Writeln('# The Plane is (x: '+floattostr(plane.normal.x)+'|y: '+floattostr(plane.normal.y)+'|z: '+floattostr(plane.normal.z)+')');
          Writeln('# The Projection Base is at ('+floattostr(base.x)+'|'+floattostr(base.y)+'|'+floattostr(base.z)+')');
          Writeln('# The distance between Origin and Base is '+floattostr(distance3d(base,gpsto3d(origin,wgs84)))+baseunit);
       end;
    end;
    if jconfig.Find('output/pixelwidth',res) then begin
       pixelwidth:=strtofloat(res.AsString);
       Writeln('# Set pixelwidth to '+floattostr(pixelwidth)+' m');
    end;
    if jconfig.Find('plane/edge/WGS84/north_lat',res) then north:=strtofloat(res.AsString);
    if jconfig.Find('plane/edge/WGS84/south_lat',res) then south:=strtofloat(res.AsString);
    if jconfig.Find('plane/edge/WGS84/west_lon',res) then west:=strtofloat(res.AsString);
    if jconfig.Find('plane/edge/WGS84/east_lon',res) then east:=strtofloat(res.AsString);

    if jconfig.Find('output/subchunksize',res) then begin
       subchunksize:=strtofloat(res.AsString);
       Writeln('# Set subchunksize to '+floattostr(subchunksize)+baseunit+'. Therfore each chunk has a height and width of '+inttostr(round(subchunksize*(1/pixelwidth)))+' pixel.');
    end
    else begin
       subchunksize:=10.0;
       Writeln('# No subchunksize was found. Using default size of '+floattostr(subchunksize)+baseunit+'. Therfore each chunk has a height and width of '+inttostr(round(subchunksize*(1/pixelwidth)))+' pixel.');
    end;

    compute_edge(north,west,east,south);
    configured:=true;
end;

function tGraph.translateSingleCoordiante(gps: tWGS84): t2d;
var _3d:t3d;
    _2d:t2d;
begin
  //_3d:=gpsto3d(gps,self.wgs84);
  //_3d:=get3dOnPlane(getRay(
end;

end.

