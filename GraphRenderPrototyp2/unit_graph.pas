unit unit_graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsontools, FPImage, FPCanvas, FPImgCanv,
     FPWritePNG, graphics, math;

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
    function get_gps:tWGS84;
    procedure set_pointzero(_pointZero:twgs84);
end;

type

  { tGraph }

 tGraph=class
  private
    ways:tlist;
    //bitmap:tbitmap;
    //png:TPortableNetworkGraphic;
  public
    constructor create();
    procedure loadFromJSONFile(path:string);
    procedure renderImageToFile(path:string);
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
    function paint(c:TFPCustomCanvas):TFPCustomCanvas;
    function get_minGPS:tWGS84;
    function get_maxGPS:tWGS84;
    procedure set_pointZero(_pz:twgs84);
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

function tWay.paint(c: TFPCustomCanvas): TFPCustomCanvas;
var points:array of tpoint;
    xy:tcarthesian;
    i:integer;
begin
//get array of points (tpoint)
  setlength(points,nodes.Count);
  for i:=0 to nodes.count-1 do begin
    xy:=tgraphnode(nodes.Items[i]).get_carthesian;
    points[i]:=tpoint.Create(xy.x,xy.y);
  end;
  //setup pen
   with c do
    begin
      pen.mode    := pmCopy;
      pen.style   := psSolid;
      pen.width   := 10;      //todo set way width
      pen.FPColor := TColorToFPColor(rgbtocolor(255,255,255));  //todo: generate costs
    end;

  //draw polyline
  c.Polyline(points); //Bezier Line is also possible
  result:=c;
end;

function tWay.get_minGPS: tWGS84;
var i:integer;
    min_gps,gps:twgs84;
begin
  //gen PointZero
  min_gps:=tgraphnode(nodes.Items[0]).get_gps;
  for i:=0 to nodes.count-1 do begin
    gps:=tgraphnode(nodes.Items[i]).get_gps;
    if gps.lon < min_gps.lon then min_gps.lon:=gps.lon;
    if gps.lat < min_gps.lat then min_gps.lat:=gps.lat;
  end;
  result:=min_gps;
end;

function tWay.get_maxGPS: tWGS84;
var i:integer;
    max_gps,gps:twgs84;
begin
  //gen PointZero
  max_gps:=tgraphnode(nodes.Items[0]).get_gps;
  for i:=0 to nodes.count-1 do begin
    gps:=tgraphnode(nodes.Items[i]).get_gps;
    if gps.lon > max_gps.lon then max_gps.lon:=gps.lon;
    if gps.lat > max_gps.lat then max_gps.lat:=gps.lat;
  end;
  result:=max_gps;
end;

procedure tWay.set_pointZero(_pz: twgs84);
var i:integer;
begin
  for i:=0 to nodes.count-1 do begin
    tgraphnode(nodes.items[i]).set_pointzero(_pz);
  end;
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
    pz_min,pz_max,pz:twgs84;
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
     //gen PointZero
     pz_min:=tway(ways.items[0]).get_minGPS;
     pz_max:=tway(ways.items[0]).get_maxGPS;
     for wi:=0 to ways.count-1 do begin
      wgs84:=tway(ways.items[wi]).get_minGPS;
      if wgs84.lon < pz_min.lon then pz_min.lon:=wgs84.lon;
      if wgs84.lat < pz_min.lat then pz_min.lat:=wgs84.lat;

      wgs84:=tway(ways.items[wi]).get_maxGPS;;
      if wgs84.lon > pz_max.lon then pz_max.lon:=wgs84.lon;
      if wgs84.lat > pz_max.lat then pz_max.lon:=wgs84.lat;
     end;
     pz.lon:=pz_min.lon+(pz_max.lon-pz_min.lon)/2;
     pz.lat:=pz_min.lat+(pz_max.lat-pz_min.lat)/2;
     for wi:=0 to ways.count-1 do begin
      tway(ways.items[wi]).set_pointZero(pz);
     end;
  end;

end;

procedure tGraph.renderImageToFile(path: string);
var
    i,x1,y1:integer;
    canvas : TFPCustomCanvas;
    image : TFPCustomImage;
    writer : TFPCustomImageWriter;
begin


     { Create an image 1000x1000 pixels}
  image := TFPMemoryImage.Create (1000,1000);   //todo: set size

  { Attach the image to the canvas }
  Canvas := TFPImageCanvas.Create (image);

  {
  with canvas do
  begin
    pen.mode    := pmCopy;
    pen.style   := psSolid;
    pen.width   := 1;
    pen.FPColor := TColorToFPColor(rgbtocolor(255,255,255));
  end;
  //draw random points
   randomize();
   //c.Brush.Color:=rgbtocolor(255,255,255);
   for i:=0 to 10 do begin
      x1:=round((canvas.Width-10)*random());
      y1:=round((canvas.Height-10)*random());
      canvas.Ellipse(x1,y1,x1+10,y1+10);
      canvas.li
   end;

   }
  { Create the writer }
  Writer := TFPWriterPNG.Create;

  { Save to file }
  image.SaveToFile ('DrawTest.png', writer);

  { Clean up! }
  Canvas.Free;
  image.Free;
  writer.Free;
end;

{ tGraphNode }

constructor tGraphNode.create(_id: longint; _gps: tWGS84);
begin
  gps:=_gps;
  id:=_id;
end;

function tGraphNode.get_carthesian: tcarthesian;
var r_lat,r_lon:double;
begin
  //convert to radians
  r_lat:=DegToRad(gps.lat);
  r_lon:=DegToRad(gps.lon);
  //convert gps to sphere

  //convert pointZero to sphere

  //project sphere on plane

  //convert polar to carthesian

  //return carthesian
end;

function tGraphNode.get_gps: tWGS84;
begin
  result:=gps;
end;

procedure tGraphNode.set_pointzero(_pointZero: twgs84);
begin
  pointZero:=_pointZero;
end;

end.

