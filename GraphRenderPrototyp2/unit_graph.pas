unit unit_graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsontools, FPImage, FPCanvas, FPImgCanv,
     FPWritePNG, graphics, math;

type tsurface=(sASPHALT,sPAVED,sUNKNOWN);



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
     x:double;
     y:double;
     scale_factor:double;
end;

type t3D=record
     x:double;
     y:double;
     z:double;
end;

type tRay=record
     origin:t3D;
     direction:t3D;
end;

const earth_radius: double = 6378137;     //in m
      earth_vert_axis: double = 6356752.314245; // in m
      erth_inverse_flattening: double = 298.257223563; // 1/f


function gpstosphere(gps:twgs84; r:double):tsphere;  //converts WGS84 to Spherical
function sphereTo3d(p:tsphere):t3d;                                 //converts Spherical to 3d Carthesian
function gpsTo3d(gps:twgs84; r:double):t3d;          //converts WGS84 to 3d Carthesian
function get3dOnPlane(ray:tray; plane:t3D):t3d;                     //projects ray on plane and returns Intersection with plane
function genPlane(p:t3d):t3d;                                       //generates Tangential Plane at Point p (It assumes that the Ball has its center at (0,0,0)
function invertPoint(p:tsphere):tsphere;                            //invertiert p


type

{ tGraphNode }

 tGraphNode=class
  private
    gps:tWGS84;
    pointZero:tWGS84;
    xy:tcarthesian;
    id:longint;
  public
    constructor create(_id:longint;_gps:tWGS84);
    function get_carthesian:tcarthesian;
    function get_gps:tWGS84;
    procedure convert_to_carthesian;
    procedure set_pointzero(_pointZero:twgs84);
end;

type

  { tGraph }

 tGraph=class
  private
    ways:tlist;
    upper_left_offset:tcarthesian;
    lower_right_offset:tcarthesian;
    origin:twgs84;
    base:t3d;
    plane:t3d;
    radius:double;
  public
    constructor create();
    procedure loadFromJSONFile(path:string);
    procedure renderImageToFile(path:string);
    procedure loadConfigFromJSON(path:string);
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
    offset:tcarthesian;
  public
    constructor create(_id:longint);
    procedure add_node(_node:tGraphNode);
    function paint(c:TFPCustomCanvas):TFPCustomCanvas;
    function get_minGPS:tWGS84;
    function get_maxGPS:tWGS84;
    procedure set_pointZero(_pz:twgs84);
    procedure set_offset(o:tcarthesian);
end;

implementation

function gpstosphere(gps: twgs84; r: double): tsphere;
begin
  result.azimuthalangle:=gps.lon;
  result.polarangle:=90-gps.lat;
  result.radius:=r;
  Writeln('gpsToSphere: Azimuth: '+floattostr(result.azimuthalangle)
  +'° Polar: '+floattostr(result.polarangle)+'° r: '+floattostr(result.radius));
end;

function sphereTo3d(p: tsphere): t3d;
begin
  result.x:=p.radius*sin(p.polarangle)*cos(p.azimuthalangle);
  result.y:=p.radius*sin(p.polarangle)*sin(p.azimuthalangle);
  result.z:=p.radius*cos(p.polarangle);
  Writeln('sphereTo3d: ('+floattostr(result.x)+'|'+floattostr(result.y)+'|'+floattostr(result.z)+')');
end;

function gpsTo3d(gps: twgs84; r: double): t3d;
begin
  result:=sphereto3d(gpstosphere(gps,r));
end;

function get3dOnPlane(ray: tray; plane: t3D): t3d;
var r:double;
begin
 //calculate r
 r:=(((plane.x*ray.origin.x)*-1)
 -(plane.y*ray.origin.y)
 -(plane.z*ray.origin.z))
 /
 ((plane.x*ray.direction.x)
 +(plane.y*ray.direction.x)
 +(plane.z*ray.direction.z));
 //get point on plane
 result.x:=ray.origin.x+(r*ray.direction.x);
 result.y:=ray.origin.y+(r*ray.direction.y);
 result.z:=ray.origin.z+(r*ray.direction.z);
end;

function genPlane(p: t3d): t3d;
begin
 result.x:=power((p.x*-1),2)+p.x;
 result.y:=power((p.y*-1),2)+p.y;
 result.z:=power((p.z*-1),2)+p.z;
end;

function invertPoint(p: tsphere): tsphere;
begin
 result.polarangle:=180-p.polarangle;
 result.azimuthalangle:=p.azimuthalangle+180;
 result.radius:=p.radius;
end;

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
    //points[i]:=tpoint.Create(xy.x-offset.x,xy.y-offset.y);
    Writeln('Way '+inttostr(id)+' has point at x: '+inttostr(points[i].X)+' y: '+inttostr(points[i].Y));
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

procedure tWay.set_offset(o: tcarthesian);
begin
  offset:=o;
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
    ul,lr:tGraphnode;
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

      wgs84:=tway(ways.items[wi]).get_maxGPS;
      //writeln(floattostr(wgs84.lon));
      if wgs84.lon > pz_max.lon then pz_max.lon:=wgs84.lon;
      if wgs84.lat > pz_max.lat then pz_max.lat:=wgs84.lat;
     end;
     pz.lon:=pz_min.lon+(pz_max.lon-pz_min.lon)/2;
     pz.lat:=pz_min.lat+(pz_max.lat-pz_min.lat)/2;
     writeln('PointZero has lat: '+floattostr(pz.lat)+' and lon: '+floattostr(pz.lon));
     ul:=tgraphnode.create(0,pz_min);
     lr:=tgraphnode.create(0,pz_max);
     ul.set_pointzero(pz);
     lr.set_pointzero(pz);
     upper_left_offset:=ul.get_carthesian;
     lower_right_offset:=lr.get_carthesian;
     //writeln('Upper Left Corner: '+inttostr(upper_left_offset.x)+'|'+inttostr(upper_left_offset.y));
     //writeln('Lower Right Corner: '+inttostr(lower_right_offset.x)+'|'+inttostr(lower_right_offset.y));
     for wi:=0 to ways.count-1 do begin
      tway(ways.items[wi]).set_pointZero(pz);
      tway(ways.items[wi]).set_offset(upper_left_offset);
     end;
  end;

end;

procedure tGraph.renderImageToFile(path: string);
var
    i,x,y:integer;
    canvas : TFPCustomCanvas;
    image : TFPCustomImage;
    writer : TFPCustomImageWriter;
begin
 // x:=lower_right_offset.x-upper_left_offset.x;
 // y:=lower_right_offset.y-upper_left_offset.y;

     { Create an image 1000x1000 pixels}
  image := TFPMemoryImage.Create (x,y);   //todo: set size

  { Attach the image to the canvas }
  Canvas := TFPImageCanvas.Create (image);


  //draw points
  for i:=0 to ways.count-1 do begin
   canvas:=tway(ways.items[i]).paint(canvas);
  end;

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

procedure tGraph.loadConfigFromJSON(path: string);
var j,res:tjsonnode;

begin
    j:=tjsonnode.Create;
    j.LoadFromFile(path);
    if j.Find('WGS84/radius',res) then begin
       Writeln(res.AsString);
       radius:=strtofloat(res.AsString);
    end
    else radius:=earth_radius;
    if j.Find('plane/origin/WGS84/lat',res) then begin
       Writeln(res.AsString);
       origin.lat:=strtofloat(res.AsString);
       if j.Find('plane/origin/WGS84/lon',res) then begin
          Writeln(res.AsString);
          origin.lon:=strtofloat(res.AsString);
          plane:=genplane(gpsto3d(origin,radius));
          base:=sphereto3d(invertPoint(gpstosphere(origin,radius)));
          Writeln('The Projection Base is at ('+floattostr(base.x)+'|'+floattostr(base.y)+'|'+floattostr(base.z)+')');
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
  result:=xy;
end;

function tGraphNode.get_gps: tWGS84;
begin
  result:=gps;
end;

procedure tGraphNode.convert_to_carthesian;

begin




end;

procedure tGraphNode.set_pointzero(_pointZero: twgs84);
begin
  pointZero:=_pointZero;
end;

end.

