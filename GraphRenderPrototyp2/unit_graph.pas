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

type t3D=record
     x:double;
     y:double;
     z:double;
end;

type t2d=record
     x:qword;
     y:qword;
end;

type tRay=record
     origin:t3D;
     direction:t3D;
end;

type tTangentPlane=record
     normal:t3d;     //vector (in our case the point P)
     argument:double; //c (or radius^2)
end;

const earth_radius: double = 6378137;     //in m
      earth_vert_axis: double = 6356752.314245; // in m
      erth_inverse_flattening: double = 298.257223563; // 1/f


function gpstosphere(gps:twgs84; r:double):tsphere;  //converts WGS84 to Spherical
function sphereTo3d(p:tsphere):t3d;                                 //converts Spherical to 3d Carthesian
function gpsTo3d(gps:twgs84; r:double):t3d;          //converts WGS84 to 3d Carthesian
function get3dOnPlane(ray:tray; plane:ttangentplane):t3d;                     //projects ray on plane and returns Intersection with plane
function genPlane(p:t3d):tTangentPlane;                                       //generates Tangential Plane at Point p (It assumes that the Ball has its center at (0,0,0)
function invertPoint(p:tsphere):tsphere;                            //invertiert p
function getRay(a:t3d;b:t3d):tray;
function distance3D(a:t3d;b:t3d):double;             //returns Distance between 2 Points
function angleBetweenPoints(a:t3d;b:t3d;base:t3d):double;      //calculates smallest Angle between two Rays
function distanceToRay(p:t3d;r:tray):double;                   //returns minimal distance to ray

type

{ tGraphNode }

 tGraphNode=class
  private
    igps:tWGS84;
    i3d:t3d;
    iid:longint;
    iprojected:boolean;
    function get_3d: t3d;
    function get_gps: twgs84;
    function get_id: longint;
    function get_projected: boolean;
    procedure set_3d(AValue: t3d);
    procedure set_gps(AValue: twgs84);
  public
    constructor create(_id:longint;_gps:tWGS84);
    property gps:twgs84 read get_gps write set_gps;
    property p3d:t3d read get_3d write set_3d;
    property projected:boolean read get_projected;
    property ID:longint read get_id;
    procedure project(plane:ttangentplane;base:t3d;r:double);
end;

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
    procedure compute_edge(n,w,e,s:double);
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
    iid:longint;
    inodes:tlist;
    isurface:tsurface;
    iwidth:double;
    imaxspeed:double;
    function getWidth: double;
    function get_Node(index:integer): tGraphNode;
    procedure setWidth(AValue: double);
  public
    constructor create(_id:longint);
    procedure add_node(_node:tGraphNode);
    function paint(c:TFPCustomCanvas; ux:tray; uy:tray; pixelwidth:double):TFPCustomCanvas;
    property Nodes[index:integer]:tGraphNode read get_Node;
    property pWidth:double read getWidth write setWidth;
    function Count:integer;
end;

implementation

function gpstosphere(gps: twgs84; r: double): tsphere;
begin
  result.azimuthalangle:=gps.lon;
  result.polarangle:=90-gps.lat;
  result.radius:=r;
  //Writeln('gpsToSphere: Azimuth: '+floattostr(result.azimuthalangle)
  //+'° Polar: '+floattostr(result.polarangle)+'° r: '+floattostr(result.radius));
end;

function sphereTo3d(p: tsphere): t3d;
begin
  //Writeln('sphereTo3d: Polar: '+floattostr(p.polarangle)
  //+'° Azimuth: '+floattostr(p.azimuthalangle)+'° r: '+floattostr(p.radius));
  result.x:=p.radius*cos(p.azimuthalangle)*sin(p.polarangle);
  result.y:=p.radius*sin(p.azimuthalangle)*sin(p.polarangle);
  result.z:=p.radius*cos(p.polarangle);
  //Writeln('sphereTo3d: ('+floattostr(result.x)+'|'+floattostr(result.y)+'|'+floattostr(result.z)+')');
end;

function gpsTo3d(gps: twgs84; r: double): t3d;
begin
  result:=sphereto3d(gpstosphere(gps,r));
end;

function get3dOnPlane(ray: tray; plane: ttangentplane): t3d;
var r:double;
begin
 //calculate r
 r:=(((plane.normal.x*ray.origin.x)*-1)
 -(plane.normal.y*ray.origin.y)
 -(plane.normal.z*ray.origin.z)+plane.argument)
 /
 ((plane.normal.x*ray.direction.x)
 +(plane.normal.y*ray.direction.y)
 +(plane.normal.z*ray.direction.z));
 //get point on plane
 result.x:=ray.origin.x+(r*ray.direction.x);
 result.y:=ray.origin.y+(r*ray.direction.y);
 result.z:=ray.origin.z+(r*ray.direction.z);
end;

function genPlane(p: t3d): tTangentPlane;
var center:t3d;
begin
 center.x:=0;
 center.y:=0;
 center.z:=0;
 result.normal.x:=p.x;
 result.normal.y:=p.y;
 result.normal.z:=p.z;
 result.argument:=power(distance3d(center,p),2);
 //result.x:=power((p.x*(-1)),2)+p.x;
 //result.y:=power((p.y*(-1)),2)+p.y;
 //result.z:=power((p.z*(-1)),2)+p.z;
end;

function invertPoint(p: tsphere): tsphere;
begin
 result.polarangle:=180-p.polarangle;
 result.azimuthalangle:=p.azimuthalangle+180;
 result.radius:=p.radius;
 //Writeln('InvertPoint: Azimuth: '+floattostr(result.azimuthalangle)
  //+'° Polar: '+floattostr(result.polarangle)+'°');
end;

function getRay(a: t3d; b: t3d): tray;
begin
  result.origin:=a;
  result.direction.x:=b.x-a.x;
  result.direction.y:=b.y-a.y;
  result.direction.z:=b.z-a.z;
end;

function distance3D(a: t3d; b: t3d): double;
begin
  result:=sqrt(
      (power((b.x-a.x),2)
      +power((b.y-a.y),2)
      +power((b.z-a.z),2)));
end;

function angleBetweenPoints(a: t3d; b: t3d; base: t3d): double;
var vec1,vec2:t3d;
begin
  //Caclulate vec1, vec2
  vec1.x:=a.x-base.x;
  vec1.y:=a.y-base.y;
  vec1.z:=a.z-base.z;

  vec2.x:=b.x-base.x;
  vec2.y:=b.y-base.y;
  vec2.z:=b.z-base.z;

  result:=arccos(((vec1.x*vec2.x)+(vec1.y*vec2.y)+(vec1.z*vec2.z))
  /
  (sqrt(power(vec1.x,2)+power(vec1.y,2)+power(vec1.z,2))
  *sqrt(power(vec2.x,2)+power(vec2.y,2)+power(vec2.z,2))));
end;

function distanceToRay(p: t3d; r: tray): double;
var D1,D2,D3:double;
begin
  D1:=((p.y-r.origin.y)*r.direction.z)-((p.z-r.origin.z)*r.direction.y);
  D2:=((p.z-r.origin.z)*r.direction.x)-((p.x-r.origin.x)*r.direction.z);
  D3:=((p.x-r.origin.x)*r.direction.y)-((p.y-r.origin.y)*r.direction.x);
  result:=sqrt((power(D1,2)+power(D2,2)+power(D3,2)))/sqrt((power(r.direction.x,2)+power(r.direction.y,2)+power(r.direction.z,2)));
end;

{ tWay }

function tWay.get_Node(index: integer): tGraphNode;
begin
  result:=tGraphNode(inodes.Items[index]);
end;

function tWay.getWidth: double;
begin
  result:=iwidth;
end;

procedure tWay.setWidth(AValue: double);
begin
  iwidth:=AValue;
end;

constructor tWay.create(_id: longint);
begin
  iid:=_id;
  inodes:=tlist.Create;
end;

procedure tWay.add_node(_node: tGraphNode);
begin
  inodes.Add(_node);
end;

function tWay.paint(c: TFPCustomCanvas; ux:tray; uy:tray; pixelwidth:double): TFPCustomCanvas;
var points:array of tpoint;
    x:integer;
    y:integer;
    i:integer;
begin
//get array of points (tpoint)
  setlength(points,inodes.Count);
  for i:=0 to inodes.count-1 do begin
    x:=round(distanceToRay(tgraphnode(inodes.Items[i]).get_3d,uy)*(1/pixelwidth));
    y:=round(distanceToRay(tgraphnode(inodes.Items[i]).get_3d,ux)*(1/pixelwidth));
    points[i]:=tpoint.Create(x,y);
    Writeln('Way '+inttostr(iid)+' has point at x: '+inttostr(points[i].X)+' y: '+inttostr(points[i].Y));
  end;
  //setup pen
   with c do
    begin
      pen.mode    := pmCopy;
      pen.style   := psSolid;
      pen.width   := 30; //round(iwidth*(1/pixelwidth));      //disabled becaus of bad scaling //todo set way width
      pen.FPColor := TColorToFPColor(rgbtocolor(255,255,255));  //todo: generate costs
    end;

  //draw polyline
  c.Polyline(points); //Bezier Line is also possible
  result:=c;
end;

function tWay.Count: integer;
begin
  result:=inodes.Count;
end;


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
        w:=tway.create(strtoint(way.Find('@id').AsString));
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
  n:=tway(ways.Items[0]).get_Node(0);
  for i:=0 to ways.Count-1 do begin
   w:=tway(ways.Items[i]);
   for j:=0 to w.Count-1 do begin
    w.get_Node(j).project(plane,base,radius);
    Writeln('distance to prev. node:'+floattostr(distance3d(n.get_3d,w.get_Node(j).get_3d))+baseunit);
    n:=w.get_Node(j);
   end;
  end;

  //project 3d to 2d

  //render image

  { Create an image 1000x1000 pixels}
  width:=round(distance3d(map_corners[0],map_corners[1])*(1/pixelwidth));
  height:=round(distance3d(map_corners[0],map_corners[3])*(1/pixelwidth));
  Writeln('Output canvas dimmension (w,h): '+inttostr(width)+','+inttostr(height));

  image := TFPMemoryImage.Create (width,height);   //todo: set size

  { Attach the image to the canvas }
  Canvas := TFPImageCanvas.Create (image);


  //draw points
  for i:=0 to ways.count-1 do begin
   canvas:=tway(ways.items[i]).paint(canvas,map_borders[0],map_borders[3],pixelwidth);
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

{ tGraphNode }


function tGraphNode.get_3d: t3d;
begin
 if iprojected then result:=i3d;
end;

function tGraphNode.get_id: longint;
begin
 result:=iid;
end;

function tGraphNode.get_projected: boolean;
begin
 result:=iprojected;
end;

procedure tGraphNode.set_3d(AValue: t3d);
begin
 i3d:=AValue;
end;

procedure tGraphNode.set_gps(AValue: twgs84);
begin
 igps:=AValue;
end;

constructor tGraphNode.create(_id: longint; _gps: tWGS84);
begin
  igps:=_gps;
  iid:=_id;
  iprojected:=false;
  writeln('Createded Node '+inttostr(iid)+' at ( '+
 floattostr(gps.lat)+' | '+floattostr(gps.lon)+' )');
end;


function tGraphNode.get_gps: twgs84;
begin
  result:=igps;
end;

procedure tGraphNode.project(plane: ttangentplane; base: t3d; r: double);
var ray:tray;
begin
 //construct ray
 ray:=getRay(base,gpsto3d(igps,r));
 i3d:=get3dOnPlane(ray,plane);
 iprojected:=true;
 writeln('Projected Node '+inttostr(iid)+' to ( '+
 floattostr(i3d.x)+','+floattostr(i3d.y)+','+floattostr(i3d.z)+' )');
end;

end.

