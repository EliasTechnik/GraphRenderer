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
function getRay(a:t3d;b:t3d):tray;
function distance3D(a:t3d;b:t3d):double;             //returns Distance between 2 Points
function angleBetweenPoints(a:t3d;b:t3d;base:t3d):double;      //calculates smallest Angle between two Rays

type

{ tGraphNode }

 tGraphNode=class
  private
    igps:tWGS84;
    i2d:tcarthesian;
    i3d:t3d;
    iid:longint;
    iprojected:boolean;
    function get_2d: tcarthesian;
    function get_3d: t3d;
    function get_gps: twgs84;
    function get_id: longint;
    function get_projected: boolean;
    procedure set_2d(AValue: tcarthesian);
    procedure set_3d(AValue: t3d);
    procedure set_gps(AValue: twgs84);
  public
    constructor create(_id:longint;_gps:tWGS84);
    property gps:twgs84 read get_gps write set_gps;
    property p2d:tcarthesian read get_2d write set_2d;
    property p3d:t3d read get_3d write set_3d;
    property projected:boolean read get_projected;
    property ID:longint read get_id;
    procedure project(plane:t3d;base:t3d;r:double);
end;

type

  { tGraph }

 tGraph=class
  private
    ways:tlist;
    min_edge:t3d;
    max_edge:t3d;
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
    iid:longint;
    inodes:tlist;
    isurface:tsurface;
    iwidth:double;
    imaxspeed:double;
    function get_Node(index:integer): tGraphNode;
  public
    constructor create(_id:longint);
    procedure add_node(_node:tGraphNode);
    function paint(c:TFPCustomCanvas):TFPCustomCanvas;
    property Nodes[index:integer]:tGraphNode read get_Node;
    function Count:integer;
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
  Writeln('sphereTo3d: Azimuth: '+floattostr(p.azimuthalangle)
  +'° Polar: '+floattostr(p.polarangle)+'° r: 'floattostr(p.radius));
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
 Writeln('InvertPoint: Azimuth: '+floattostr(result.azimuthalangle)
  +'° Polar: '+floattostr(result.polarangle)+'°');
end;

function getRay(a: t3d; b: t3d): tray;
begin
  result.origin:=a;
  result.direction.x:=a.x-b.x;
  result.direction.y:=a.y-b.y;
  result.direction.z:=a.z-b.z;
end;

function distance3D(a: t3d; b: t3d): double;
begin
  result:=sqrt((power((b.x-a.x),2)+power((b.y-a.y),2)+power((b.z-a.z),2)));
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

{ tWay }

function tWay.get_Node(index: integer): tGraphNode;
begin
  result:=tGraphNode(inodes.Items[index]);
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

function tWay.paint(c: TFPCustomCanvas): TFPCustomCanvas;
var points:array of tpoint;
    xy:tcarthesian;
    i:integer;
begin
//get array of points (tpoint)
  setlength(points,inodes.Count);
  for i:=0 to inodes.count-1 do begin
    xy:=tgraphnode(inodes.Items[i]).get_2d;
    //points[i]:=tpoint.Create(xy.x-offset.x,xy.y-offset.y);
    Writeln('Way '+inttostr(iid)+' has point at x: '+inttostr(points[i].X)+' y: '+inttostr(points[i].Y));
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

function tWay.Count: integer;
begin
  result:=inodes.Count;
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
            //writeln('Node '+floattostr(wgs84.lat)+' '+floattostr(wgs84.lon)+' added');
            w.add_node(tGraphNode.create(strtoint(nd.Child(nodes).Find('@id').AsString),wgs84));
        end;
        //TODO: parse tag (surface, width, maxspeed
        ways.Add(w);
     end;
  end;
end;

procedure tGraph.renderImageToFile(path: string);
var
    i,j,x,y:integer;
    w:tway;
    p:t3d;
    canvas : TFPCustomCanvas;
    image : TFPCustomImage;
    writer : TFPCustomImageWriter;
begin
  //generate 3d coordiantes
  for i:=0 to ways.Count-1 do begin
   w:=tway(ways.Items[i]);
   for j:=0 to w.Count-1 do begin
    w.get_Node(j).project(plane,base,radius);
   end;
  end;
  //gather edges
  p:=tway(ways.Items[0]).get_Node(0).get_3d;  //init p (fails if no node and way was loaded

  min_edge:=p;
  max_edge:=p;
  for i:=0 to ways.Count-1 do begin
   w:=tway(ways.Items[i]);
   for j:=0 to w.Count-1 do begin
    p:=w.get_Node(j).get_3d;
    Writeln('The p is at ('+floattostr(p.x)+'|'+floattostr(p.y)+'|'+floattostr(p.z)+')');
    if p.x<min_edge.x then min_edge.x:=p.x;
    if p.y<min_edge.y then min_edge.y:=p.y;
    if p.z<min_edge.z then min_edge.z:=p.z;

    if p.x>max_edge.x then max_edge.x:=p.x;
    if p.y>max_edge.y then max_edge.y:=p.y;
    if p.z>max_edge.z then max_edge.z:=p.z;
   end;
  end;
  Writeln('The min_edge is at ('+floattostr(min_edge.x)+'|'+floattostr(min_edge.y)+'|'+floattostr(min_edge.z)+')');
  Writeln('The max_edge is at ('+floattostr(max_edge.x)+'|'+floattostr(max_edge.y)+'|'+floattostr(max_edge.z)+')');
  writeln('Area cross-section: '+floattostr(distance3d(min_edge,max_edge))+' m');

  //project 3d to 2d

  //render image

     { Create an image 1000x1000 pixels}
  image := TFPMemoryImage.Create (100,100);   //todo: set size

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
  //image.SaveToFile ('DrawTest.png', writer);

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
    DefaultFormatSettings.DecimalSeparator := '.';    //change to decimal point
    if j.Find('WGS84/radius',res) then begin
       //Writeln(res.AsString);
       radius:=strtofloat(res.AsString);
    end
    else radius:=earth_radius;
    if j.Find('plane/origin/WGS84/lat',res) then begin
       //Writeln(res.AsString);
       origin.lat:=strtofloat(res.AsString);
       if j.Find('plane/origin/WGS84/lon',res) then begin
          //Writeln(res.AsString);
          origin.lon:=strtofloat(res.AsString);
          plane:=genplane(gpsto3d(origin,radius));
          base:=sphereto3d(invertPoint(gpstosphere(origin,radius)));
          Writeln('The Projection Base is at ('+floattostr(base.x)+'|'+floattostr(base.y)+'|'+floattostr(base.z)+')');
       end;
    end;
end;

{ tGraphNode }

function tGraphNode.get_2d: tcarthesian;
begin
  if iprojected then result:=i2d;
end;

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

procedure tGraphNode.set_2d(AValue: tcarthesian);
begin
 i2d:=AValue;
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


function tGraphNode.get_gps: tWGS84;
begin
  result:=igps;
end;

procedure tGraphNode.project(plane: t3d; base: t3d; r: double);
var ray:tray;
begin
 //construct ray
 ray:=getRay(base,gpsto3d(igps,r));
 i3d:=get3dOnPlane(ray,plane);
 iprojected:=true;
 writeln('Projected Node '+inttostr(iid)+' to ( '+
 floattostr(i3d.x)+' | '+floattostr(i3d.y)+' | '+floattostr(i3d.z)+' )');
end;

end.

