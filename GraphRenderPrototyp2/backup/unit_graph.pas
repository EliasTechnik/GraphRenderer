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

type twgs84params=record
     eccentricity_square:double;
     semi_major_axis:double;
     semi_minor_axis:double;
end;

type tSphere=record
     radius:double;
     polarangle:double;
     azimuthalangle:double;
end;

type t2d=record
     x:qword;
     y:qword;
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

type tTangentPlane=record
     normal:t3d;     //vector (in our case the point P)
     argument:double; //c (or radius^2)
end;

//function gpstosphere(gps:twgs84; r:double):tsphere;  //converts WGS84 to Spherical
function gpsto3d(gps:twgs84;params:twgs84params):t3d;  //converts WGS84 to sphere based on the elipsoid
//function sphereTo3d(p:tsphere):t3d;                                 //converts Spherical to 3d Carthesian
//function gpsTo3d(gps:twgs84; r:double):t3d;          //converts WGS84 to 3d Carthesian
function get3dOnPlane(ray:tray; plane:ttangentplane):t3d;                     //projects ray on plane and returns Intersection with plane
function genPlane(p:t3d):tTangentPlane;                                       //generates Tangential Plane at Point p (It assumes that the Ball has its center at (0,0,0)
function invertPoint(p:tsphere):tsphere;                            //invertiert p
function invertPoint(p:t3d):t3d;
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
    i2d:t2d; //set externaly
    iid:longint;
    iprojected:boolean;
    function get_2d: t2d;
    function get_3d: t3d;
    function get_gps: twgs84;
    function get_id: longint;
    function get_projected: boolean;
    procedure set_2d(AValue: t2d);
    procedure set_3d(AValue: t3d);
    procedure set_gps(AValue: twgs84);
  public
    constructor create(_id:longint;_gps:tWGS84);
    property gps:twgs84 read get_gps write set_gps;
    property p3d:t3d read get_3d write set_3d;
    property p2d:t2d read get_2d write set_2d;
    property projected:boolean read get_projected;
    property ID:longint read get_id;
    procedure project(plane:ttangentplane;base:t3d;params:twgs84params);
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
    icost:byte;
    iparent:pointer;
    function getWidth: double;
    function get_cost: byte;
    function get_Node(index:integer): tGraphNode;
    function get_parent: pointer;
    procedure setWidth(AValue: double);
    procedure set_cost(AValue: byte);
  public
    constructor create(_id:longint;_parent:pointer);
    procedure add_node(_node:tGraphNode);
    procedure projecttoplane(ux:tray; uy:tray; pixelwidth:double);
    property Nodes[index:integer]:tGraphNode read get_Node;
    property pWidth:double read getWidth write setWidth;
    property pParent:pointer read get_parent;
    property pCost:byte read get_cost write set_cost;
    function Count:integer;
end;

implementation

{
function gpstosphere(gps: twgs84; r: double): tsphere;
begin
  result.azimuthalangle:=gps.lon;
  result.polarangle:=90-gps.lat;
  result.radius:=r;
  //Writeln('gpsToSphere: Azimuth: '+floattostr(result.azimuthalangle)
  //+'° Polar: '+floattostr(result.polarangle)+'° r: '+floattostr(result.radius));
end;
}
function gpsto3d(gps: twgs84; params: twgs84params): t3d;
var n:double;
begin
  n:=(params.semi_major_axis)/(sqrt((1-(params.eccentricity_square*power(sin(gps.lat),2)))));
  result.x:=n*cos(degtorad(gps.lat))*cos(degtorad(gps.lon));
  result.y:=n*cos(degtorad(gps.lat))*sin(degtorad(gps.lon));
  result.z:=(n*(1-params.eccentricity_square))*sin(degtorad(gps.lat));
end;
{
function sphereTo3d(p: tsphere): t3d;
begin
  //Writeln('sphereTo3d: Polar: '+floattostr(p.polarangle)
  //+'° Azimuth: '+floattostr(p.azimuthalangle)+'° r: '+floattostr(p.radius));
  result.x:=p.radius*cos(p.azimuthalangle)*sin(p.polarangle);
  result.y:=p.radius*sin(p.azimuthalangle)*sin(p.polarangle);
  result.z:=p.radius*cos(p.polarangle);
  //result.x:=p.radius*cos(p.azimuthalangle)*cos(p.polarangle);
  //result.y:=p.radius*cos(p.azimuthalangle)*sin(p.polarangle);
  //result.z:=p.radius*sin(p.azimuthalangle);
  //Writeln('sphereTo3d: ('+floattostr(result.x)+'|'+floattostr(result.y)+'|'+floattostr(result.z)+')');
end;
}
{
function gpsTo3d(gps: twgs84; r: double): t3d;
begin
  result:=sphereto3d(gpstosphere(gps,r));
end;
}

function get3dOnPlane(ray: tray; plane: ttangentplane): t3d;
var t:double;
begin
 //calculate t
 t:=(((plane.normal.x*ray.origin.x)*-1)
 -(plane.normal.y*ray.origin.y)
 -(plane.normal.z*ray.origin.z)+plane.argument)
 /
 ((plane.normal.x*ray.direction.x)
 +(plane.normal.y*ray.direction.y)
 +(plane.normal.z*ray.direction.z));
 //get point on plane
 result.x:=ray.origin.x+(t*ray.direction.x);
 result.y:=ray.origin.y+(t*ray.direction.y);
 result.z:=ray.origin.z+(t*ray.direction.z);
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

function invertPoint(p: t3d): t3d;
begin
  result.x:=p.x*(-1);
  result.y:=p.y*(-1);
  result.z:=p.z*(-1);
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

function tWay.get_parent: pointer;
begin
  result:=iparent;
end;

function tWay.getWidth: double;
begin
  result:=iwidth;
end;

function tWay.get_cost: byte;
begin
  result:=icost;
end;

procedure tWay.setWidth(AValue: double);
begin
  iwidth:=AValue;
end;

procedure tWay.set_cost(AValue: byte);
begin
  icost:=AValue;
end;

constructor tWay.create(_id: longint; _parent: pointer);
begin
  iid:=_id;
  inodes:=tlist.Create;
  iparent:=_parent;
end;

procedure tWay.add_node(_node: tGraphNode);
begin
  inodes.Add(_node);
end;

procedure tWay.projecttoplane(ux:tray; uy:tray; pixelwidth:double);
var i:integer;
    p:t2d;
begin
//get array of points (tpoint)
  for i:=0 to inodes.count-1 do begin
    p.x:=round(distanceToRay(tgraphnode(inodes.Items[i]).p3d,uy)*(1/pixelwidth));
    p.y:=round(distanceToRay(tgraphnode(inodes.Items[i]).p3d,ux)*(1/pixelwidth));
    tgraphnode(inodes.items[i]).p2d:=p;
    Writeln('Way '+inttostr(iid)+' has point at x: '+inttostr(p.x)+' y: '+inttostr(p.y));
  end;
end;

function tWay.Count: integer;
begin
  result:=inodes.Count;
end;


{ tGraphNode }


function tGraphNode.get_3d: t3d;
begin
 if iprojected then result:=i3d;
end;

function tGraphNode.get_2d: t2d;
begin
  result:=i2d;
end;

function tGraphNode.get_id: longint;
begin
 result:=iid;
end;

function tGraphNode.get_projected: boolean;
begin
 result:=iprojected;
end;

procedure tGraphNode.set_2d(AValue: t2d);
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


function tGraphNode.get_gps: twgs84;
begin
  result:=igps;
end;

procedure tGraphNode.project(plane: ttangentplane; base: t3d;
  params: twgs84params);
var ray:tray;
begin
 //construct ray
 ray:=getRay(base,gpsto3d(igps,params));
 //get point on plane
 i3d:=get3dOnPlane(ray,plane);
 iprojected:=true;
 writeln('Projected Node '+inttostr(iid)+' to ( '+
 floattostr(i3d.x)+','+floattostr(i3d.y)+','+floattostr(i3d.z)+' )');
end;

end.

