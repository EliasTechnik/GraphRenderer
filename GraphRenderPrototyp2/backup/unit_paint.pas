unit unit_paint;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unit_graph, graphics, BGRABitmap, BGRABitmapTypes, jsontools;


type

{ tchunk }

 tchunk=class //wrapper for the FPImage
 private
  offset:t2d;
  chunksize:integer;
  ichunkid:t2d; //2d id of that chunk
  bmp:tBGRABitmap;
  function get_chunkid: t2d;
  function get_offset: t2d;
  procedure set_chunkid(AValue: t2d);
 public
  constructor create(_chunksize:integer;_offset:t2d);
  procedure savetofile(path:string); //returns filename
  procedure paintCircle(center:t2d;radius:integer;cost:byte); //the 2d coordinates refer to the global 2d Reference. So no offsetcalculation is needed
  property chunkid:t2d read get_chunkid write set_chunkid;
  property pOffset:t2d read get_offset;
  destructor free;
end;

{ tmappainter }

 tmappainter=class
  private
   chunksize:integer; //width and heght of a chunk
   chunks:tlist; //contais objects of type tlist x[y] x=col y=row
   //chunk:=tchunk(tlist(chunks.items[x]).Items[y]);
   procedure expandMap(cx,cy:integer);
   function getChunk(x,y:integer):tchunk;
  public
   constructor create(_chunksize:integer);
   procedure paintWay(w:tway);
   procedure paintLine(p1:t2d;p2:t2d;width:integer;cost:byte);
   function saveToFolder(path:string;prefix:string):tjsonnode;
end;

implementation

uses unit_graphmanager;

{ tchunk }

function tchunk.get_chunkid: t2d;
begin
  result:=ichunkid;
end;

function tchunk.get_offset: t2d;
begin
  result:=offset;
end;

procedure tchunk.set_chunkid(AValue: t2d);
begin
  ichunkid:=Avalue;
end;

constructor tchunk.create(_chunksize: integer; _offset: t2d);
begin
  chunksize:=_chunksize;
  offset:=_offset;
  //bitmap
  bmp:=tBGRABitmap.Create(chunksize,chunksize,BGRABlack);
  bmp.Canvas.Brush.Style:=bssolid;
end;

procedure tchunk.savetofile(path: string);
begin
 bmp.SaveToFile(path);
end;

procedure tchunk.paintCircle(center: t2d; radius: integer; cost: byte);
var x,y:integer;
begin
  bmp.canvas.Brush.Color:=rgbtocolor(cost,cost,cost);
  bmp.Canvas.pen.Color:=rgbtocolor(cost,cost,cost);
  bmp.canvas.pen.Width:=1;//radius*2;
  bmp.canvas.pen.Style:=pssolid;

  //calc offset
  x:=center.x-offset.x;
  y:=center.y-offset.y;

  bmp.Canvas.Ellipse(x-radius,y-radius,x+radius,y+radius);
end;

destructor tchunk.free;
begin
  bmp.Free;
end;

{ tmappainter }

procedure tmappainter.expandMap(cx, cy: integer);
var x,y,xmax,ymax:integer;
    ycol:tlist;
    chunk:tchunk;
    offset:t2d;
    id:t2d;

begin
  xmax:=chunks.Count-1;
  ymax:=tlist(chunks[0]).count-1;

  //expand current map to new size in y direction
  for x:=0 to chunks.count-1 do begin
    ycol:=tlist(chunks.items[x]);
    offset.x:=x*chunksize;
    for y:=ymax+1 to cy do begin
      offset.y:=chunksize*y;
      chunk:=tchunk.create(chunksize,offset);
      id.x:=x;
      id.y:=y;
      chunk.chunkid:=id;
      ycol.Add(chunk);
    end;
  end;

  //add missing ycols in x direction
  for x:=xmax+1 to cx do begin
    ycol:=tlist.create;
    offset.x:=x*chunksize;
    for y:=0 to cy do begin
        offset.y:=chunksize*y;
        chunk:=tchunk.create(chunksize,offset);
        id.x:=x;
        id.y:=y;
        chunk.chunkid:=id;
        ycol.Add(chunk);
    end;
    chunks.Add(ycol);
  end;
  Writeln('Expanded the map from ('+inttostr(xmax)+','+inttostr(ymax)+') to ('+inttostr(cx)+','+inttostr(cy)+')');
end;

function tmappainter.getChunk(x, y: integer): tchunk;
var xc,yc:integer;
begin
  xc:=x div chunksize;
  yc:=y div chunksize;
  //Writeln('Get subchunk ('+inttostr(xc)+','+inttostr(yc)+')');
  //check if the target chunk is currently outside the map
  if (chunks.count-1<xc) or (tlist(chunks.items[0]).Count-1<yc) then self.expandMap(xc,yc);  //expand map to chunk
  result:=tchunk(tlist(chunks.items[xc]).items[yc]); //return chunk
end;

constructor tmappainter.create(_chunksize: integer);
var ycol:tlist;
    offset:t2d;
    subchunk:tchunk;
begin
  chunksize:=_chunksize;
  chunks:=tlist.create;
  ycol:=tlist.create;
  offset.x:=0;
  offset.y:=0;

  subchunk:=tchunk.create(chunksize,offset);
  subchunk.chunkid:=offset; //normaly the id isn't the offset but for the first chunk it is.
  ycol.Add(subchunk);
  chunks.Add(ycol);
end;

procedure tmappainter.paintWay(w: tway);
var i:integer;
    dot1,dot2:t2d;
begin
  if w.Count>1 then begin
    dot2:=w.Nodes[0].p2d;
    for i:=1 to w.Count-1 do begin
      dot1:=dot2;
      dot2:=w.Nodes[i].p2d;
      //Writeln('Try to paint line from ('+inttostr(dot1.x)+','+inttostr(dot1.y)+') to ('+inttostr(dot2.x)+','+inttostr(dot2.y)+') with width of '+inttostr(round(w.pWidth*(1/tgraph(w.pParent).pPixelwidth)))+' px');
      self.paintLine(dot1,dot2,round(w.pWidth*(1/tgraph(w.pParent).pPixelwidth)),255);   //w.pCost
    end;
  end;
end;

procedure tmappainter.paintLine(p1: t2d; p2: t2d; width: integer; cost: byte);
var m,b,delta_x,delta_y:double;
    x,y,radius:integer;
    dot,id:t2d;
    subchunk:tchunk;
begin
  radius:=round(width/2);
  //get delta x
  delta_x:=p2.x-p1.x;
  delta_x:=abs(delta_x);
  //get delta y
  delta_y:=(p2.y-p1.y);
  delta_y:=abs(delta_y);

  if (delta_y<>0) and (delta_x<>0) then begin
    if delta_x>=delta_y then begin
      if p1.x>p2.x then begin
        //swap points
        dot:=p1;
        p1:=p2;
        p2:=dot;
      end;
      //writeln('P1: ('+inttostr(p1.x)+','+inttostr(p1.y )+') P2: ('+inttostr(p2.x)+','+inttostr(p2.y )+')');
      //draw in x steps
      m:=(p2.y-p1.y)/(p2.x-p1.x);
      b:=((m*(-1))*p1.x)+p1.y;
      //Writeln('function: y = '+floattostrf(m,ffFixed,32,4)+' * x + '+floattostrf(b,ffFixed,32,4));
      for x:=p1.x to p2.x do begin
        y:=round(m*x+b);
        dot.x:=x;
        dot.y:=y;
        subchunk:=self.getChunk(dot.x,dot.y);
        id:=subchunk.chunkid;
        //writeln('Painting dot on ('+inttostr(dot.x)+','+inttostr(dot.y)+').');
        subchunk.paintCircle(dot,radius,cost);
      end;
    end
    else begin
      if p1.y>p2.y then begin
        //swap points
        dot:=p1;
        p1:=p2;
        p2:=dot;
      end;
      //draw in y steps
      m:=(p2.y-p1.y)/(p2.x-p1.x);
      b:=((m*(-1))*p1.x)+p1.y;
      //Writeln('function: x = ( y - '+floattostr(b)+') / '+floattostr(m));
      for y:=p1.y to p2.y do begin
        x:=round((y-b)/m);
        dot.x:=x;
        dot.y:=y;
        subchunk:=self.getChunk(dot.x,dot.y);
        id:=subchunk.chunkid;
        //writeln('Painting dot on ('+inttostr(dot.x)+','+inttostr(dot.y)+').');
        subchunk.paintCircle(dot,radius,cost);
      end;
    end;
  end
  else begin
    if (delta_x=0) and (delta_y<>0)  then begin
      //horizontal line
      if p1.y>p2.y then begin
        //swap points
        dot:=p1;
        p1:=p2;
        p2:=dot;
      end;
      dot.x:=p1.x;
      for y:=p1.y to p2.y do begin
        dot.y:=y;
        subchunk:=self.getChunk(dot.x,dot.y);
        id:=subchunk.chunkid;
        //writeln('Painting dot on ('+inttostr(dot.x)+','+inttostr(dot.y)+').');
        subchunk.paintCircle(dot,radius,cost);
      end;
    end;
    if (delta_x<>0) and (delta_y=0)  then begin
      //vertical line
      if p1.x>p2.x then begin
        //swap points
        dot:=p1;
        p1:=p2;
        p2:=dot;
      end;
      dot.y:=p1.y;
      for x:=p1.x to p2.x do begin
        dot.x:=x;
        subchunk:=self.getChunk(dot.x,dot.y);
        id:=subchunk.chunkid;
        //writeln('Painting dot on ('+inttostr(dot.x)+','+inttostr(dot.y)+').');
        subchunk.paintCircle(dot,radius,cost);
      end;
    end;

  end;
end;

function tmappainter.saveToFolder(path: string; prefix: string): tjsonnode;
var x,y:integer;
    filename:string;
    subchunk:tchunk;
    node,subchunks:tjsonnode;
    line:string;
begin
  result:=tjsonnode.Create;
  subchunks:=result.Add('subchunks');
  subchunks.Add('size',inttostr(chunksize));
  for x:=0 to chunks.count-1 do begin
    for y:=0 to tlist(chunks.items[x]).count-1 do begin
      filename:=path+prefix+inttostr(x)+'_'+inttostr(y)+'.png';
      subchunk:=tchunk(tlist(chunks.items[x]).items[y]);
      subchunk.savetofile(filename);
      line:=prefix+inttostr(x)+'_'+inttostr(y)+'.png';
      node:=subchunks.Add(line);
      node.Add('x',inttostr(subchunk.pOffset.x);
      node.Add('y',inttostr(subchunk.pOffset.y);
      Writeln('Saved subchunk "'+filename+'"');
    end;
  end;
end;

end.

