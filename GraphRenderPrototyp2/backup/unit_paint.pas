unit unit_paint;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unit_graph, FPImage, FPCanvas, FPImgCanv,FPWritePNG, graphics;


type

{ tchunk }

 tchunk=class //wrapper for the FPImage
 private
  offset:t2d;
  chunksize:integer;
  c:TFPCustomCanvas;
  image:TFPCustomImage;
  writer:TFPCustomImageWriter;
 public
  constructor create(_chunksize:integer;_offset:t2d);
  function savetofile(path:string):string; //returns filename
  procedure paintCircle(center:t2d;radius:integer;cost:byte); //the 2d coordinates refer to the global 2d Reference. So no offsetcalculation is needed
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
   procedure saveToFolder(path:string);
end;

implementation

uses unit_graphmanager;

{ tchunk }

constructor tchunk.create(_chunksize: integer; _offset: t2d);
begin
  chunksize:=_chunksize;
  offset:=_offset;
  image := TFPMemoryImage.Create (_chunksize,_chunksize);

  c := TFPImageCanvas.Create (image);
  with c do
  begin
    pen.mode    := pmCopy;
    pen.style   := psSolid;
    pen.width   := 1;
    pen.FPColor := TColorToFPColor(rgbtocolor(0,0,0));
  end;
  c.FillRect(0,0,c.Width,c.Height); //black out

  { Create the writer }
  Writer := TFPWriterPNG.Create;
end;

function tchunk.savetofile(path: string): string;
begin
 image.SaveToFile ('DrawTest.png', writer);
end;

procedure tchunk.paintCircle(center: t2d; radius: integer; cost: byte);
var i:integer;
    x,y:integer;
begin
  c.Pen.FPColor:=tcolortofpcolor(rgbtocolor(cost,cost,cost));
  //calc offset
  x:=center.x-offset.x;
  y:=center.y-offset.y;
  //fill circle
  for i:=1 to radius do c.Ellipse(center.x-i,center.y-i,center.x+i,center.y+i);
end;

destructor tchunk.free;
begin
  c.Free;
  image.Free;
  writer.Free;
end;

{ tmappainter }

procedure tmappainter.expandMap(cx, cy: integer);
var x,y,xmax,ymax:integer;
    ycol:tlist;
    chunk:tchunk;
    offset:t2d;

begin
  xmax:=chunks.Count-1;
  ymax:=tlist(chunks[0]).count-1;

  for x:=xmax to cx do begin
    ycol:=tlist.create;
    offset.x:=x*chunksize;
    for y:=ymax to cy do begin
        offset.y:=chunksize*y;
        chunk:=tchunk.create(chunksize,offset);
        ycol.Add(chunk);
    end;
    chunks.Add(ycol);
  end;
end;

function tmappainter.getChunk(x, y: integer): tchunk;
var xc,yc:integer;
begin
  xc:=x div chunksize;
  yc:=y div chunksize;
  //check if the target chunk is currently outside the map
  if (chunks.count<=x) or (tlist(chunks.items[0]).Count<=y) then self.expandMap(xc,yc);  //expand map to chunk
  result:=tchunk(tlist(chunks.items[x]).items[y]); //return chunk
end;

constructor tmappainter.create(_chunksize: integer);
var ycol:tlist;
    offset:t2d;
begin
  chunks:=tlist.create;
  ycol:=tlist.create;
  offset.x:=0;
  offset.y:=0;
  ycol.Add(tchunk.create(chunksize,offset));
  chunks.Add(ycol)
end;

procedure tmappainter.paintWay(w: tway);
var i:integer;
    dot1,dot2:t2d;
begin
  if w.Count>2 then begin
    dot2:=w.Nodes[0].p2d;
    for i:=1 to w.Count-1 do begin
      dot1:=dot2;
      dot2:=w.Nodes[i].p2d;
      self.paintLine(dot1,dot2,round(w.pWidth*(1/tgraph(w.pParent).pPixelwidth)),w.pCost);
    end;
  end;
end;

procedure tmappainter.paintLine(p1: t2d; p2: t2d; width: integer; cost: byte);
var m,b:double;
    x,y:integer;
    dot:t2d;
begin
  m:=(p2.y-p1.y)/(p2.x-p1.x);
  b:=(((-1)*(m))*p1.x)+p1.y;

  for x:=p1.x to p2.x do begin
    y:=round(m*x+b)
    dot.x:=x;
    dot.y:=y;
    self.getChunk(x,y).paintCircle(dot,round(width/2),cost);
  end;
end;

procedure tmappainter.saveToFolder(path: string);
var x,y:integer;
    filename:string;
begin
  for x:=0 to chunks.count-1 do begin
    for y:=0 to tlist(chunks.items[x]).count-1 do begin
      filename:=path+'chunk_'+inttostr(x)+'_'+inttostr(y)+'.png';
      tchunk(tlist(chunks.items[x]).items[y]).savetofile(filename);
    end;
  end;
end;

end.

