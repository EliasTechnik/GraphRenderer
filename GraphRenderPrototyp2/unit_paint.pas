unit unit_paint;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unit_graph, FPImage, FPCanvas, FPImgCanv;


type tchunk=class //wrapper for the FPImage
 private
  offset:t2d;
  chunksize:integer
  c:TFPCustomCanvas;
  image : TFPCustomImage;
  writer : TFPCustomImageWriter;
 public
  constructor create(_chunksize:integer;_offset:t2d);
  function savetofile(path:string):string; //returns filename
  procedure paintCircle(center:t2d;radius:integer;cost:byte);


{ tmappainter }

 tmappainter=class
  private
   ichunkwidth:qword;
   ichunkheight:qword;
   chunks:tlist; //contais objects of type tlist
  public
   constructor create(chunkheight:qword;chunkwidth:qword);
   procedure paintWay(w:tway);
end;

implementation

{ tmappainter }

constructor tmappainter.create(chunkheight: qword; chunkwidth: qword);
begin

end;

procedure tmappainter.paintWay(w: tway);
begin

end;

end.

