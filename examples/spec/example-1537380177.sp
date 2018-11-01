shape Rect:
  w  : R
  h  : R
  render:
    m {-w / 2} {-h / 2}
    l {w} 0
    l  0 {h}
    l {-w} 0
    z
  origin (cx,cy)
  handles:
    tl = (cx - w / 2 , cy - h / 2)
    tr = (cx + w / 2 , cy - h / 2)
    bl = (cx - w / 2 , cy + h / 2)
    br = (cx + w / 2 , cy + h / 2)
  move tl (x,y):
    lock br
  move tr (x,y):
    lock bl
  move bl (x,y):
    lock tr
  move br (x,y):
    lock tl

shape Ellipse:
  rx : R
  ry : R
  render:
    m 0 {-ry}
    a {rx} {ry} 0 0 1  {rx}  {ry}
    a {rx} {ry} 0 0 1 {-rx}  {ry}
    a {rx} {ry} 0 0 1  {rx} {-ry}
    a {rx} {ry} 0 0 1 {-rx} {-ry}
    z
  origin (cx,cy)
  handles:
    t = (cx      , cy - ry)
    r = (cx + rx , cy)
    b = (cx      , cy + ry)
    l = (cx - rx , cy)
  move t (x,y):
    ry <- cy - y
    lock cx cy rx
  move r (x,y):
    rx <- x - cx
    lock cx cy ry
  move b (x,y):
    rx <- y - cy
    lock cx cy rx
  move l (x,y):
    rx <- cx - x
    lock cx cy ry

