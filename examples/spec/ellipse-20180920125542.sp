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
  move t <- (x,y):
    ry <- cy - y
    lock cx cy rx
  move r <- (x,y):
    rx <- x - cx
    lock cx cy ry
  move b <- (x,y):
    rx <- y - cy
    lock cx cy rx
  move l <- (x,y):
    rx <- cx - x
    lock cx cy ry

