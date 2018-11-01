shape Rect:
  w  : R
  h  : R
  cx : R
  cy : R
  render:
    M {cx} {cy}
    m {-w / 2} {-h / 2}
    l {w} 0
    l  0 {h}
    l {-w} 0
    z
  handles:
    tl = (cx - w / 2 , cy - h / 2)
    tr = (cx + w / 2 , cy - h / 2)
    bl = (cx - w / 2 , cy + h / 2)
    br = (cx + w / 2 , cy + h / 2)
  move tl:
    lock br
  move tr:
    lock bl
  move bl:
    lock tr
  move br:
    lock tl

