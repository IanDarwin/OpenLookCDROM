# A D4 symmetric system (d4symm.p)

xsq = x x	# xsq and zsq are temporary functions
zsq = z z

x' = y        
y' = x (mu - (xsq+zsq)) + delta x zsq + 
     epsilon ((xsq+zsq) y + nu y + Axzw x (x y + z w) + Ayz2 y zsq
z' = w
w' = z (mu - (xsq+zsq)) + delta z xsq + 
     epsilon ((xsq+zsq) w + nu w + Axzw z (x y + z w) + Ayz2 w xsq

Energy = 0.5 (y y + w w - mu (xsq + zsq) + 0.5 (xsq + zsq) (xsq + zsq) - 
              delta xsq zsq)
AngMom = y z - x w

INITIAL mu 2 delta 0.95 epsilon 0 nu -3.52 Axzw 1 Ayz2 0
RANGE x -5 5 y -5 5 z -5 5 w -5 5
