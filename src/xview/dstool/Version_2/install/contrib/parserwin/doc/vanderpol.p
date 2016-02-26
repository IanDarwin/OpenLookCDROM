# The Van der Pol oscillator (vanderpol.p)
x' = y 
y' = alpha (1 - x^2) y - x + beta cos(omega t)
t' = 1

Rsqr = x x + y y     # square of polar radial coordinate

INITIAL alpha 1.0 beta 0 omega 1.0
RANGE x 3 -3 y 3 -3

