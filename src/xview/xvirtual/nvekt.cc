//  1993 by M. Roth

#include "szene.h"
#include "nvekt.h"

void NormVektor::Errechnen(Matrix &m)
{
  projektion = m * ursprung;
  helligkeit = projektion * SzeneGetLight();
  if (helligkeit<0)
    helligkeit=0;
}
