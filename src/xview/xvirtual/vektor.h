// 1993 by M. Roth
//
// Rechnen mit Vektoren und Matrizen v[3] m[3][3]

#ifndef _Vektor_h
#define _Vektor_h 1

#ifndef __MATH_H
#include <math.h>
#endif
#ifndef __IOSTREAM_H
#include <iostream.h>
#endif

/*
----------------------------------------------------------

Verarbeitung von Vektoren mit 3 Ausprägungen

----------------------------------------------------------
*/

class Vektor
{
 public:
  float x,y,z;

  /* Konstruktor */
  Vektor (float i0=0, float i1=0, float i2=0)
  { x=i0; y=i1; z=i2; }

  /* Operatoren */

  /* Vektoraddition */
  Vektor operator+(Vektor a)
  { return Vektor(a.x+x, a.y+y, a.z+z ); }

  /* Vektorsubtraktion */
  Vektor operator-(Vektor a)
  { return Vektor( x-a.x, y-a.y, z-a.z ); }

  /* Vektormultiplikation mit einem Skalar */
  Vektor operator*(float a)
  { return Vektor( x*a, y*a, z*a ); }

  /* Vektordivision mit einem Skalar */
  Vektor operator/(float a)
  { return Vektor( x/a, y/a, z/a ); }

  /* Skalarprodukt */
  float operator*(Vektor a)
  { return (a.x*x + a.y*y + a.z*z); }

  /* Ausgabe */
  friend ostream& operator<<(ostream&,Vektor&);
};

/* Formatierte Ausgabe eines Vektors */
inline ostream& operator<<(ostream &o,Vektor& a)
{
  o << "(" << a.x << "," << a.y << "," << a.z << ")";
  return o;
}

/*
----------------------------------------------------------

Verarbeitung von Matrizen mit 3*3 Ausprägungen

----------------------------------------------------------
*/

class Matrix
{
 private:
  float i[3][3];
 public:
  /* Konstruktor */
  Matrix (){}
  Matrix (float i00,float i01,float i02,
	  float i10,float i11,float i12,
	  float i20,float i21,float i22)
  {
    i[0][0]=i00; i[0][1]=i01; i[0][2]=i02;
    i[1][0]=i10; i[1][1]=i11; i[1][2]=i12;
    i[2][0]=i20; i[2][1]=i21; i[2][2]=i22;
  }
  Matrix(float a,float b,float c)
  {
    a=a*M_PI/180;
    b=b*M_PI/180;
    c=c*M_PI/180;
    i[0][0] =  cos(b)  *  cos(c);
    i[0][1] = -cos(b)  *  sin(c);
    i[0][2] =  sin(b);
    i[1][0] =  cos(a)  *  sin(c)  +  sin(a)  *  sin(b)  *  cos(c);
    i[1][1] =  cos(a)  *  cos(c)  -  sin(a)  *  sin(b)  *  sin(c);
    i[1][2] = -sin(a)  *  cos(b);
    i[2][0] =  sin(a)  *  sin(c)  -  cos(a)  *  sin(b)  *  cos(c);
    i[2][1] =  sin(a)  *  cos(c)  +  cos(a)  *  sin(b)  *  sin(c);
    i[2][2] =  cos(a)  *  cos(b);
  }
  /* Operatoren */
  Matrix operator*(Matrix &m)
  {
   return Matrix (
    i[0][0]*m.i[0][0] + i[0][1]*m.i[1][0] + i[0][2]*m.i[2][0],
    i[0][0]*m.i[0][1] + i[0][1]*m.i[1][1] + i[0][2]*m.i[2][1],
    i[0][0]*m.i[0][2] + i[0][1]*m.i[1][2] + i[0][2]*m.i[2][2],
    i[1][0]*m.i[0][0] + i[1][1]*m.i[1][0] + i[1][2]*m.i[2][0],
    i[1][0]*m.i[0][1] + i[1][1]*m.i[1][1] + i[1][2]*m.i[2][1],
    i[1][0]*m.i[0][2] + i[1][1]*m.i[1][2] + i[1][2]*m.i[2][2],
    i[2][0]*m.i[0][0] + i[2][1]*m.i[1][0] + i[2][2]*m.i[2][0],
    i[2][0]*m.i[0][1] + i[2][1]*m.i[1][1] + i[2][2]*m.i[2][1],
    i[2][0]*m.i[0][2] + i[2][1]*m.i[1][2] + i[2][2]*m.i[2][2]);
  }
  /* Matrix-Vektor Produkt */
  Vektor operator*(Vektor &v)
  {
    return Vektor (
	i[0][0]*v.x + i[0][1]*v.y + i[0][2]*v.z,
	i[1][0]*v.x + i[1][1]*v.y + i[1][2]*v.z,
	i[2][0]*v.x + i[2][1]*v.y + i[2][2]*v.z);
  }
  /* Ausgabe */
  friend ostream& operator<<(ostream&,Matrix&);
};

/* Formatierte Ausgabe einer Matrix */
inline ostream& operator<<(ostream &o,Matrix& m)
{
  o << m.i[0][0] << "," << m.i[0][1] << "," << m.i[0][2] << "\n";
  o << m.i[1][0] << "," << m.i[1][1] << "," << m.i[1][2] << "\n";
  o << m.i[2][0] << "," << m.i[2][1] << "," << m.i[2][2] << "\n";
  return o;
}
#endif
