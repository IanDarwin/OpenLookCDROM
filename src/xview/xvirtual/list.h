// Verwaltung einer Liste von Objekten  M. Roth 1993

#ifndef _List_h
#define _List_h 1

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif

template <class Typ> class List;
template <class Typ> class NodePtr;

// Objekt
template <class Typ>
class Node
{
 friend class List<Typ>;
 friend class NodePtr<Typ>;
 private:
  Node 			*next;
 public:
  Node()
  {
    next=0;
  }
};

// Pointerverwaltung fuer eine Objektliste
template <class Typ>
class NodePtr
{
 private:
  Node<Typ>		*node;
 public:
  NodePtr(Node<Typ> *n=0)
  {
    node=n;
  }
  Node<Typ> *operator++()
  {
    node=node->next;

    return node;
  }
  Node<Typ> *operator++(int)
  {
    Node<Typ> *n=node;
    node=node->next;
    return n;
  }
  Typ *operator->()
  {
    return (Typ *) node;
  }
  int operator==(NodePtr<Typ> n)
  {
    return node==n.node;
  }
  int operator!=(NodePtr<Typ> n)
  {
    return node!=n.node;
  }
  Typ & operator *()
    {
      return (Typ &)(*node);
    }
  operator Typ *()
    {
      return (Typ *)node;
    }
};

// Verankerung der Objekte
template <class Typ>
class List
{
 friend class NodePtr<Typ>;
 private:
  NodePtr<Typ>		first;
  NodePtr<Typ>		last;
 public:
  List()
  {
    first=0;last=0;
  }
  void operator+=(NodePtr<Typ> n)
  {
    n->next = 0;
    if (first==0) first=n;
    else last->next=n;
    last = n;
  }
  void operator-=(NodePtr<Typ>);
  NodePtr<Typ> operator[](int);
  int Count();
  int Number(NodePtr<Typ>);
  NodePtr<Typ> First() {return first;}
  NodePtr<Typ> Last()  {return last;}
  void Flush();
  friend ostream& operator<<(ostream&,List&);
};

template <class Typ>
void List<Typ>::operator-=(NodePtr<Typ> n)
{
  NodePtr<Typ> 		p;

  p=first;
  if (n==p)
  {
    first=n->next;
    p=0;
  }
  else
  {
    while(n!=p->next) ++p;
    p->next=n->next;
    n->next=0;
  }
  if (n->next==0) last=p;
}

// Rueckgabe des n-ten Elementes einer Liste
template <class Typ>
NodePtr<Typ> List<Typ>::operator[](int i)
{
  NodePtr<Typ>		p;

  p=first;
  while(i--) p++;
  return p;
}

// Rueckgabe der Anzahl der Elemente einer Liste
template <class Typ>
int List<Typ>::Count()
{
  NodePtr<Typ>		p;
  int			c=0;

  p=first;
  while(p)
  {
    c++;
    p++;
  }
  return c;
}

// Nummer eines Listeintrages ermitteln
template <class Typ>
int List<Typ>::Number(NodePtr<Typ> ps)
{
  int			c=0;
  NodePtr<Typ> 		p;

  p=first;
  while(p)
  {
    if(p==ps) break;
    c++;
    p++;
  }
  return c;
}

// Alle Elemente der Liste loeschen
template <class Typ>
void List<Typ>::Flush()
{
  NodePtr<Typ>		p;
  NodePtr<Typ>		del;

  while(p=first)
  {
    del=p;
    p++;
    delete (Typ *)(del);
    first=0;
    last=0;
  }
}

#endif
