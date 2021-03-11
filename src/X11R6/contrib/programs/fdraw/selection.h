/*
 * selection.h - a list of selected manipulators
 */

#ifndef _selection_h
#define _selection_h

class ManipList;
class Manipulator;

class Selection {
public:
    Selection(long size = 10);
    virtual ~Selection();

    Selection* copy();

    long count() const;
    Manipulator* item(long index);

    void add(Manipulator*);
    void remove(Manipulator*);
    void remove_all();
    Boolean selected(Manipulator*);
private:
    ManipList* mlist_;
};

#endif
