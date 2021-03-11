/*
 * selection.c
 */

#include "globals.h"
#include "manipulators.h"
#include "selection.h"

Selection::Selection (long size) {
    mlist_ = new ManipList(size);
}

Selection::~Selection () {
    delete mlist_;
}

Selection* Selection::copy () {
    Selection* newcopy = new Selection(mlist_->count());
    for (long i = 0; i < mlist_->count(); i++) {
        newcopy->add(mlist_->item(i));
    }
    return newcopy;
}

long Selection::count () const {
    return mlist_->count();
}

Manipulator* Selection::item (long index) {
    return mlist_->item(index);
}

void Selection::add (Manipulator* m) {
    if (!selected(m)) {
        mlist_->append(m);
    }
}

void Selection::remove(Manipulator* m) {
    for (long i = 0; i < mlist_->count(); i++) {
        if (mlist_->item(i) == m) {
            mlist_->remove(i);
            break;
        }
    }
}

void Selection::remove_all () {
    mlist_->remove_all();
}

Boolean Selection::selected (Manipulator* m) {
    for (long i = 0; i < mlist_->count(); i++) {
        if (mlist_->item(i) == m) {
            return true;
        }
    }
    return false;
}
