/*
 * fdraw.h
 */

#ifndef _fdraw_h
#define _fdraw_h

#include <X11/Fresco/widgets.h>
#include <X11/Fresco/Impls/viewers.h>

class Command;
class FigViewer;
class Fresco;
class Tool;

class FDraw : public ViewerImpl {
public:
    FDraw(Fresco*);
    ~FDraw();

    virtual Boolean key_press(GlyphTraversalRef, EventRef);
private:
    FigViewer* viewer_;
private:
    Glyph_return interior();
    Glyph_return tools();
    Glyph_return commands();
   
    Button_return tool_button(GlyphRef, Tool*, TelltaleRef);
    Glyph_return command_button(const char*, Command*);
    void flip_to(long card);
private:
    GlyphRef deck_;
    Boolean editing_;
};


#endif
