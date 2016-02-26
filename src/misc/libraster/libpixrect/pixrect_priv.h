#ifndef _PIXRECT_PRIV_H_
#define _PIXRECT_PRIV_H_

#define PROP_TO_RASOP(op) ((op) >> 1)

extern struct pixrect* raster_to_pixrect ARGS(( struct raster* r ));

#endif /*_PIXRECT_PRIV_H_*/
