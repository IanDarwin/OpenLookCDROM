#include "extensions_config.h"

EXTENSIONS += $(GRAPHEDDIR)/contrib/fileselector/fileselector.o 
EXTENSIONS += $(GRAPHEDDIR)/sgraph/Sgraph.o

#if defined(CHROBAK_PAYNE_DRAWING_ALGORITHM)
EXTENSIONS += $(GRAPHEDDIR)/contrib/ChrobakPayne/ChrobakPayne.o 
#endif

#if defined(PQ_PLANARITY_TEST_ALGORITHM)
EXTENSIONS += $(GRAPHEDDIR)/contrib/PQplanar/PQplanar.o 
#endif

#if defined(BENDS_DRAWING_ALGORITHM)
EXTENSIONS += $(GRAPHEDDIR)/contrib/bends/bends.o 
#endif

#if defined(OLD_CONNECTIVITY_ALGORITHMS)
EXTENSIONS += $(GRAPHEDDIR)/contrib/conn/conn.o 
#endif

#if defined(MAX_CLIQUE_ALGORITHM)
EXTENSIONS += $(GRAPHEDDIR)/contrib/maxclique/maxclique.o 
#endif

#if defined(RT_TREE_DRAWING_ALGORITHM)
EXTENSIONS += $(GRAPHEDDIR)/contrib/rtlayout/rtlayout.o 
#endif

#if defined(SPRING_EMBEDDER_DRAWING_ALGORITHM)
EXTENSIONS += $(GRAPHEDDIR)/contrib/springembedder/springembedder.o 
#endif

#if defined(SUGIYAMA_LAYOUT_ALGORITHM)
EXTENSIONS += $(GRAPHEDDIR)/contrib/sugiyama/sugiyama.o 
#endif

#if defined(TERMGRAPH)
EXTENSIONS += $(GRAPHEDDIR)/contrib/termgraph/termgraph.o 
#endif

#if defined(TREE_LAYOUT_ALGORITHM)
EXTENSIONS += $(GRAPHEDDIR)/contrib/tree/tree.o 
#endif

#if defined(CONNECTIVITY_TEST_ALGORITHMS)
EXTENSIONS += $(GRAPHEDDIR)/contrib/connect/connect.o 
#endif

#if defined(WOODS_DRAWING_ALGORITHM)
EXTENSIONS += $(GRAPHEDDIR)/contrib/woods/woods.o 
#endif

#if defined(SPRINGEMBEDDER_KAMADA)
EXTENSIONS += $(GRAPHEDDIR)/contrib/spring2/spring2.o 
#endif

#if defined(HT_PLANARITY_TEST_ALGORITHM) || defined(HT_DUALGRAPH_ALGORITHM)
EXTENSIONS += $(GRAPHEDDIR)/contrib/HTplanar/HTplanar.o
#endif

#if defined(DRAW_CONVEX_ALGORITHM)
EXTENSIONS += $(GRAPHEDDIR)/contrib/ConvexDraw/ConvexDraw.o
#endif
