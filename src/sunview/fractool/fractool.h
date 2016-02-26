#define PI              3.141593653589793
#define PI_OVER_2       1.570796326794896
#define TWO_PI          6.283185307179586
#define THREE_PI_OVER_2	4.712389980384689
#define N_SINES		1801

#define RUNNING         0
#define DEFINING_XFORM  1
#define DEFINING_INIT   2

#define NONE		0	/* For curvetype */
#define OPEN		1
#define CLOSED		2

#define GRIDSIZE	22	/* Base of grid */
#define GRIDHEIGHT	19	/* Height of triangular grid */
#define SQUARE		1	/* For grid_type */
#define TRIANGULAR	2



struct endpoint {
        float x, y;
        struct endpoint *next;
};

struct curve {
        struct endpoint *start;
        struct curve *next;
};

struct rule {
        float rho, theta;
        struct rule *next;
};

struct curve_rules {
        struct rule *start;
        struct curve_rules *next;
        short flags;
};
#define CONNECT_INITIAL         1
#define CONNECT_TERMINAL        2

struct rules_hdr {
        struct curve_rules *rules;
        long probability;               /* Composite! */
        struct rules_hdr *next;
};



