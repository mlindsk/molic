#ifndef OUTLIERUTILS_H
#define OUTLIERUTILS_H

#include "set_ops.h"
#include "misc_utils.h"

RIV    n_a(RCM & A);
RIV    n_b(RIV & na, RIV & b);
VD     subtract(VD x);
VD     xlogx(VD x);
VD     x1logx1(VD x);
RCM    subM( RCM & A, RCV & x );
RL     a_marginals( RCM A, RL & am );
double TY(RCV y, RL & C_marginals, RL & S_marginals);

#endif
